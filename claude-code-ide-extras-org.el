;;; claude-code-ide-extras-org.el --- Org-babel and export MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;;         Tim Ransom
;; Keywords: tools, org, babel, ai, claude, mcp
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Org-babel and Org-export MCP (Model Context Protocol)
;; tools for claude-code-ide.el, enabling Claude to:
;;
;; - List all source blocks in an org file
;; - Execute named source blocks or entire buffers
;; - Read existing results without re-executing
;; - Tangle org files or individual blocks
;; - Export org files to PDF, HTML, LaTeX, Markdown, and Jupyter notebooks
;; - Inspect export settings and keywords
;;
;; Designed for data science workflows involving course materials,
;; Jupyter notebook export, and PDF generation.
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-org)
;;   (claude-code-ide-extras-org-setup)

;;; Code:

(require 'claude-code-ide)
(require 'org)
(require 'ob-core)
(require 'org-element)

;; Declare external functions to avoid byte-compiler warnings
(declare-function org-latex-export-to-pdf "ox-latex" (&optional async subtreep visible-only body-only ext-plist))
(declare-function org-html-export-to-html "ox-html" (&optional async subtreep visible-only body-only ext-plist))
(declare-function org-latex-export-to-latex "ox-latex" (&optional async subtreep visible-only body-only ext-plist))
(declare-function org-md-export-to-markdown "ox-md" (&optional async subtreep visible-only body-only ext-plist))
(declare-function org-jupyter-export-to-ipynb "ox-jupyter" (&optional async subtreep visible-only body-only ext-plist))

(defgroup claude-code-ide-extras-org nil
  "Org-babel and export MCP tools for claude-code-ide."
  :group 'org
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-org-")

(defconst claude-code-ide-extras-org-version "0.0.4"
  "Version of claude-code-ide-extras-org.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-org-list-babel-blocks-tool-name
  "claude-code-ide-extras-org/list_babel_blocks"
  "MCP tool name for list_babel_blocks.")

(defconst claude-code-ide-extras-org-babel-execute-tool-name
  "claude-code-ide-extras-org/babel_execute"
  "MCP tool name for babel_execute.")

(defconst claude-code-ide-extras-org-babel-execute-buffer-tool-name
  "claude-code-ide-extras-org/babel_execute_buffer"
  "MCP tool name for babel_execute_buffer.")

(defconst claude-code-ide-extras-org-get-babel-result-tool-name
  "claude-code-ide-extras-org/get_babel_result"
  "MCP tool name for get_babel_result.")

(defconst claude-code-ide-extras-org-babel-tangle-tool-name
  "claude-code-ide-extras-org/babel_tangle"
  "MCP tool name for babel_tangle.")

(defconst claude-code-ide-extras-org-org-export-tool-name
  "claude-code-ide-extras-org/org_export"
  "MCP tool name for org_export.")

(defconst claude-code-ide-extras-org-get-export-settings-tool-name
  "claude-code-ide-extras-org/get_export_settings"
  "MCP tool name for get_export_settings.")

;;; Customization

(defcustom claude-code-ide-extras-org-list-babel-blocks-usage-prompt
  "List all source blocks in an org file with their names, languages, line numbers, and header arguments. Useful for understanding the structure of a literate programming document before executing or tangling."
  "Usage guidance for the list_babel_blocks MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-org)

(put 'claude-code-ide-extras-org-list-babel-blocks-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-org-list-babel-blocks-tool-name)

(defcustom claude-code-ide-extras-org-babel-execute-usage-prompt
  "Execute a single named source block in an org file and return its results. The block must have a #+NAME: identifier. Results are returned as text."
  "Usage guidance for the babel_execute MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-org)

(put 'claude-code-ide-extras-org-babel-execute-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-org-babel-execute-tool-name)

(defcustom claude-code-ide-extras-org-babel-execute-buffer-usage-prompt
  "Execute all source blocks in an org file in sequence. Returns a summary of how many blocks were executed. Useful for running an entire notebook-style document."
  "Usage guidance for the babel_execute_buffer MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-org)

(put 'claude-code-ide-extras-org-babel-execute-buffer-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-org-babel-execute-buffer-tool-name)

(defcustom claude-code-ide-extras-org-get-babel-result-usage-prompt
  "Read the existing result of a named source block without re-executing it. Returns the #+RESULTS block content, or a message if no results are found."
  "Usage guidance for the get_babel_result MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-org)

(put 'claude-code-ide-extras-org-get-babel-result-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-org-get-babel-result-tool-name)

(defcustom claude-code-ide-extras-org-babel-tangle-usage-prompt
  "Tangle an org file to extract source code into output files. Can tangle the entire file or a single named block. Returns the list of output files created."
  "Usage guidance for the babel_tangle MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-org)

(put 'claude-code-ide-extras-org-babel-tangle-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-org-babel-tangle-tool-name)

(defcustom claude-code-ide-extras-org-org-export-usage-prompt
  "Export an org file to a specified format. Supported backends: pdf, html, latex, md, ipynb. For ipynb export, ox-jupyter or ox-ipynb must be installed. Returns the output file path."
  "Usage guidance for the org_export MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-org)

(put 'claude-code-ide-extras-org-org-export-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-org-org-export-tool-name)

(defcustom claude-code-ide-extras-org-get-export-settings-usage-prompt
  "Read all export-relevant keywords from an org file (#+TITLE, #+AUTHOR, #+OPTIONS, #+PROPERTY, etc.). Useful for understanding how a document is configured before exporting."
  "Usage guidance for the get_export_settings MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-org)

(put 'claude-code-ide-extras-org-get-export-settings-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-org-get-export-settings-tool-name)

;;; Internal helpers

(defun claude-code-ide-extras-org--open-file (file-path)
  "Open FILE-PATH in a buffer without displaying it, returning the buffer.
Signals an error if the file does not exist or is not readable."
  (let ((path (expand-file-name file-path)))
    (unless (file-exists-p path)
      (error "File does not exist: %s" path))
    (unless (file-readable-p path)
      (error "File is not readable: %s" path))
    (find-file-noselect path)))

(defun claude-code-ide-extras-org--ensure-org-mode (buf)
  "Ensure BUF is in `org-mode'.  Switch if necessary."
  (with-current-buffer buf
    (unless (derived-mode-p 'org-mode)
      (org-mode))))

(defun claude-code-ide-extras-org--summarize-header-args (params)
  "Summarize PARAMS (header argument string or alist) into a short string."
  (cond
   ((null params) "")
   ((stringp params) (string-trim params))
   ((listp params)
    (mapconcat
     (lambda (pair)
       (if (consp pair)
           (format "%s %s" (car pair) (cdr pair))
         (format "%s" pair)))
     params " "))
   (t (format "%s" params))))

;;; Tool implementations

(defun claude-code-ide-extras-org--list-babel-blocks (file-path)
  "List all source blocks in FILE-PATH with names, languages, and line numbers."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((buf (claude-code-ide-extras-org--open-file file-path))
               (blocks '()))
          (claude-code-ide-extras-org--ensure-org-mode buf)
          (with-current-buffer buf
            (org-with-wide-buffer
             (org-element-map (org-element-parse-buffer) 'src-block
               (lambda (src)
                 (let* ((lang (org-element-property :language src))
                        (name (org-element-property :name src))
                        (begin (org-element-property :begin src))
                        (params (org-element-property :parameters src))
                        (line (line-number-at-pos begin)))
                   (push (list line lang name params) blocks))))))
          (setq blocks (nreverse blocks))
          (if (null blocks)
              (format "No source blocks found in %s" file-path)
            (concat
             (format "Source blocks in %s (%d total):\n\n"
                     (file-name-nondirectory file-path)
                     (length blocks))
             (format "%-6s  %-12s  %-30s  %s\n" "Line" "Language" "Name" "Header Args")
             (make-string 80 ?-)
             "\n"
             (mapconcat
              (lambda (block)
                (let ((line (nth 0 block))
                      (lang (nth 1 block))
                      (name (nth 2 block))
                      (params (nth 3 block)))
                  (format "%-6d  %-12s  %-30s  %s"
                          line
                          (or lang "?")
                          (or name "(unnamed)")
                          (claude-code-ide-extras-org--summarize-header-args params))))
              blocks "\n"))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-org--babel-execute (file-path block-name)
  "Execute the named source block BLOCK-NAME in FILE-PATH and return results.
BLOCK-NAME must match a #+NAME: identifier in the file."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buf (claude-code-ide-extras-org--open-file file-path)))
          (claude-code-ide-extras-org--ensure-org-mode buf)
          (with-current-buffer buf
            (org-with-wide-buffer
             (let ((pos (org-babel-find-named-block block-name)))
               (unless pos
                 (error "Named block not found: %s" block-name))
               (goto-char pos)
               (let ((result (org-babel-execute-src-block)))
                 (if result
                     (format "Result of block '%s':\n%s" block-name
                             (if (stringp result)
                                 result
                               (format "%S" result)))
                   (format "Block '%s' executed successfully (no return value)." block-name)))))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-org--babel-execute-buffer (file-path)
  "Execute all source blocks in FILE-PATH in sequence.
Returns a summary of the execution."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buf (claude-code-ide-extras-org--open-file file-path)))
          (claude-code-ide-extras-org--ensure-org-mode buf)
          (with-current-buffer buf
            (org-with-wide-buffer
             ;; Count blocks before executing
             (let ((block-count 0))
               (org-element-map (org-element-parse-buffer) 'src-block
                 (lambda (_src)
                   (setq block-count (1+ block-count))))
               (if (= block-count 0)
                   (format "No source blocks found in %s" file-path)
                 (org-babel-execute-buffer)
                 (format "Successfully executed all %d source block(s) in %s."
                         block-count
                         (file-name-nondirectory file-path)))))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-org--get-babel-result (file-path block-name)
  "Read the existing result of BLOCK-NAME in FILE-PATH without re-executing.
BLOCK-NAME must match a #+NAME: identifier in the file."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buf (claude-code-ide-extras-org--open-file file-path)))
          (claude-code-ide-extras-org--ensure-org-mode buf)
          (with-current-buffer buf
            (org-with-wide-buffer
             (let ((pos (org-babel-find-named-block block-name)))
               (unless pos
                 (error "Named block not found: %s" block-name))
               (goto-char pos)
               (let ((result-pos (org-babel-where-is-src-block-result)))
                 (if (not result-pos)
                     (format "No results found for block '%s'. Execute the block first." block-name)
                   (goto-char result-pos)
                   ;; Skip the #+RESULTS: line
                   (forward-line 1)
                   (let ((result-start (point))
                         (result-text ""))
                     ;; Collect result lines until we hit a non-result line
                     (while (and (not (eobp))
                                 (looking-at "^\\(:\\|#\\+\\(begin\\|end\\)_\\| \\|$\\)"))
                       (forward-line 1))
                     (setq result-text (string-trim
                                        (buffer-substring-no-properties result-start (point))))
                     (if (string-empty-p result-text)
                         (format "Results block for '%s' is empty." block-name)
                       (format "Results of block '%s':\n%s" block-name result-text)))))))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-org--babel-tangle (file-path &optional block-name)
  "Tangle FILE-PATH to extract source code into output files.
If BLOCK-NAME is given, tangle only that named block.
Returns the list of output files created."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((path (expand-file-name file-path)))
          (unless (file-exists-p path)
            (error "File does not exist: %s" path))
          (if (and block-name (not (string-empty-p block-name)))
              ;; Tangle a single named block
              (let ((buf (claude-code-ide-extras-org--open-file path)))
                (claude-code-ide-extras-org--ensure-org-mode buf)
                (with-current-buffer buf
                  (org-with-wide-buffer
                   (let ((pos (org-babel-find-named-block block-name)))
                     (unless pos
                       (error "Named block not found: %s" block-name))
                     (goto-char pos)
                     (let ((tangled-files (org-babel-tangle '(4))))
                       (if (and tangled-files (listp tangled-files) (> (length tangled-files) 0))
                           (concat
                            (format "Tangled block '%s' from %s:\n\n"
                                    block-name (file-name-nondirectory path))
                            (mapconcat (lambda (f) (format "  %s" f)) tangled-files "\n"))
                         (format "Block '%s' tangled but no output files were reported." block-name)))))))
            ;; Tangle entire file
            (let ((tangled-files (org-babel-tangle-file path)))
              (if (and tangled-files (listp tangled-files) (> (length tangled-files) 0))
                  (concat
                   (format "Tangled %s (%d file(s)):\n\n"
                           (file-name-nondirectory path) (length tangled-files))
                   (mapconcat (lambda (f) (format "  %s" f)) tangled-files "\n"))
                (format "No tangle targets found in %s. Ensure blocks have :tangle header args."
                        (file-name-nondirectory path))))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-org--org-export (file-path backend &optional async)
  "Export FILE-PATH using the specified BACKEND.
BACKEND is a string: \"pdf\", \"html\", \"latex\", \"md\", or \"ipynb\".
When ASYNC is \"true\", export asynchronously."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buf (claude-code-ide-extras-org--open-file file-path))
              (async-p (and async (string= async "true"))))
          (claude-code-ide-extras-org--ensure-org-mode buf)
          (with-current-buffer buf
            (let ((output-file
                   (pcase (downcase backend)
                     ("pdf"
                      (require 'ox-latex)
                      (org-latex-export-to-pdf async-p))
                     ("html"
                      (require 'ox-html)
                      (org-html-export-to-html async-p))
                     ("latex"
                      (require 'ox-latex)
                      (org-latex-export-to-latex async-p))
                     ("md"
                      (require 'ox-md)
                      (org-md-export-to-markdown async-p))
                     ("ipynb"
                      (cond
                       ((require 'ox-jupyter nil t)
                        (org-jupyter-export-to-ipynb async-p))
                       ((require 'ox-ipynb nil t)
                        ;; ox-ipynb may use a different export function name
                        (if (fboundp 'org-ipynb-export-to-ipynb)
                            (funcall #'org-ipynb-export-to-ipynb async-p)
                          (error "ox-ipynb loaded but export function not found. Check package documentation")))
                       (t
                        (error "No Jupyter notebook export backend available. Install ox-jupyter or ox-ipynb"))))
                     (_
                      (error "Unsupported export backend: %s. Supported: pdf, html, latex, md, ipynb" backend)))))
              (if async-p
                  (format "Async export to %s started for %s. Output will be generated in the background."
                          backend (file-name-nondirectory file-path))
                (if output-file
                    (format "Exported %s to %s:\n  %s"
                            (file-name-nondirectory file-path)
                            backend
                            (expand-file-name output-file))
                  (format "Export to %s completed but no output file path was returned." backend))))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-org--get-export-settings (file-path)
  "Read all export-relevant keywords from FILE-PATH.
Returns #+TITLE, #+AUTHOR, #+OPTIONS, #+PROPERTY, and other keyword lines."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buf (claude-code-ide-extras-org--open-file file-path))
              (settings '()))
          (claude-code-ide-extras-org--ensure-org-mode buf)
          (with-current-buffer buf
            (org-with-wide-buffer
             (org-element-map (org-element-parse-buffer 'greater-element) 'keyword
               (lambda (kw)
                 (let ((key (org-element-property :key kw))
                       (value (org-element-property :value kw)))
                   (push (cons key value) settings))))))
          (setq settings (nreverse settings))
          (if (null settings)
              (format "No export keywords found in %s" (file-name-nondirectory file-path))
            (concat
             (format "Export settings in %s (%d keywords):\n\n"
                     (file-name-nondirectory file-path)
                     (length settings))
             (mapconcat
              (lambda (pair)
                (format "  #+%-20s %s" (concat (car pair) ":") (cdr pair)))
              settings "\n"))))
      (error (format "Error: %s" (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-org-setup ()
  "Register all Org-babel and export MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-org--list-babel-blocks
   :name claude-code-ide-extras-org-list-babel-blocks-tool-name
   :description "List all source blocks in an org file with their names, languages, line numbers, and header arguments. Useful for understanding the structure of a literate programming document."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the org file to inspect.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-org--babel-execute
   :name claude-code-ide-extras-org-babel-execute-tool-name
   :description "Execute a single named source block in an org file and return its results. The block must have a #+NAME: identifier."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the org file containing the block.")
           (:name "block_name"
            :type string
            :description "The #+NAME: identifier of the source block to execute.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-org--babel-execute-buffer
   :name claude-code-ide-extras-org-babel-execute-buffer-tool-name
   :description "Execute all source blocks in an org file in sequence. Returns a summary of how many blocks were executed. Useful for running an entire notebook-style document."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the org file to execute.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-org--get-babel-result
   :name claude-code-ide-extras-org-get-babel-result-tool-name
   :description "Read the existing result of a named source block without re-executing it. Returns the #+RESULTS block content, or a message if no results are found."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the org file containing the block.")
           (:name "block_name"
            :type string
            :description "The #+NAME: identifier of the source block whose result to read.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-org--babel-tangle
   :name claude-code-ide-extras-org-babel-tangle-tool-name
   :description "Tangle an org file to extract source code into output files. Can tangle the entire file or a single named block. Returns the list of output files created."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the org file to tangle.")
           (:name "block_name"
            :type string
            :description "Optional #+NAME: identifier to tangle only that block. If omitted, tangles the entire file."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-org--org-export
   :name claude-code-ide-extras-org-org-export-tool-name
   :description "Export an org file to a specified format. Supported backends: pdf, html, latex, md, ipynb. For ipynb, ox-jupyter or ox-ipynb must be installed."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the org file to export.")
           (:name "backend"
            :type string
            :description "Export backend: \"pdf\", \"html\", \"latex\", \"md\", or \"ipynb\".")
           (:name "async"
            :type string
            :description "Set to \"true\" for asynchronous export. Default is synchronous."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-org--get-export-settings
   :name claude-code-ide-extras-org-get-export-settings-tool-name
   :description "Read all export-relevant keywords from an org file (#+TITLE, #+AUTHOR, #+OPTIONS, #+PROPERTY, etc.). Useful for understanding document configuration before exporting."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the org file to inspect.")))

  (message "Claude Code IDE Extras: org-babel and export tools registered"))

(provide 'claude-code-ide-extras-org)
;;; claude-code-ide-extras-org.el ends here
