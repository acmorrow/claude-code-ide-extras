;;; claude-code-ide-extras-emacs.el --- Emacs introspection MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;; Keywords: tools, help, ai, claude, mcp
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

;; This package provides Emacs introspection MCP (Model Context Protocol)
;; tools for claude-code-ide.el, enabling Claude to:
;;
;; - Discover Emacs functions, variables, and commands
;; - Read documentation for Emacs symbols
;; - Search Emacs help system
;; - Learn about the user's Emacs environment
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-emacs)
;;   (claude-code-ide-extras-emacs-setup)

;;; Code:

(require 'claude-code-ide)
(require 'claude-code-ide-extras-common)

(defgroup claude-code-ide-extras-emacs nil
  "Emacs introspection MCP tools for claude-code-ide."
  :group 'help
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-emacs-")

(defconst claude-code-ide-extras-emacs-version "0.0.0"
  "Version of claude-code-ide-extras-emacs.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-emacs-describe-tool-name
  "claude-code-ide-extras-emacs/describe"
  "MCP tool name for describe.")

(defconst claude-code-ide-extras-emacs-apropos-tool-name
  "claude-code-ide-extras-emacs/apropos"
  "MCP tool name for apropos.")

(defconst claude-code-ide-extras-emacs-apropos-command-tool-name
  "claude-code-ide-extras-emacs/apropos_command"
  "MCP tool name for apropos_command.")

(defconst claude-code-ide-extras-emacs-apropos-documentation-tool-name
  "claude-code-ide-extras-emacs/apropos_documentation"
  "MCP tool name for apropos_documentation.")

(defconst claude-code-ide-extras-emacs-buffer-query-tool-name
  "claude-code-ide-extras-emacs/buffer_query"
  "MCP tool name for buffer_query.")

(defconst claude-code-ide-extras-emacs-buffer-search-tool-name
  "claude-code-ide-extras-emacs/buffer_search"
  "MCP tool name for buffer_search.")

(defconst claude-code-ide-extras-emacs-read-dir-locals-tool-name
  "claude-code-ide-extras-emacs/read_dir_locals"
  "MCP tool name for read_dir_locals.")

;;; Customization

(defcustom claude-code-ide-extras-emacs-describe-usage-prompt
  "Get symbol documentation. Try 'symbol' type for complete info on functions, variables, and modes."
  "Usage guidance for the describe MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-describe-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-describe-tool-name)

(defcustom claude-code-ide-extras-emacs-apropos-usage-prompt
  "Search for symbols by name pattern. Use for broad exploration of available functionality."
  "Usage guidance for the apropos MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-apropos-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-apropos-tool-name)

(defcustom claude-code-ide-extras-emacs-apropos-command-usage-prompt
  "Find interactive commands only. More focused than apropos for discovering M-x commands."
  "Usage guidance for the apropos_command MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-apropos-command-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-apropos-command-tool-name)

(defcustom claude-code-ide-extras-emacs-apropos-documentation-usage-prompt
  "Search documentation by concept, not just symbol names. Useful for discovering related functionality."
  "Usage guidance for the apropos_documentation MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-apropos-documentation-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-apropos-documentation-tool-name)

(defcustom claude-code-ide-extras-emacs-buffer-query-usage-prompt
  "Read any Emacs buffer contents. Useful for *compilation*, *scratch*, *Messages*, etc."
  "Usage guidance for the buffer_query MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-buffer-query-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-buffer-query-tool-name)

(defcustom claude-code-ide-extras-emacs-buffer-search-usage-prompt
  "Search any buffer with regex. Complement to buffer_query for finding specific content."
  "Usage guidance for the buffer_search MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-buffer-search-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-buffer-search-tool-name)

(defcustom claude-code-ide-extras-emacs-read-dir-locals-usage-prompt
  "Read file-local configuration variables for a specific file."
  "Usage guidance for the read_dir_locals MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-read-dir-locals-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-read-dir-locals-tool-name)

;;; Tool implementations

  ;; Custom MCP tools for Emacs introspection
  (defun claude-code-ide-extras-emacs--describe (name type)
    "Describe an Emacs symbol/mode/package.
NAME is the symbol name as a string.
TYPE is one of: function, variable, mode, package, symbol."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((symbol (intern name))
                 (temp-buf (generate-new-buffer " *temp-help*")))
            (unwind-protect
                (save-window-excursion
                  (cl-letf (((symbol-function 'help-buffer)
                             (lambda () temp-buf)))
                    (pcase type
                      ("function" (describe-function symbol))
                      ("variable" (describe-variable symbol))
                      ("mode" (describe-function symbol))
                      ("package" (describe-package symbol))
                      ("symbol" (describe-symbol symbol))
                      (_ (error "Unknown type '%s'. Must be one of: function, variable, mode, package, symbol" type))))
                  (with-current-buffer temp-buf
                    (buffer-string)))
              (when (buffer-live-p temp-buf)
                (kill-buffer temp-buf))))
        (error (format "Error describing %s: %s" name (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--apropos (pattern)
    "Search for all Emacs symbols matching PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (let ((show-all t))
              (apropos pattern show-all)
              ;; Capture content from *Apropos* buffer
              (with-current-buffer "*Apropos*"
                (prog1 (buffer-string)
                  (kill-buffer)))))
        (error (format "Error running apropos: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--apropos-command (pattern)
    "Search for interactive Emacs commands matching PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (apropos-command pattern t)
            ;; Capture content from *Apropos* buffer
            (with-current-buffer "*Apropos*"
              (prog1 (buffer-string)
                (kill-buffer))))
        (error (format "Error running apropos-command: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--apropos-documentation (pattern)
    "Search Emacs documentation for PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (apropos-documentation pattern)
            ;; Capture content from *Apropos* buffer
            (with-current-buffer "*Apropos*"
              (prog1 (buffer-string)
                (kill-buffer))))
        (error (format "Error running apropos-documentation: %s" (error-message-string err))))))

  ;; Buffer access tools
  (defun claude-code-ide-extras-emacs--buffer-query (buffer-name &optional start-line num-lines)
    "Query buffer contents by line range.
BUFFER-NAME is the name of the buffer to query.
Optional START-LINE is the first line to retrieve (1-based, negative
counts from end).
Optional NUM-LINES is the number of lines to retrieve.
Both must be provided together or both omitted."
    (claude-code-ide-mcp-server-with-session-context nil
      (claude-code-ide-extras-common--buffer-query buffer-name start-line num-lines)))

  (defun claude-code-ide-extras-emacs--buffer-search (buffer-name pattern &optional context-lines)
    "Search buffer contents for pattern.
BUFFER-NAME is the name of the buffer to search.
PATTERN is a regular expression to search for.
Optional CONTEXT-LINES specifies lines of context before/after each match."
    (claude-code-ide-mcp-server-with-session-context nil
      (claude-code-ide-extras-common--buffer-search buffer-name pattern context-lines)))

  (defun claude-code-ide-extras-emacs--read-dir-locals (file-path)
    "Read effective dir-local variables for FILE-PATH.
Opens FILE-PATH and returns buffer-local-variables as a Lisp form."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((buffer (find-file-noselect file-path)))
            (unwind-protect
                (with-current-buffer buffer
                  (format "%S" (buffer-local-variables)))
              (kill-buffer buffer)))
        (error (format "Error reading dir-locals for %s: %s"
                      file-path
                      (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-emacs-setup ()
  "Register all Emacs introspection MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--describe
   :name claude-code-ide-extras-emacs-describe-tool-name
   :description "Get documentation for an Emacs symbol. Returns docstring, current value (for variables), arguments (for functions), and other metadata from the running Emacs session."
   :args '((:name "name"
            :type string
            :description "The name of the symbol to describe (e.g., 'projectile-compile-project', 'lsp-mode').")
           (:name "type"
            :type string
            :description "The type of thing to describe: function, variable, mode, package, or symbol. Use 'symbol' for a unified view of all aspects.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--apropos
   :name claude-code-ide-extras-emacs-apropos-tool-name
   :description "Search for all Emacs symbols (functions, variables, faces, etc.) matching a pattern. Use for broad exploration."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match symbol names.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--apropos-command
   :name claude-code-ide-extras-emacs-apropos-command-tool-name
   :description "Search for interactive Emacs commands (callable via M-x) matching a pattern. More focused than emacs_apropos - only returns commands users can invoke."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match command names.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--apropos-documentation
   :name claude-code-ide-extras-emacs-apropos-documentation-tool-name
   :description "Search Emacs documentation text for a pattern. Finds functions/variables whose docstrings contain the pattern. Use for concept-based search (e.g., 'buffer naming', 'code formatting')."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match in documentation text.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--buffer-query
   :name claude-code-ide-extras-emacs-buffer-query-tool-name
   :description "Read contents from any Emacs buffer by line range. Lines are 1-based (line 1 is first line). Negative start_line counts from end (-100 = 100th line from end). Lines longer than the configured maximum are truncated. Use for reading compilation output, scratch buffers, message logs, or any other buffer contents. Both start_line and num_lines must be provided together or both omitted."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer to read (e.g., '*scratch*', '*Messages*', '*compilation*').")
           (:name "start_line"
            :type number
            :description "First line to read (1-based, negative counts from end). Must be provided with num_lines."
            :optional t)
           (:name "num_lines"
            :type number
            :description "Number of lines to read starting from start_line. Must be provided with start_line."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--buffer-search
   :name claude-code-ide-extras-emacs-buffer-search-tool-name
   :description "Search any Emacs buffer for a pattern using regular expressions. Returns matching lines with optional context. Use for finding specific content in compilation output, logs, scratch buffers, or any other buffer."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer to search (e.g., '*scratch*', '*Messages*', '*compilation*').")
           (:name "pattern"
            :type string
            :description "Regular expression pattern to search for.")
           (:name "context_lines"
            :type number
            :description "Number of context lines before and after each match (optional, default 0)."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--read-dir-locals
   :name claude-code-ide-extras-emacs-read-dir-locals-tool-name
   :description "Read buffer-local variables for a specific file path. Opens the file and returns buffer-local-variables as a Lisp form."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file or directory to read buffer-local variables for.")))

  (message "Claude Code IDE Extras: Emacs tools registered"))

(provide 'claude-code-ide-extras-emacs)
;;; claude-code-ide-extras-emacs.el ends here
