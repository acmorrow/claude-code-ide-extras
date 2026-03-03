;;; claude-code-ide-extras-eglot.el --- Eglot MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;;         Tim Ransom
;; Keywords: tools, eglot, ai, claude, mcp
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

;; This package provides Eglot-specific MCP (Model Context Protocol) tools
;; for claude-code-ide.el, enabling Claude to:
;;
;; - Format buffers using Eglot (LSP)
;; - Get hover information (type signatures, documentation)
;; - List and execute LSP code actions at a position
;; - Search symbols across the workspace
;; - Get structured file outlines (document symbols)
;; - Perform semantic renames across the project
;;
;; This is the built-in alternative to claude-code-ide-extras-lsp (which
;; requires lsp-mode).  Eglot is included with Emacs 29+.
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-eglot)
;;   (claude-code-ide-extras-eglot-setup)

;;; Code:

(require 'eglot nil t)
(require 'claude-code-ide)
(require 'claude-code-ide-extras-common)

(defgroup claude-code-ide-extras-eglot nil
  "Eglot MCP tools for claude-code-ide."
  :group 'eglot
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-eglot-")

(defconst claude-code-ide-extras-eglot-version "0.0.3"
  "Version of claude-code-ide-extras-eglot.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-eglot-format-buffer-tool-name
  "claude-code-ide-extras-eglot/format_buffer"
  "MCP tool name for format_buffer.")

(defconst claude-code-ide-extras-eglot-describe-thing-at-point-tool-name
  "claude-code-ide-extras-eglot/describe_thing_at_point"
  "MCP tool name for describe_thing_at_point.")

(defconst claude-code-ide-extras-eglot-code-actions-tool-name
  "claude-code-ide-extras-eglot/code_actions"
  "MCP tool name for code_actions.")

(defconst claude-code-ide-extras-eglot-workspace-symbols-tool-name
  "claude-code-ide-extras-eglot/workspace_symbols"
  "MCP tool name for workspace_symbols.")

(defconst claude-code-ide-extras-eglot-document-symbols-tool-name
  "claude-code-ide-extras-eglot/document_symbols"
  "MCP tool name for document_symbols.")

(defconst claude-code-ide-extras-eglot-rename-tool-name
  "claude-code-ide-extras-eglot/rename"
  "MCP tool name for rename.")

;;; Customization

(defcustom claude-code-ide-extras-eglot-format-buffer-usage-prompt
  "Format code using Eglot language server. Respects project formatting configuration."
  "Usage guidance for the format_buffer MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-eglot)

(put 'claude-code-ide-extras-eglot-format-buffer-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-eglot-format-buffer-tool-name)

(defcustom claude-code-ide-extras-eglot-describe-thing-at-point-usage-prompt
  "Get type information and documentation from Eglot. Requires an Eglot LSP server running for the file."
  "Usage guidance for the describe_thing_at_point MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-eglot)

(put 'claude-code-ide-extras-eglot-describe-thing-at-point-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-eglot-describe-thing-at-point-tool-name)

(defcustom claude-code-ide-extras-eglot-code-actions-usage-prompt
  "List or execute LSP code actions at a position. Without action_title, lists available actions. With action_title, executes the matching action."
  "Usage guidance for the code_actions MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-eglot)

(put 'claude-code-ide-extras-eglot-code-actions-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-eglot-code-actions-tool-name)

(defcustom claude-code-ide-extras-eglot-workspace-symbols-usage-prompt
  "Search for symbols across the workspace using Eglot. Returns matching symbol names, kinds, locations, and containers."
  "Usage guidance for the workspace_symbols MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-eglot)

(put 'claude-code-ide-extras-eglot-workspace-symbols-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-eglot-workspace-symbols-tool-name)

(defcustom claude-code-ide-extras-eglot-document-symbols-usage-prompt
  "Get a structured outline of symbols in a file using Eglot. Returns symbol names, kinds, and line ranges."
  "Usage guidance for the document_symbols MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-eglot)

(put 'claude-code-ide-extras-eglot-document-symbols-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-eglot-document-symbols-tool-name)

(defcustom claude-code-ide-extras-eglot-rename-usage-prompt
  "Rename a symbol across the project using Eglot LSP. Applies changes to all references and saves affected buffers."
  "Usage guidance for the rename MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-eglot)

(put 'claude-code-ide-extras-eglot-rename-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-eglot-rename-tool-name)

;;; Internal helpers

(defun claude-code-ide-extras-eglot--prepare-buffer-for-file (file-path)
  "Get or create buffer for FILE-PATH with Eglot initialized if needed.
If the buffer does not yet have an active Eglot session, calls
`eglot-ensure' to start one.  This enables MCP tools to access semantic
information without requiring the buffer to be displayed.

Returns the buffer, which may be newly created or pre-existing."
  (let* ((existing (get-file-buffer file-path))
         (buffer (or existing (find-file-noselect file-path))))
    (with-current-buffer buffer
      (unless (bound-and-true-p eglot--managed-mode)
        (eglot-ensure)))
    buffer))

(defun claude-code-ide-extras-eglot--symbol-kind-name (kind)
  "Convert LSP SymbolKind integer KIND to a human-readable string."
  (alist-get kind eglot--symbol-kind-names "Unknown"))

(defun claude-code-ide-extras-eglot--format-lsp-location (location)
  "Format an LSP Location plist LOCATION as a \"file:line\" string."
  (let* ((uri (plist-get location :uri))
         (range (plist-get location :range))
         (file (eglot-uri-to-path uri))
         (line (if range
                   (1+ (plist-get (plist-get range :start) :line))
                 1)))
    (format "%s:%d" file line)))

(defun claude-code-ide-extras-eglot--format-symbol-information (symbols)
  "Format a vector of flat SymbolInformation SYMBOLS as a readable string."
  (mapconcat
   (lambda (sym)
     (let* ((name (plist-get sym :name))
            (kind (plist-get sym :kind))
            (location (plist-get sym :location))
            (container (plist-get sym :containerName))
            (kind-name (claude-code-ide-extras-eglot--symbol-kind-name kind))
            (loc-str (claude-code-ide-extras-eglot--format-lsp-location location)))
       (if (and container (not (string-empty-p container)))
           (format "[%s] %s  %s  (in %s)" kind-name name loc-str container)
         (format "[%s] %s  %s" kind-name name loc-str))))
   (append symbols nil)
   "\n"))

(defun claude-code-ide-extras-eglot--format-document-symbols (symbols &optional depth)
  "Recursively format a vector of hierarchical DocumentSymbol SYMBOLS.
DEPTH controls indentation level (default 0)."
  (let ((depth (or depth 0))
        (indent (make-string (* (or depth 0) 2) ?\s)))
    (mapconcat
     (lambda (sym)
       (let* ((name (plist-get sym :name))
              (kind (plist-get sym :kind))
              (range (plist-get sym :range))
              (children (plist-get sym :children))
              (kind-name (claude-code-ide-extras-eglot--symbol-kind-name kind))
              (start-line (1+ (plist-get (plist-get range :start) :line)))
              (end-line (1+ (plist-get (plist-get range :end) :line)))
              (line-info (if (= start-line end-line)
                             (format "L%d" start-line)
                           (format "L%d-%d" start-line end-line)))
              (entry (format "%s[%s] %s  %s" indent kind-name name line-info)))
         (if (and children (> (length children) 0))
             (concat entry "\n"
                     (claude-code-ide-extras-eglot--format-document-symbols
                      children (1+ depth)))
           entry)))
     (append symbols nil)
     "\n")))

;;; Tool implementations

(defun claude-code-ide-extras-eglot--format-buffer (file-path)
  "Format the specified file using Eglot formatting.
FILE-PATH must be an absolute path to the file to format."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-eglot--prepare-buffer-for-file file-path)))
      (if (not target-buffer)
          (format "Error: Could not open file: %s" file-path)
        (with-current-buffer target-buffer
          (let ((stale (not (verify-visited-file-modtime (current-buffer)))))
            (if (and (buffer-modified-p) stale)
                (format "Error: buffer has unsaved modifications and file changed on disk: %s" file-path)
              (when stale
                (revert-buffer t t t))
              (if (not (bound-and-true-p eglot--managed-mode))
                  (format "Error: Eglot not active in buffer for file: %s" file-path)
                (condition-case err
                    (progn
                      (eglot-format-buffer)
                      (save-buffer)
                      (format "Successfully formatted and saved: %s" (buffer-file-name)))
                  (error (format "Error formatting %s: %s"
                                 file-path
                                 (error-message-string err))))))))))))

(defun claude-code-ide-extras-eglot--describe-thing-at-point (file-path line column)
  "Get Eglot hover information at FILE-PATH:LINE:COLUMN.
Returns formatted hover text including type signature and documentation.
LINE is 1-based, COLUMN is 0-based (Emacs conventions)."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((target-buffer (claude-code-ide-extras-eglot--prepare-buffer-for-file file-path)))
        (with-current-buffer target-buffer
          (condition-case err
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- line))
                (move-to-column column)
                (let ((server (eglot-current-server)))
                  (if (not server)
                      (format "Error: No Eglot server active for file: %s" file-path)
                    (let* ((response (eglot--request
                                      server
                                      :textDocument/hover
                                      (eglot--TextDocumentPositionParams)))
                           (contents (plist-get response :contents)))
                      (if (not contents)
                          (format "No hover information at %s:%d:%d" file-path line column)
                        (let ((info (eglot--hover-info contents)))
                          (if (and info (not (string-empty-p info)))
                              (substring-no-properties info)
                            (format "No hover information at %s:%d:%d" file-path line column))))))))
            (error
             (format "Error getting hover info at %s:%d:%d: %s"
                     file-path line column (error-message-string err)))))))))

(defun claude-code-ide-extras-eglot--code-actions (file-path line column &optional action-kind action-title)
  "List or execute LSP code actions at FILE-PATH:LINE:COLUMN.
When ACTION-TITLE is nil, return a list of available actions.
When ACTION-TITLE is provided, execute the matching action.
ACTION-KIND optionally filters actions by kind (e.g. \"quickfix\").
LINE is 1-based, COLUMN is 0-based."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-eglot--prepare-buffer-for-file file-path)))
      (with-current-buffer target-buffer
        (condition-case err
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- line))
              (move-to-column column)
              (let ((server (eglot-current-server)))
                (if (not server)
                    (format "Error: No Eglot server active for file: %s" file-path)
                  (let* ((params (eglot--code-action-params
                                  :beg (point) :end (point)
                                  :only action-kind))
                         (actions (eglot--request server :textDocument/codeAction params))
                         (action-list (append actions nil)))
                    (if (not action-title)
                        ;; List mode
                        (if (null action-list)
                            (format "No code actions available at %s:%d:%d" file-path line column)
                          (mapconcat
                           (lambda (action)
                             (let ((title (plist-get action :title))
                                   (kind (or (plist-get action :kind) "")))
                               (format "- %s [%s]" title kind)))
                           action-list
                           "\n"))
                      ;; Execute mode
                      (let ((chosen (cl-find-if
                                     (lambda (a) (string= (plist-get a :title) action-title))
                                     action-list)))
                        (if (not chosen)
                            (format "Error: No action matching title \"%s\" at %s:%d:%d"
                                    action-title file-path line column)
                          (let ((eglot-confirm-server-edits nil))
                            (eglot-execute server chosen))
                          ;; Save any modified buffers
                          (dolist (buf (buffer-list))
                            (when (and (buffer-file-name buf)
                                       (buffer-modified-p buf))
                              (with-current-buffer buf (save-buffer))))
                          (format "Executed action: %s" action-title))))))))
          (error (format "Error with code actions at %s:%d:%d: %s"
                         file-path line column (error-message-string err))))))))

(defun claude-code-ide-extras-eglot--workspace-symbols (file-path query)
  "Search for symbols matching QUERY across the workspace.
FILE-PATH is used to identify the Eglot server."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-eglot--prepare-buffer-for-file file-path)))
      (with-current-buffer target-buffer
        (condition-case err
            (let ((server (eglot-current-server)))
              (if (not server)
                  (format "Error: No Eglot server active for file: %s" file-path)
                (let ((symbols (eglot--request server :workspace/symbol
                                               `(:query ,query))))
                  (if (or (null symbols) (zerop (length symbols)))
                      (format "No symbols found matching \"%s\"" query)
                    (claude-code-ide-extras-eglot--format-symbol-information symbols)))))
          (error (format "Error searching workspace symbols: %s"
                         (error-message-string err))))))))

(defun claude-code-ide-extras-eglot--document-symbols (file-path)
  "Get a structured outline of symbols in FILE-PATH."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-eglot--prepare-buffer-for-file file-path)))
      (with-current-buffer target-buffer
        (condition-case err
            (let ((server (eglot-current-server)))
              (if (not server)
                  (format "Error: No Eglot server active for file: %s" file-path)
                (let ((res (eglot--request server :textDocument/documentSymbol
                                           `(:textDocument
                                             ,(eglot--TextDocumentIdentifier)))))
                  (if (or (null res) (zerop (length res)))
                      (format "No symbols found in %s" file-path)
                    (let ((head (elt res 0)))
                      (eglot--dcase head
                        (((SymbolInformation))
                         (claude-code-ide-extras-eglot--format-symbol-information res))
                        (((DocumentSymbol))
                         (claude-code-ide-extras-eglot--format-document-symbols res))))))))
          (error (format "Error getting document symbols for %s: %s"
                         file-path (error-message-string err))))))))

(defun claude-code-ide-extras-eglot--rename (file-path line column new-name)
  "Rename the symbol at FILE-PATH:LINE:COLUMN to NEW-NAME.
LINE is 1-based, COLUMN is 0-based."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-eglot--prepare-buffer-for-file file-path)))
      (with-current-buffer target-buffer
        (condition-case err
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- line))
              (move-to-column column)
              (let ((server (eglot-current-server)))
                (if (not server)
                    (format "Error: No Eglot server active for file: %s" file-path)
                  (let* ((edit (eglot--request
                                server :textDocument/rename
                                `(,@(eglot--TextDocumentPositionParams)
                                  :newName ,new-name)))
                         (eglot-confirm-server-edits nil))
                    (eglot--apply-workspace-edit edit 'eglot-rename)
                    ;; Save affected buffers
                    (let ((changed-files nil))
                      (dolist (buf (buffer-list))
                        (when (and (buffer-file-name buf)
                                   (buffer-modified-p buf))
                          (with-current-buffer buf (save-buffer))
                          (push (buffer-file-name buf) changed-files)))
                      (if changed-files
                          (format "Renamed to \"%s\". Changed files:\n%s"
                                  new-name
                                  (mapconcat (lambda (f) (format "- %s" f))
                                             (nreverse changed-files) "\n"))
                        (format "Renamed to \"%s\" (changes applied)" new-name)))))))
          (error (format "Error renaming at %s:%d:%d: %s"
                         file-path line column (error-message-string err))))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-eglot-setup ()
  "Register all Eglot MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-eglot--format-buffer
   :name claude-code-ide-extras-eglot-format-buffer-tool-name
   :description "Format a specific file using Eglot LSP formatting. Requires an absolute file path."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to format.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-eglot--describe-thing-at-point
   :name claude-code-ide-extras-eglot-describe-thing-at-point-tool-name
   :description "Get Eglot hover information (type signature and documentation) at a specific location. Returns formatted text with type, parameters, and docstring."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-eglot--code-actions
   :name claude-code-ide-extras-eglot-code-actions-tool-name
   :description "List or execute LSP code actions at a specific position. Without action_title, returns available actions. With action_title, executes the matching action."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")
           (:name "action_kind"
            :type string
            :description "Optional filter for action kind (e.g. \"quickfix\", \"refactor.extract\")."
            :optional t)
           (:name "action_title"
            :type string
            :description "Title of the action to execute. If omitted, lists available actions."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-eglot--workspace-symbols
   :name claude-code-ide-extras-eglot-workspace-symbols-tool-name
   :description "Search for symbols across the workspace using Eglot LSP. Returns symbol names, kinds, file locations, and containers."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file in the project (used to identify the LSP server).")
           (:name "query"
            :type string
            :description "Search pattern to match against symbol names.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-eglot--document-symbols
   :name claude-code-ide-extras-eglot-document-symbols-tool-name
   :description "Get a structured outline of all symbols in a file using Eglot LSP. Returns hierarchical symbol names, kinds, and line ranges."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-eglot--rename
   :name claude-code-ide-extras-eglot-rename-tool-name
   :description "Rename a symbol across the project using Eglot LSP. Applies changes to all references and saves affected buffers."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file containing the symbol.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")
           (:name "new_name"
            :type string
            :description "The new name for the symbol.")))

  (message "Claude Code IDE Extras: Eglot tools registered"))

(provide 'claude-code-ide-extras-eglot)
;;; claude-code-ide-extras-eglot.el ends here
