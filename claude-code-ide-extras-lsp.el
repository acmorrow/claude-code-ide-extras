;;; claude-code-ide-extras-lsp.el --- LSP MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;;         Tim Ransom
;; Keywords: tools, lsp, ai, claude, mcp
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

;; This package provides LSP-specific MCP (Model Context Protocol) tools
;; for claude-code-ide.el, enabling Claude to:
;;
;; - Format buffers using LSP
;; - Get hover information (type signatures, documentation)
;; - Navigate call hierarchy (incoming callers / outgoing callees)
;; - Find implementations of interfaces and abstract methods
;; - Jump to type definitions
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-lsp)
;;   (claude-code-ide-extras-lsp-setup)

;;; Code:

(require 'lsp-mode nil t)
(require 'claude-code-ide)
(require 'claude-code-ide-extras-common)

(defgroup claude-code-ide-extras-lsp nil
  "LSP MCP tools for claude-code-ide."
  :group 'lsp-mode
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-lsp-")

(defconst claude-code-ide-extras-lsp-version "0.0.4"
  "Version of claude-code-ide-extras-lsp.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-lsp-format-buffer-tool-name
  "claude-code-ide-extras-lsp/format_buffer"
  "MCP tool name for format_buffer.")

(defconst claude-code-ide-extras-lsp-describe-thing-at-point-tool-name
  "claude-code-ide-extras-lsp/describe_thing_at_point"
  "MCP tool name for describe_thing_at_point.")

(defconst claude-code-ide-extras-lsp-call-hierarchy-tool-name
  "claude-code-ide-extras-lsp/call_hierarchy"
  "MCP tool name for call_hierarchy.")

(defconst claude-code-ide-extras-lsp-find-implementations-tool-name
  "claude-code-ide-extras-lsp/find_implementations"
  "MCP tool name for find_implementations.")

(defconst claude-code-ide-extras-lsp-type-definition-tool-name
  "claude-code-ide-extras-lsp/type_definition"
  "MCP tool name for type_definition.")

;;; Customization

(defcustom claude-code-ide-extras-lsp-format-buffer-usage-prompt
  "Format code using language server. Respects project formatting configuration."
  "Usage guidance for the format_buffer MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-lsp)

(put 'claude-code-ide-extras-lsp-format-buffer-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-lsp-format-buffer-tool-name)

(defcustom claude-code-ide-extras-lsp-describe-thing-at-point-usage-prompt
  "Get type information and documentation from LSP. Requires LSP server running for the file."
  "Usage guidance for the describe_thing_at_point MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-lsp)

(put 'claude-code-ide-extras-lsp-describe-thing-at-point-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-lsp-describe-thing-at-point-tool-name)

(defcustom claude-code-ide-extras-lsp-call-hierarchy-usage-prompt
  "Show incoming callers or outgoing callees for a function using LSP. Use direction \"incoming\" for callers or \"outgoing\" for callees."
  "Usage guidance for the call_hierarchy MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-lsp)

(put 'claude-code-ide-extras-lsp-call-hierarchy-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-lsp-call-hierarchy-tool-name)

(defcustom claude-code-ide-extras-lsp-find-implementations-usage-prompt
  "Find implementations of an interface, abstract method, or class using LSP. Returns file locations of all implementations."
  "Usage guidance for the find_implementations MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-lsp)

(put 'claude-code-ide-extras-lsp-find-implementations-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-lsp-find-implementations-tool-name)

(defcustom claude-code-ide-extras-lsp-type-definition-usage-prompt
  "Jump to the type definition of a symbol using LSP. Returns the file location where the type is defined."
  "Usage guidance for the type_definition MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-lsp)

(put 'claude-code-ide-extras-lsp-type-definition-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-lsp-type-definition-tool-name)

;;; Internal helpers

(defun claude-code-ide-extras-lsp--prepare-buffer-for-file (file-path)
  "Get or create buffer for FILE-PATH with LSP initialized if deferred.
If the buffer has lsp-deferred configured but not yet activated (indicated by
lsp--buffer-deferred breadcrumb), this forces immediate LSP initialization by
calling (lsp).  This enables MCP tools to access semantic information without
requiring the buffer to be displayed.

Returns the buffer, which may be newly created or pre-existing."
  (let* ((existing (get-file-buffer file-path))
         (buffer (or existing (find-file-noselect file-path))))
    (with-current-buffer buffer
      (when (bound-and-true-p lsp--buffer-deferred)
        (lsp)))
    buffer))

(defun claude-code-ide-extras-lsp--format-lsp-location (location)
  "Format an LSP Location or LocationLink hash-table LOCATION as \"file:line\".
Handles both Location (with \"uri\"/\"range\") and LocationLink
\(with \"targetUri\"/\"targetSelectionRange\")."
  (let* ((uri (or (gethash "uri" location)
                  (gethash "targetUri" location)))
         (range (or (gethash "range" location)
                    (gethash "targetSelectionRange" location)))
         (file (lsp--uri-to-path uri))
         (line (if range
                   (1+ (gethash "line" (gethash "start" range)))
                 1)))
    (format "%s:%d" file line)))

(defun claude-code-ide-extras-lsp--format-locations-response (response)
  "Format an LSP locations RESPONSE as a newline-separated string.
RESPONSE may be nil, a single Location/LocationLink hash-table, or a vector."
  (cond
   ((null response) nil)
   ((vectorp response)
    (if (zerop (length response))
        nil
      (mapconcat #'claude-code-ide-extras-lsp--format-lsp-location
                 (append response nil) "\n")))
   ((hash-table-p response)
    (claude-code-ide-extras-lsp--format-lsp-location response))
   (t nil)))

(defconst claude-code-ide-extras-lsp--symbol-kind-alist
  '((1 . "File") (2 . "Module") (3 . "Namespace") (4 . "Package")
    (5 . "Class") (6 . "Method") (7 . "Property") (8 . "Field")
    (9 . "Constructor") (10 . "Enum") (11 . "Interface") (12 . "Function")
    (13 . "Variable") (14 . "Constant") (15 . "String") (16 . "Number")
    (17 . "Boolean") (18 . "Array") (19 . "Object") (20 . "Key")
    (21 . "Null") (22 . "EnumMember") (23 . "Struct") (24 . "Event")
    (25 . "Operator") (26 . "TypeParameter"))
  "Alist mapping LSP SymbolKind integers to human-readable names.")

(defun claude-code-ide-extras-lsp--symbol-kind-name (kind)
  "Convert LSP SymbolKind integer KIND to a human-readable string."
  (or (alist-get kind claude-code-ide-extras-lsp--symbol-kind-alist)
      (format "Kind<%s>" kind)))

(defun claude-code-ide-extras-lsp--format-call-hierarchy-item (item)
  "Format a CallHierarchyItem hash-table ITEM as \"[Kind] name  file:line  (detail)\"."
  (let* ((name (gethash "name" item))
         (kind (gethash "kind" item))
         (uri (gethash "uri" item))
         (range (gethash "selectionRange" item))
         (detail (gethash "detail" item))
         (kind-name (claude-code-ide-extras-lsp--symbol-kind-name kind))
         (file (lsp--uri-to-path uri))
         (line (if range
                   (1+ (gethash "line" (gethash "start" range)))
                 1)))
    (if (and detail (not (string-empty-p detail)))
        (format "[%s] %s  %s:%d  (%s)" kind-name name file line detail)
      (format "[%s] %s  %s:%d" kind-name name file line))))

(defun claude-code-ide-extras-lsp--format-call-hierarchy-calls (calls direction)
  "Format a vector of call hierarchy CALLS as a readable string.
DIRECTION is \"incoming\" or \"outgoing\", determining whether to
extract the \"from\" or \"to\" field from each call."
  (let ((key (if (string= direction "incoming") "from" "to")))
    (mapconcat
     (lambda (call)
       (claude-code-ide-extras-lsp--format-call-hierarchy-item
        (gethash key call)))
     (append calls nil)
     "\n")))

;;; Tool implementations

(defun claude-code-ide-extras-lsp--format-buffer (file-path)
  "Format the specified file using LSP formatting.
FILE-PATH must be an absolute path to the file to format."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-lsp--prepare-buffer-for-file file-path)))
      (if (not target-buffer)
          (format "Error: Could not open file: %s" file-path)
        (with-current-buffer target-buffer
          ;; The Edit tool writes directly to disk, potentially leaving the Emacs
          ;; buffer stale. Handle the four cases:
          ;;   clean  + in-sync: proceed normally
          ;;   clean  + stale:   revert from disk, then proceed
          ;;   dirty  + in-sync: proceed normally (format buffer content, save works)
          ;;   dirty  + stale:   genuine conflict, error out
          (let ((stale (not (verify-visited-file-modtime (current-buffer)))))
            (if (and (buffer-modified-p) stale)
                (format "Error: buffer has unsaved modifications and file changed on disk: %s" file-path)
              (when stale
                (revert-buffer t t t))   ; ignore-auto, noconfirm, preserve-modes
              (if (not (bound-and-true-p lsp-mode))
                  (format "Error: LSP mode not active in buffer for file: %s" file-path)
                (condition-case err
                    (progn
                      ;; Format the buffer in place using LSP server
                      (lsp-format-buffer)

                      ;; Save automatically after formatting. Claude's intent is to format
                      ;; the FILE (persistent), not just the buffer (temporary), so auto-save
                      ;; makes this explicit. Leaving the buffer modified creates confusing
                      ;; state for the user - they see a modified indicator but didn't make
                      ;; the edit. Some LSP operations (diagnostics, indexing) may also depend
                      ;; on the file-on-disk being up to date with buffer contents. If
                      ;; formatting fails, the error propagates and the file remains unchanged
                      ;; (no partial save). Not saving and letting Claude call a separate save
                      ;; tool was considered but rejected because it adds complexity for no
                      ;; benefit since the 99% case is "format then save immediately".
                      (save-buffer)

                      (format "Successfully formatted and saved: %s" (buffer-file-name)))
                  (error (format "Error formatting %s: %s"
                                file-path
                                (error-message-string err))))))))))))

(defun claude-code-ide-extras-lsp--describe-thing-at-point (file-path line column)
  "Get LSP hover information at FILE-PATH:LINE:COLUMN.
Returns formatted hover text including type signature and documentation.
LINE is 1-based, COLUMN is 0-based (Emacs conventions)."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((target-buffer (claude-code-ide-extras-lsp--prepare-buffer-for-file file-path)))
        (with-current-buffer target-buffer
          (condition-case err
              (save-excursion
                ;; Position at the specified location
                (goto-char (point-min))
                (forward-line (1- line))
                (move-to-column column)

                ;; Query LSP server for hover information at current position. Uses
                ;; the LSP textDocument/hover protocol: construct the request with
                ;; file URI and line/column position, wrap it in LSP request format,
                ;; send it to the language server, wait for response, and extract the
                ;; content. The language server returns hover contents in either plain
                ;; text or markdown format, plus optional syntax-highlighted code blocks.
                (let ((contents (-some->> (lsp--text-document-position-params)
                                  (lsp--make-request "textDocument/hover")
                                  (lsp--send-request)
                                  (lsp:hover-contents))))
                  (if (and contents (not (equal contents "")))
                      ;; Render hover content as plain text. lsp--render-on-hover-content
                      ;; handles conversion from markdown/markup to readable text.
                      ;; Split and trim to clean up formatting artifacts.
                      (mapconcat 'string-trim-right
                                 (split-string (lsp--render-on-hover-content contents t) "\n")
                                 "\n")
                    ;; No hover info: either position is not on a symbol, or
                    ;; language server doesn't have information for this symbol.
                    ;; This is normal for whitespace, comments, or undeclared symbols.
                    (format "No hover information at %s:%d:%d" file-path line column))))
            (error
             (format "Error getting hover info at %s:%d:%d: %s"
                     file-path line column (error-message-string err)))))))))

(defun claude-code-ide-extras-lsp--call-hierarchy (file-path line column direction)
  "Get call hierarchy at FILE-PATH:LINE:COLUMN in DIRECTION.
DIRECTION is \"incoming\" (callers) or \"outgoing\" (callees).
LINE is 1-based, COLUMN is 0-based."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-lsp--prepare-buffer-for-file file-path)))
      (with-current-buffer target-buffer
        (condition-case err
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- line))
              (move-to-column column)
              (if (not (bound-and-true-p lsp-mode))
                  (format "Error: LSP mode not active for file: %s" file-path)
                (let ((items (lsp-request
                              "textDocument/prepareCallHierarchy"
                              (lsp--text-document-position-params))))
                  (if (or (null items) (zerop (length items)))
                      (format "No call hierarchy item found at %s:%d:%d" file-path line column)
                    (let* ((item (elt items 0))
                           (method (if (string= direction "incoming")
                                       "callHierarchy/incomingCalls"
                                     "callHierarchy/outgoingCalls"))
                           (calls (lsp-request method
                                               (list :item item))))
                      (if (or (null calls) (zerop (length calls)))
                          (format "No %s calls found for symbol at %s:%d:%d"
                                  direction file-path line column)
                        (claude-code-ide-extras-lsp--format-call-hierarchy-calls
                         calls direction)))))))
          (error (format "Error getting call hierarchy at %s:%d:%d: %s"
                         file-path line column (error-message-string err))))))))

(defun claude-code-ide-extras-lsp--find-implementations (file-path line column)
  "Find implementations of symbol at FILE-PATH:LINE:COLUMN.
LINE is 1-based, COLUMN is 0-based."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-lsp--prepare-buffer-for-file file-path)))
      (with-current-buffer target-buffer
        (condition-case err
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- line))
              (move-to-column column)
              (if (not (bound-and-true-p lsp-mode))
                  (format "Error: LSP mode not active for file: %s" file-path)
                (let* ((response (lsp-request
                                  "textDocument/implementation"
                                  (lsp--text-document-position-params)))
                       (formatted (claude-code-ide-extras-lsp--format-locations-response response)))
                  (or formatted
                      (format "No implementations found for symbol at %s:%d:%d"
                              file-path line column)))))
          (error (format "Error finding implementations at %s:%d:%d: %s"
                         file-path line column (error-message-string err))))))))

(defun claude-code-ide-extras-lsp--type-definition (file-path line column)
  "Get type definition of symbol at FILE-PATH:LINE:COLUMN.
LINE is 1-based, COLUMN is 0-based."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((target-buffer (claude-code-ide-extras-lsp--prepare-buffer-for-file file-path)))
      (with-current-buffer target-buffer
        (condition-case err
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- line))
              (move-to-column column)
              (if (not (bound-and-true-p lsp-mode))
                  (format "Error: LSP mode not active for file: %s" file-path)
                (let* ((response (lsp-request
                                  "textDocument/typeDefinition"
                                  (lsp--text-document-position-params)))
                       (formatted (claude-code-ide-extras-lsp--format-locations-response response)))
                  (or formatted
                      (format "No type definition found for symbol at %s:%d:%d"
                              file-path line column)))))
          (error (format "Error getting type definition at %s:%d:%d: %s"
                         file-path line column (error-message-string err))))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-lsp-setup ()
  "Register all LSP MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-lsp--format-buffer
   :name claude-code-ide-extras-lsp-format-buffer-tool-name
   :description "Format a specific file using LSP formatting. Requires an absolute file path."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to format.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-lsp--describe-thing-at-point
   :name claude-code-ide-extras-lsp-describe-thing-at-point-tool-name
   :description "Get LSP hover information (type signature and documentation) at a specific location. Returns formatted text with type, parameters, and docstring."
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
   :function #'claude-code-ide-extras-lsp--call-hierarchy
   :name claude-code-ide-extras-lsp-call-hierarchy-tool-name
   :description "Get incoming callers or outgoing callees for a function using LSP. Uses the two-step call hierarchy protocol."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file containing the function.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")
           (:name "direction"
            :type string
            :description "\"incoming\" for callers or \"outgoing\" for callees.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-lsp--find-implementations
   :name claude-code-ide-extras-lsp-find-implementations-tool-name
   :description "Find implementations of an interface, abstract method, or class using LSP. Returns file:line locations."
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
   :function #'claude-code-ide-extras-lsp--type-definition
   :name claude-code-ide-extras-lsp-type-definition-tool-name
   :description "Jump to the type definition of a symbol using LSP. Returns the file:line location where the type is defined."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")))

  (message "Claude Code IDE Extras: LSP tools registered"))

(provide 'claude-code-ide-extras-lsp)
;;; claude-code-ide-extras-lsp.el ends here
