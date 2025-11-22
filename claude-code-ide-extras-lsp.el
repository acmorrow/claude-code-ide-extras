;;; claude-code-ide-extras-lsp.el --- LSP MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
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
;; - Interact with LSP language servers
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-lsp)
;;   (claude-code-ide-extras-lsp-setup)

;;; Code:

(require 'lsp-mode)
(require 'claude-code-ide)

(defgroup claude-code-ide-extras-lsp nil
  "LSP MCP tools for claude-code-ide."
  :group 'lsp-mode
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-lsp-")

(defconst claude-code-ide-extras-lsp-version "0.0.0"
  "Version of claude-code-ide-extras-lsp.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-lsp-format-buffer-tool-name
  "claude-code-ide-extras-lsp/format_buffer"
  "MCP tool name for format_buffer.")

(defconst claude-code-ide-extras-lsp-describe-thing-at-point-tool-name
  "claude-code-ide-extras-lsp/describe_thing_at_point"
  "MCP tool name for describe_thing_at_point.")

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

;;; Tool implementations

  (defun claude-code-ide-extras-lsp--format-buffer (file-path)
    "Format the specified file using LSP formatting.
FILE-PATH must be an absolute path to the file to format."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((target-buffer (or (find-buffer-visiting file-path)
                               (find-file-noselect file-path))))
        (if (not target-buffer)
            (format "Error: Could not open file: %s" file-path)
          (with-current-buffer target-buffer
            (if (not (bound-and-true-p lsp-mode))
                (format "Error: LSP mode not active in buffer for file: %s" file-path)
              (condition-case err
                  (progn
                    (lsp-format-buffer)
                    (save-buffer)
                    (format "Successfully formatted and saved: %s" (buffer-file-name)))
                (error (format "Error formatting %s: %s"
                              file-path
                              (error-message-string err))))))))))

  ;; lsp-describe-thing-at-point wrapper (returns hover info as string)
  (defun claude-code-ide-extras-lsp--describe-thing-at-point (file-path line column)
    "Get LSP hover information at FILE-PATH:LINE:COLUMN.
Returns formatted hover text including type signature and documentation.
LINE is 1-based, COLUMN is 0-based (Emacs conventions)."
    (if (not file-path)
        (error "file_path parameter is required")
      (claude-code-ide-mcp-server-with-session-context nil
        (let ((target-buffer (or (find-buffer-visiting file-path)
                                 (find-file-noselect file-path))))
          (with-current-buffer target-buffer
            (condition-case err
                (save-excursion
                  ;; Position at the specified location
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column column)
                  ;; Get hover contents from LSP
                  (let ((contents (-some->> (lsp--text-document-position-params)
                                    (lsp--make-request "textDocument/hover")
                                    (lsp--send-request)
                                    (lsp:hover-contents))))
                    (if (and contents (not (equal contents "")))
                        ;; Render the hover content as text (same as lsp--display-contents does)
                        (mapconcat 'string-trim-right
                                   (split-string (lsp--render-on-hover-content contents t) "\n")
                                   "\n")
                      (format "No hover information at %s:%d:%d" file-path line column))))
              (error
               (format "Error getting hover info at %s:%d:%d: %s"
                       file-path line column (error-message-string err)))))))))

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

  (message "Claude Code IDE Extras: LSP tools registered"))

(provide 'claude-code-ide-extras-lsp)
;;; claude-code-ide-extras-lsp.el ends here
