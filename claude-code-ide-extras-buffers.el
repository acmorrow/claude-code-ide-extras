;;; claude-code-ide-extras-buffers.el --- Buffer management MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow
;; Co-author: Tim Ransom

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;; Keywords: tools, ai, claude, mcp
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

;; This package provides buffer management MCP (Model Context Protocol)
;; tools for claude-code-ide.el, enabling Claude to:
;;
;; - List all open buffers with metadata and filtering
;; - Get detailed information about a specific buffer
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-buffers)
;;   (claude-code-ide-extras-buffers-setup)

;;; Code:

(require 'claude-code-ide)

(defgroup claude-code-ide-extras-buffers nil
  "Buffer management MCP tools for claude-code-ide."
  :group 'tools
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-buffers-")

(defconst claude-code-ide-extras-buffers-version "0.0.4"
  "Version of claude-code-ide-extras-buffers.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-buffers-list-buffers-tool-name
  "claude-code-ide-extras-buffers/list_buffers"
  "MCP tool name for list_buffers.")

(defconst claude-code-ide-extras-buffers-get-buffer-info-tool-name
  "claude-code-ide-extras-buffers/get_buffer_info"
  "MCP tool name for get_buffer_info.")

;;; Customization

(defcustom claude-code-ide-extras-buffers-list-buffers-usage-prompt
  "List all open buffers with optional filtering by major mode or modified status. Useful for discovering what buffers are available."
  "Usage guidance for the list_buffers MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-buffers)

(put 'claude-code-ide-extras-buffers-list-buffers-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-buffers-list-buffers-tool-name)

(defcustom claude-code-ide-extras-buffers-get-buffer-info-usage-prompt
  "Get detailed information about a specific buffer including major mode, minor modes, encoding, and process info."
  "Usage guidance for the get_buffer_info MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-buffers)

(put 'claude-code-ide-extras-buffers-get-buffer-info-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-buffers-get-buffer-info-tool-name)

;;; Internal helpers

(defun claude-code-ide-extras-buffers--format-buffer-entry (buf)
  "Format a single buffer BUF as a summary line."
  (with-current-buffer buf
    (let ((name (buffer-name buf))
          (file (or (buffer-file-name buf) ""))
          (mode (symbol-name major-mode))
          (modified (if (buffer-modified-p buf) "modified" "unmodified"))
          (size (buffer-size buf)))
      (format "  %s\n    File: %s\n    Mode: %s\n    Status: %s\n    Size: %d bytes"
              name
              (if (string-empty-p file) "(no file)" file)
              mode
              modified
              size))))

(defun claude-code-ide-extras-buffers--internal-buffer-p (buf)
  "Return non-nil if BUF is an internal buffer (name starts with space)."
  (string-prefix-p " " (buffer-name buf)))

;;; Tool implementations

(defun claude-code-ide-extras-buffers--list-buffers (&optional mode-filter modified-only)
  "List all open buffers with metadata.
Optional MODE-FILTER is a string to filter buffers by major mode name.
Optional MODIFIED-ONLY, when \"true\", shows only modified buffers."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((all-buffers (buffer-list))
               (buffers (seq-remove #'claude-code-ide-extras-buffers--internal-buffer-p all-buffers))
               ;; Filter by mode if requested
               (buffers (if (and mode-filter (not (string-empty-p mode-filter)))
                            (seq-filter
                             (lambda (buf)
                               (with-current-buffer buf
                                 (string-match-p mode-filter (symbol-name major-mode))))
                             buffers)
                          buffers))
               ;; Filter by modified status if requested
               (buffers (if (and modified-only (equal modified-only "true"))
                            (seq-filter #'buffer-modified-p buffers)
                          buffers)))
          (if (null buffers)
              "No buffers match the given criteria."
            (concat
             (format "Buffers (%d):\n\n" (length buffers))
             (mapconcat #'claude-code-ide-extras-buffers--format-buffer-entry buffers "\n\n"))))
      (error (format "Error listing buffers: %s" (error-message-string err))))))

(defun claude-code-ide-extras-buffers--get-buffer-info (buffer-name)
  "Get detailed information about a specific buffer.
BUFFER-NAME is the name of the buffer to inspect."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buf (get-buffer buffer-name)))
          (if (not buf)
              (format "Error: Buffer '%s' not found." buffer-name)
            (with-current-buffer buf
              (let* ((name (buffer-name buf))
                     (file (or (buffer-file-name buf) "(no file)"))
                     (mode (symbol-name major-mode))
                     (minor-modes
                      (mapconcat #'symbol-name
                                 (seq-filter
                                  (lambda (m)
                                    (and (boundp m) (symbol-value m)))
                                  minor-mode-list)
                                 ", "))
                     (modified (if (buffer-modified-p buf) "yes" "no"))
                     (read-only (if buffer-read-only "yes" "no"))
                     (size (buffer-size buf))
                     (line-count (count-lines (point-min) (point-max)))
                     (encoding (symbol-name buffer-file-coding-system))
                     (proc (get-buffer-process buf))
                     (proc-info (if proc
                                    (format "%s (%s)"
                                            (process-name proc)
                                            (symbol-name (process-status proc)))
                                  "none")))
                (format (concat
                         "Buffer: %s\n"
                         "File: %s\n"
                         "Major mode: %s\n"
                         "Minor modes: %s\n"
                         "Modified: %s\n"
                         "Read-only: %s\n"
                         "Size: %d bytes\n"
                         "Lines: %d\n"
                         "Encoding: %s\n"
                         "Process: %s")
                        name file mode minor-modes modified read-only
                        size line-count encoding proc-info)))))
      (error (format "Error getting buffer info: %s" (error-message-string err))))))

;;; Tool registration

(defun claude-code-ide-extras-buffers-setup ()
  "Register buffer management MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-buffers--list-buffers
   :name claude-code-ide-extras-buffers-list-buffers-tool-name
   :description "List all open Emacs buffers with metadata. Returns buffer name, file path, major mode, modified status, and size. Filters out internal buffers by default. Use mode_filter to show only buffers in a specific major mode, and modified_only to show only unsaved buffers."
   :args '((:name "mode_filter"
            :type string
            :description "Optional regex to filter buffers by major mode name (e.g., \"org-mode\", \"python\")."
            :optional t)
           (:name "modified_only"
            :type string
            :description "Set to \"true\" to show only modified (unsaved) buffers."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-buffers--get-buffer-info
   :name claude-code-ide-extras-buffers-get-buffer-info-tool-name
   :description "Get detailed information about a specific Emacs buffer. Returns buffer name, file path, major mode, active minor modes, modified status, read-only status, size, line count, encoding, and associated process."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the buffer to inspect (e.g., '*scratch*', 'init.el').")))

  (message "Claude Code IDE Extras: Buffer management tools registered"))

(provide 'claude-code-ide-extras-buffers)
;;; claude-code-ide-extras-buffers.el ends here
