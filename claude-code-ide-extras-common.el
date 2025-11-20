;;; claude-code-ide-extras-common.el --- Common utilities for claude-code-ide-extras  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;; Package-Requires: ((emacs "30.1"))
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

;; This package provides common utilities shared across claude-code-ide-extras
;; packages. It includes buffer search and query utilities used by multiple
;; MCP tool implementations.
;;
;; This is an internal library - no MCP tools are registered here.
;; Other packages (projectile, core, lsp) depend on this library.
;;
;; Part of the claude-code-ide-extras suite.

;;; Code:

(defgroup claude-code-ide-extras-common nil
  "Common utilities for claude-code-ide-extras packages."
  :group 'tools
  :prefix "claude-code-ide-extras-common-")

;;; Buffer utilities

(defun claude-code-ide-extras-common--search-buffer (buffer-name pattern &optional context-lines)
  "Search BUFFER-NAME for PATTERN using occur, return formatted results.
BUFFER-NAME is the name of the buffer to search.
PATTERN is a regular expression to search for.
CONTEXT-LINES specifies number of lines before/after each match (default 0)."
  (let ((buf (get-buffer buffer-name)))
    (if (not buf)
        (format "Error: Buffer not found: %s" buffer-name)
      (let ((saved-occur-buf (get-buffer "*Occur*")))
        ;; Save any existing *Occur* buffer by renaming it temporarily
        (when saved-occur-buf
          (with-current-buffer saved-occur-buf
            (rename-buffer (generate-new-buffer-name "*Occur*") t)))
        (unwind-protect
            (save-window-excursion
              (progn
                ;; Run occur - creates new *Occur*
                (with-current-buffer buf
                  (occur pattern (or context-lines 0)))
                ;; Read from the new *Occur* buffer
                (with-current-buffer "*Occur*"
                  (buffer-substring-no-properties (point-min) (point-max)))))
          ;; Clean up: kill our *Occur*, restore saved one
          (when (get-buffer "*Occur*")
            (kill-buffer "*Occur*"))
          (when saved-occur-buf
            (with-current-buffer saved-occur-buf
              (rename-buffer "*Occur*"))))))))

(provide 'claude-code-ide-extras-common)
;;; claude-code-ide-extras-common.el ends here
