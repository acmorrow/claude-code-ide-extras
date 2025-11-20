;;; claude-code-ide-extras-common.el --- Common utilities for claude-code-ide-extras  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

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

(defcustom claude-code-ide-extras-common-max-line-length 2000
  "Maximum length of a single line when querying buffers.
Lines longer than this are truncated to prevent token overflow."
  :type 'integer
  :group 'claude-code-ide-extras-common)

;;; Buffer utilities

(defun claude-code-ide-extras-common--buffer-search (buffer-name pattern &optional context-lines)
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

(defun claude-code-ide-extras-common--buffer-query (buffer-name &optional start-line num-lines)
  "Retrieve contents from BUFFER-NAME.
BUFFER-NAME is the name of the buffer to query.
Optional START-LINE is the first line to retrieve (1-based, negative
counts from end).
Optional NUM-LINES is the number of lines to retrieve.

START-LINE and NUM-LINES must both be provided or both be omitted.
If omitted, returns entire buffer contents.
If START-LINE is negative, counts from end (-1 = last line, -100 =
100th from end).

Lines longer than `claude-code-ide-extras-common-max-line-length'
are truncated.
Returns the buffer contents for the specified line range."
  (let ((buf (get-buffer buffer-name)))
    (if (not buf)
        (format "Error: Buffer not found: %s" buffer-name)
      ;; Validate parameters
      (when (or (and start-line (not num-lines))
                (and num-lines (not start-line)))
        (error "start-line and num-lines must both be provided or both be omitted"))
      (with-current-buffer buf
        (save-excursion
          (if (not start-line)
              ;; No range specified - return whole buffer
              (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                     (lines (split-string content "\n" t)))
                (string-join
                 (mapcar (lambda (line)
                           (if (> (length line) claude-code-ide-extras-common-max-line-length)
                               (substring line 0 claude-code-ide-extras-common-max-line-length)
                             line))
                         lines)
                 "\n"))
            ;; Range specified - compute actual start line
            (let* ((total-lines (count-lines (point-min) (point-max)))
                   (actual-start (if (< start-line 0)
                                     (+ total-lines start-line 1)
                                   start-line))
                   ;; Clamp to valid range
                   (actual-start (max 1 (min actual-start total-lines))))
              (goto-char (point-min))
              (forward-line (1- actual-start))
              (let* ((start-pos (point))
                     (_ (forward-line num-lines))
                     (end-pos (point))
                     (content (buffer-substring-no-properties start-pos end-pos))
                     (lines (split-string content "\n" t)))
                ;; Truncate long lines
                (string-join
                 (mapcar (lambda (line)
                           (if (> (length line) claude-code-ide-extras-common-max-line-length)
                               (substring line 0 claude-code-ide-extras-common-max-line-length)
                             line))
                         lines)
                 "\n")))))))))

(provide 'claude-code-ide-extras-common)
;;; claude-code-ide-extras-common.el ends here
