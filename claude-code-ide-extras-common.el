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

(require 'seq)

(defgroup claude-code-ide-extras-common nil
  "Common utilities for claude-code-ide-extras packages."
  :group 'tools
  :prefix "claude-code-ide-extras-common-")

(defconst claude-code-ide-extras-common-version "0.0.3"
  "Version of claude-code-ide-extras-common.")

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

      ;; Occur always creates or reuses a buffer named "*Occur*". If the user
      ;; has existing occur results visible, preserve them to avoid clobbering
      ;; the user's interactive work. Temporarily rename any existing *Occur*
      ;; buffer, run the search, extract the results, and then restore the
      ;; original. This isolation ensures Claude's searches don't interfere
      ;; with the user's workflow.
      (let ((saved-occur-buf (get-buffer "*Occur*")))
        ;; Preserve any existing *Occur* buffer by renaming it.
        (when saved-occur-buf
          (with-current-buffer saved-occur-buf
            (rename-buffer (generate-new-buffer-name "*Occur*") t)))

        (unwind-protect
            ;; Run the search in an isolated window environment. save-window-excursion
            ;; prevents occur from changing the user's window layout.
            (save-window-excursion
              (progn
                ;; Occur creates the *Occur* buffer only if matches are found.
                (with-current-buffer buf
                  (occur pattern (or context-lines 0)))

                ;; Extract results if matches were found.
                (let ((occur-buf (get-buffer "*Occur*")))
                  (if occur-buf
                      ;; Success path: matches found, buffer created.
                      (with-current-buffer occur-buf
                        (buffer-substring-no-properties (point-min) (point-max)))
                    ;; No matches: occur didn't create a buffer.
                    "0 matches found"))))

          ;; Cleanup - always restore original state, even on error.
          ;; Kill our temporary *Occur* buffer.
          (when (get-buffer "*Occur*")
            (kill-buffer "*Occur*"))

          ;; Restore the user's original *Occur* buffer if it existed.
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
    ;; Validate buffer exists.
    (if (not buf)
        (format "Error: Buffer not found: %s" buffer-name)

      ;; Validate parameter consistency. Range parameters are all-or-nothing to
      ;; avoid ambiguity. Requiring both ensures clear semantics: either "all
      ;; content" or "specific range", never "partial range specification".
      (when (or (and start-line (not num-lines))
                (and num-lines (not start-line)))
        (error "start-line and num-lines must both be provided or both be omitted"))
      (with-current-buffer buf
        (save-excursion
          (if (not start-line)
              ;; Extract entire buffer. Simple case - no range calculation needed.
              (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                     (lines (split-string content "\n" t)))
                (string-join
                 (mapcar (lambda (line)
                           ;; Truncate excessively long lines to prevent token overflow
                           ;; in Claude's context window. Long lines are typically
                           ;; minified code or data dumps, not useful for reasoning.
                           (if (> (length line) claude-code-ide-extras-common-max-line-length)
                               (substring line 0 claude-code-ide-extras-common-max-line-length)
                             line))
                         lines)
                 "\n"))

            ;; Extract specific range. Handle negative indexing where -1 means
            ;; last line, -100 means 100th line from end. This matches common
            ;; tail/head semantics and is useful for "show me last N lines of
            ;; compilation output".
            (let* ((total-lines (count-lines (point-min) (point-max)))
                   (actual-start (if (< start-line 0)
                                     (+ total-lines start-line 1)
                                   start-line))
                   ;; Clamp to valid range - don't error on out-of-bounds, just
                   ;; adjust to nearest valid value. This is more forgiving for
                   ;; Claude's imprecise line count estimates.
                   (actual-start (max 1 (min actual-start total-lines))))

              ;; Navigate to start position.
              (goto-char (point-min))
              (forward-line (1- actual-start))
              (let* ((start-pos (point))
                     ;; forward-line moves point, doesn't return position.
                     (_ (forward-line num-lines))
                     (end-pos (point))
                     (content (buffer-substring-no-properties start-pos end-pos))
                     (lines (split-string content "\n" t)))
                ;; Apply same truncation as whole-buffer case.
                (string-join
                 (mapcar (lambda (line)
                           (if (> (length line) claude-code-ide-extras-common-max-line-length)
                               (substring line 0 claude-code-ide-extras-common-max-line-length)
                             line))
                         lines)
                 "\n")))))))))

;;; Buffer-local variable utilities

(defun claude-code-ide-extras-common--get-buffer-local-keys (file-path &optional filter-regex)
  "Get buffer-local variable names for FILE-PATH.
Opens FILE-PATH and returns list of buffer-local variable names.
Optional FILTER-REGEX (Emacs regex) filters the returned names.

Only kills the buffer if it was opened by this function (not already open)."
  ;; Validate inputs
  (unless (and file-path (stringp file-path))
    (error "file-path must be a non-nil string"))
  (when (file-directory-p file-path)
    (error "file-path must be a file, not a directory: %s" file-path))

  (let* ((existing-buffer (find-buffer-visiting file-path))
         (buffer (find-file-noselect file-path)))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((vars (buffer-local-variables))
                 ;; Extract variable names (car of each pair)
                 ;; Skip entries that aren't proper cons cells
                 (names (delq nil
                              (mapcar (lambda (entry)
                                        (when (consp entry)
                                          (symbol-name (car entry))))
                                      vars)))
                 ;; Apply filter if provided
                 (filtered (if filter-regex
                               (condition-case err
                                   (seq-filter (lambda (name)
                                                 (string-match-p filter-regex name))
                                               names)
                                 (invalid-regexp
                                  (error "Invalid regular expression '%s': %s"
                                         filter-regex (error-message-string err))))
                             names)))
            ;; Return as newline-separated list for readability
            (string-join (sort filtered #'string<) "\n")))
      ;; Only kill buffer if we opened it (not already open)
      (unless existing-buffer
        (kill-buffer buffer)))))

(defun claude-code-ide-extras-common--get-buffer-local-variables (file-path &optional filter-regex)
  "Get buffer-local variables with values for FILE-PATH.
Opens FILE-PATH and returns buffer-local-variables as a Lisp form.
Optional FILTER-REGEX (Emacs regex) filters variables by name before retrieving values.

Only kills the buffer if it was opened by this function (not already open)."
  ;; Validate inputs
  (unless (and file-path (stringp file-path))
    (error "file-path must be a non-nil string"))
  (when (file-directory-p file-path)
    (error "file-path must be a file, not a directory: %s" file-path))

  (let* ((existing-buffer (find-buffer-visiting file-path))
         (buffer (find-file-noselect file-path)))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((vars (buffer-local-variables))
                 ;; Apply filter if provided
                 (filtered (if filter-regex
                               (condition-case err
                                   (seq-filter (lambda (entry)
                                                 (and (consp entry)
                                                      (string-match-p filter-regex
                                                                      (symbol-name (car entry)))))
                                               vars)
                                 (invalid-regexp
                                  (error "Invalid regular expression '%s': %s"
                                         filter-regex (error-message-string err))))
                             vars)))
            (format "%S" filtered)))
      ;; Only kill buffer if we opened it (not already open)
      (unless existing-buffer
        (kill-buffer buffer)))))

(provide 'claude-code-ide-extras-common)
;;; claude-code-ide-extras-common.el ends here
