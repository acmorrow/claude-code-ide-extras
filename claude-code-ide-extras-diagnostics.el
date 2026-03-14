;;; claude-code-ide-extras-diagnostics.el --- Flycheck/Flymake diagnostics MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow, Tim Ransom

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;; Co-author: Tim Ransom
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

;; This package provides diagnostics (errors, warnings, info) MCP
;; (Model Context Protocol) tools for claude-code-ide.el, enabling
;; Claude to:
;;
;; - Retrieve diagnostics (errors/warnings/info) for a file or buffer
;; - Get a summary of diagnostic counts across buffers
;;
;; Supports both flymake (built-in) and flycheck as diagnostic backends.
;; Flymake is preferred when both are active.
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-diagnostics)
;;   (claude-code-ide-extras-diagnostics-setup)

;;; Code:

(require 'claude-code-ide)
(require 'claude-code-ide-extras-common)

;; Optional requires for diagnostic backends
(require 'flymake nil t)
(require 'flycheck nil t)

;; Declare flycheck functions to suppress byte-compiler warnings
(declare-function flycheck-error-filename "flycheck" (err))
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))
(declare-function flycheck-error-checker "flycheck" (err))
(declare-function flycheck-buffer "flycheck" ())
(defvar flycheck-current-errors)

(require 'compile)

(defgroup claude-code-ide-extras-diagnostics nil
  "Flycheck/Flymake diagnostics MCP tools for claude-code-ide."
  :group 'tools
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-diagnostics-")

(defconst claude-code-ide-extras-diagnostics-version "0.0.4"
  "Version of claude-code-ide-extras-diagnostics.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-diagnostics-get-diagnostics-tool-name
  "claude-code-ide-extras-diagnostics/get_diagnostics"
  "MCP tool name for get_diagnostics.")

(defconst claude-code-ide-extras-diagnostics-get-diagnostics-summary-tool-name
  "claude-code-ide-extras-diagnostics/get_diagnostics_summary"
  "MCP tool name for get_diagnostics_summary.")

(defconst claude-code-ide-extras-diagnostics-recheck-diagnostics-tool-name
  "claude-code-ide-extras-diagnostics/recheck_diagnostics"
  "MCP tool name for recheck_diagnostics.")

(defconst claude-code-ide-extras-diagnostics-get-compilation-errors-tool-name
  "claude-code-ide-extras-diagnostics/get_compilation_errors"
  "MCP tool name for get_compilation_errors.")

;;; Customization

(defcustom claude-code-ide-extras-diagnostics-get-diagnostics-usage-prompt
  "Get all diagnostics (errors, warnings, info) for a file or the current buffer. Tries flymake first, then flycheck. Returns formatted diagnostics grouped by severity."
  "Usage guidance for the get_diagnostics MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-diagnostics)

(put 'claude-code-ide-extras-diagnostics-get-diagnostics-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-diagnostics-get-diagnostics-tool-name)

(defcustom claude-code-ide-extras-diagnostics-get-diagnostics-summary-usage-prompt
  "Get a count summary of diagnostics across all buffers or for a specific file. Returns counts per severity level and total."
  "Usage guidance for the get_diagnostics_summary MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-diagnostics)

(put 'claude-code-ide-extras-diagnostics-get-diagnostics-summary-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-diagnostics-get-diagnostics-summary-tool-name)

(defcustom claude-code-ide-extras-diagnostics-recheck-diagnostics-usage-prompt
  "Trigger a re-check of diagnostics for a file or current buffer. Forces flymake or flycheck to re-run its checkers."
  "Usage guidance for the recheck_diagnostics MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-diagnostics)

(put 'claude-code-ide-extras-diagnostics-recheck-diagnostics-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-diagnostics-recheck-diagnostics-tool-name)

(defcustom claude-code-ide-extras-diagnostics-get-compilation-errors-usage-prompt
  "Parse errors from a compilation buffer (e.g. *compilation*, *grep*). Returns structured error locations extracted by Emacs' compilation-mode parser."
  "Usage guidance for the get_compilation_errors MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-diagnostics)

(put 'claude-code-ide-extras-diagnostics-get-compilation-errors-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-diagnostics-get-compilation-errors-tool-name)

;;; Internal helpers

(defun claude-code-ide-extras-diagnostics--flymake-type-to-level (type)
  "Convert flymake diagnostic TYPE to a severity level string."
  (cond
   ((eq type :error) "error")
   ((eq type :warning) "warning")
   ((eq type :note) "note")
   ;; Handle custom flymake types by checking their severity property
   ((and (symbolp type) (get type 'flymake-category))
    (let ((category (get type 'flymake-category)))
      (cond
       ((eq category 'flymake-error) "error")
       ((eq category 'flymake-warning) "warning")
       ((eq category 'flymake-note) "note")
       (t (symbol-name type)))))
   (t (format "%s" type))))

(defun claude-code-ide-extras-diagnostics--flycheck-level-to-string (level)
  "Convert flycheck error LEVEL to a severity level string."
  (cond
   ((eq level 'error) "error")
   ((eq level 'warning) "warning")
   ((eq level 'info) "info")
   (t (format "%s" level))))

(defun claude-code-ide-extras-diagnostics--get-buffer-for-file (file-path)
  "Get or find a buffer visiting FILE-PATH.
If FILE-PATH is nil, return the current buffer.
Returns the buffer or nil if the file cannot be found."
  (if file-path
      (or (find-buffer-visiting file-path)
          (when (file-exists-p file-path)
            (find-file-noselect file-path)))
    (current-buffer)))

(defun claude-code-ide-extras-diagnostics--collect-flymake (buffer)
  "Collect flymake diagnostics from BUFFER.
Returns a list of plists with :file, :line, :col, :level, :message, :checker."
  (with-current-buffer buffer
    (when (bound-and-true-p flymake-mode)
      (let ((diags (flymake-diagnostics))
            (results nil))
        (dolist (diag diags)
          (let* ((beg (flymake-diagnostic-beg diag))
                 (type (flymake-diagnostic-type diag))
                 (text (flymake-diagnostic-text diag))
                 (diag-buffer (flymake-diagnostic-buffer diag))
                 (file (if (and diag-buffer (buffer-file-name diag-buffer))
                           (buffer-file-name diag-buffer)
                         (or (buffer-file-name buffer) (buffer-name buffer))))
                 (line (with-current-buffer (or diag-buffer buffer)
                         (save-excursion
                           (goto-char beg)
                           (line-number-at-pos))))
                 (col (with-current-buffer (or diag-buffer buffer)
                        (save-excursion
                          (goto-char beg)
                          (current-column))))
                 (level (claude-code-ide-extras-diagnostics--flymake-type-to-level type))
                 (backend (if (fboundp 'flymake-diagnostic-backend)
                              (let ((be (flymake-diagnostic-backend diag)))
                                (if be (format "%s" be) "flymake"))
                            "flymake")))
            (push (list :file file :line line :col col
                        :level level :message text :checker backend)
                  results)))
        (nreverse results)))))

(defun claude-code-ide-extras-diagnostics--collect-flycheck (buffer)
  "Collect flycheck diagnostics from BUFFER.
Returns a list of plists with :file, :line, :col, :level, :message, :checker."
  (with-current-buffer buffer
    (when (bound-and-true-p flycheck-mode)
      (let ((errors flycheck-current-errors)
            (results nil))
        (dolist (err errors)
          (let* ((file (or (flycheck-error-filename err)
                           (buffer-file-name buffer)
                           (buffer-name buffer)))
                 (line (or (flycheck-error-line err) 0))
                 (col (or (flycheck-error-column err) 0))
                 (level (claude-code-ide-extras-diagnostics--flycheck-level-to-string
                         (flycheck-error-level err)))
                 (message (or (flycheck-error-message err) ""))
                 (checker (format "%s" (flycheck-error-checker err))))
            (push (list :file file :line line :col col
                        :level level :message message :checker checker)
                  results)))
        (nreverse results)))))

(defun claude-code-ide-extras-diagnostics--collect (buffer)
  "Collect diagnostics from BUFFER using flymake or flycheck.
Prefers flymake when both are active.
Returns a list of plists with :file, :line, :col, :level, :message, :checker."
  (or (claude-code-ide-extras-diagnostics--collect-flymake buffer)
      (claude-code-ide-extras-diagnostics--collect-flycheck buffer)
      nil))

(defun claude-code-ide-extras-diagnostics--format-entry (entry)
  "Format a single diagnostic ENTRY plist as a string.
Format: file:line:col: level: message (checker)"
  (format "%s:%d:%d: %s: %s (%s)"
          (plist-get entry :file)
          (plist-get entry :line)
          (plist-get entry :col)
          (plist-get entry :level)
          (plist-get entry :message)
          (plist-get entry :checker)))

(defun claude-code-ide-extras-diagnostics--group-by-level (entries)
  "Group diagnostic ENTRIES by severity level.
Returns an alist of (level . entries)."
  (let ((groups nil))
    (dolist (entry entries)
      (let* ((level (plist-get entry :level))
             (existing (assoc level groups)))
        (if existing
            (setcdr existing (append (cdr existing) (list entry)))
          (push (cons level (list entry)) groups))))
    ;; Sort: errors first, then warnings, then notes/info, then other
    (sort groups
          (lambda (a b)
            (let ((order '("error" "warning" "note" "info")))
              (< (or (cl-position (car a) order :test #'string=) 99)
                 (or (cl-position (car b) order :test #'string=) 99)))))))

(defun claude-code-ide-extras-diagnostics--count-by-level (entries)
  "Count diagnostic ENTRIES by severity level.
Returns an alist of (level . count)."
  (let ((counts nil))
    (dolist (entry entries)
      (let* ((level (plist-get entry :level))
             (existing (assoc level counts)))
        (if existing
            (setcdr existing (1+ (cdr existing)))
          (push (cons level 1) counts))))
    counts))

;;; Tool implementations

(defun claude-code-ide-extras-diagnostics--get-diagnostics (&optional file-path)
  "Get all diagnostics for FILE-PATH or the current buffer.
Returns a formatted string with diagnostics grouped by severity level."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buffer (claude-code-ide-extras-diagnostics--get-buffer-for-file file-path)))
          (if (not buffer)
              (format "Error: Cannot find buffer for %s" (or file-path "current buffer"))
            (let ((entries (claude-code-ide-extras-diagnostics--collect buffer)))
              (if (not entries)
                  (format "No diagnostics found for %s.\nNeither flymake nor flycheck appears to be active in this buffer."
                          (or file-path (buffer-name buffer)))
                (let ((grouped (claude-code-ide-extras-diagnostics--group-by-level entries))
                      (parts nil))
                  (dolist (group grouped)
                    (let ((level (car group))
                          (group-entries (cdr group)))
                      (push (format "=== %s (%d) ==="
                                    (upcase level) (length group-entries))
                            parts)
                      (dolist (entry group-entries)
                        (push (claude-code-ide-extras-diagnostics--format-entry entry) parts))
                      (push "" parts)))
                  (push (format "Total: %d diagnostics" (length entries)) parts)
                  (string-join (nreverse parts) "\n"))))))
      (error (format "Error getting diagnostics: %s" (error-message-string err))))))

(defun claude-code-ide-extras-diagnostics--get-diagnostics-summary (&optional file-path)
  "Get a count summary of diagnostics for FILE-PATH or all buffers.
Returns counts per severity level and total."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (if file-path
            ;; Single file mode
            (let ((buffer (claude-code-ide-extras-diagnostics--get-buffer-for-file file-path)))
              (if (not buffer)
                  (format "Error: Cannot find buffer for %s" file-path)
                (let ((entries (claude-code-ide-extras-diagnostics--collect buffer)))
                  (if (not entries)
                      (format "No diagnostics for %s (no active checker)" file-path)
                    (let ((counts (claude-code-ide-extras-diagnostics--count-by-level entries))
                          (parts (list (format "Diagnostics summary for %s:" file-path))))
                      (dolist (count counts)
                        (push (format "  %s: %d" (car count) (cdr count)) parts))
                      (push (format "  total: %d" (length entries)) parts)
                      (string-join (nreverse parts) "\n"))))))
          ;; All buffers mode
          (let ((all-entries nil)
                (buffer-count 0))
            (dolist (buf (buffer-list))
              (when (buffer-live-p buf)
                (let ((entries (claude-code-ide-extras-diagnostics--collect buf)))
                  (when entries
                    (cl-incf buffer-count)
                    (setq all-entries (append all-entries entries))))))
            (if (not all-entries)
                "No diagnostics found in any buffer."
              (let ((counts (claude-code-ide-extras-diagnostics--count-by-level all-entries))
                    (parts (list (format "Diagnostics summary across %d buffer(s):" buffer-count))))
                (dolist (count counts)
                  (push (format "  %s: %d" (car count) (cdr count)) parts))
                (push (format "  total: %d" (length all-entries)) parts)
                (string-join (nreverse parts) "\n")))))
      (error (format "Error getting diagnostics summary: %s" (error-message-string err))))))

(defun claude-code-ide-extras-diagnostics--recheck-diagnostics (&optional file-path)
  "Trigger a re-check of diagnostics for FILE-PATH or the current buffer.
Forces flymake or flycheck to re-run its checkers."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((buffer (claude-code-ide-extras-diagnostics--get-buffer-for-file file-path)))
          (if (not buffer)
              (format "Error: Cannot find buffer for %s" (or file-path "current buffer"))
            (with-current-buffer buffer
              (cond
               ((bound-and-true-p flymake-mode)
                (flymake-start)
                (format "Flymake recheck triggered for %s"
                        (or file-path (buffer-name buffer))))
               ((bound-and-true-p flycheck-mode)
                (flycheck-buffer)
                (format "Flycheck recheck triggered for %s"
                        (or file-path (buffer-name buffer))))
               (t
                (format "No diagnostic checker active in %s. Neither flymake nor flycheck is enabled."
                        (or file-path (buffer-name buffer))))))))
      (error (format "Error rechecking diagnostics: %s" (error-message-string err))))))

(defun claude-code-ide-extras-diagnostics--get-compilation-errors (&optional buffer-name)
  "Parse errors from a compilation buffer.
BUFFER-NAME defaults to \"*compilation*\".
Returns structured error locations from Emacs' compilation-mode parser."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((buf-name (or buffer-name "*compilation*"))
               (buffer (get-buffer buf-name)))
          (if (not buffer)
              (format "No buffer named %s found. Available compilation buffers: %s"
                      buf-name
                      (mapconcat #'buffer-name
                                 (seq-filter
                                  (lambda (b)
                                    (with-current-buffer b
                                      (derived-mode-p 'compilation-mode)))
                                  (buffer-list))
                                 ", "))
            (with-current-buffer buffer
              (unless (derived-mode-p 'compilation-mode)
                (error "Buffer %s is not a compilation buffer" buf-name))
              ;; Force compilation-mode to parse errors
              (compilation--ensure-parse (point-max))
              (let ((errors '())
                    (warnings '())
                    (infos '()))
                (save-excursion
                  (goto-char (point-min))
                  (while (not (eobp))
                    (let ((msg (get-text-property (point) 'compilation-message)))
                      (when msg
                        (let* ((loc (compilation--message->loc msg))
                               (type (compilation--message->type msg))
                               (line-text (buffer-substring-no-properties
                                           (line-beginning-position) (line-end-position)))
                               (file (when loc
                                       (let ((fs (compilation--loc->file-struct loc)))
                                         (when fs
                                           (car (compilation--file-struct->file-spec fs))))))
                               (line (when loc (compilation--loc->line loc)))
                               (col (when loc (compilation--loc->col loc)))
                               (entry (format "%s:%s:%s: %s"
                                              (or file "?")
                                              (or line "?")
                                              (or col "?")
                                              (string-trim line-text))))
                          (cond
                           ((>= type 2) (push entry errors))
                           ((= type 1) (push entry warnings))
                           (t (push entry infos))))))
                    (forward-line 1)))
                (let ((sections '()))
                  (when errors
                    (push (format "=== ERRORS (%d) ===\n%s"
                                  (length errors)
                                  (mapconcat #'identity (nreverse errors) "\n"))
                          sections))
                  (when warnings
                    (push (format "=== WARNINGS (%d) ===\n%s"
                                  (length warnings)
                                  (mapconcat #'identity (nreverse warnings) "\n"))
                          sections))
                  (when infos
                    (push (format "=== INFO (%d) ===\n%s"
                                  (length infos)
                                  (mapconcat #'identity (nreverse infos) "\n"))
                          sections))
                  (if sections
                      (format "Compilation errors from %s:\n\n%s\n\nTotal: %d errors, %d warnings, %d info"
                              buf-name
                              (mapconcat #'identity (nreverse sections) "\n\n")
                              (length errors) (length warnings) (length infos))
                    (format "No errors found in %s." buf-name)))))))
      (error (format "Error parsing compilation buffer: %s" (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-diagnostics-setup ()
  "Register all diagnostics MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-diagnostics--get-diagnostics
   :name claude-code-ide-extras-diagnostics-get-diagnostics-tool-name
   :description "Get all diagnostics (errors, warnings, info/notes) for a file or the current buffer. Tries flymake first (built-in), then flycheck. Returns formatted diagnostics grouped by severity level in format: file:line:col: level: message (checker)."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to get diagnostics for. If omitted, uses the current buffer."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-diagnostics--get-diagnostics-summary
   :name claude-code-ide-extras-diagnostics-get-diagnostics-summary-tool-name
   :description "Get a count summary of diagnostics across all buffers or for a specific file. Returns counts per severity level (error, warning, info/note) and total."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to summarize diagnostics for. If omitted, summarizes across all buffers."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-diagnostics--recheck-diagnostics
   :name claude-code-ide-extras-diagnostics-recheck-diagnostics-tool-name
   :description "Trigger a re-check of diagnostics (flymake or flycheck) for a file or the current buffer. Use after making changes to get fresh diagnostics."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to recheck. If omitted, rechecks the current buffer."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-diagnostics--get-compilation-errors
   :name claude-code-ide-extras-diagnostics-get-compilation-errors-tool-name
   :description "Parse and return structured errors from a compilation buffer (e.g. *compilation*, *grep*). Uses Emacs' compilation-mode parser to extract file, line, column, and error text."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the compilation buffer to parse (default \"*compilation*\")."
            :optional t)))

  (message "Claude Code IDE Extras: Diagnostics tools registered"))

(provide 'claude-code-ide-extras-diagnostics)
;;; claude-code-ide-extras-diagnostics.el ends here
