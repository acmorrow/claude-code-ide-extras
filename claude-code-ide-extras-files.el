;;; claude-code-ide-extras-files.el --- File system metadata MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;;         Tim Ransom
;; Keywords: tools, files, ai, claude, mcp
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

;; This package provides file system metadata MCP (Model Context Protocol)
;; tools for claude-code-ide.el, enabling Claude to:
;;
;; - Get metadata about files without loading them into buffers
;; - List directory contents with metadata
;; - Find files matching patterns in directory trees
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-files)
;;   (claude-code-ide-extras-files-setup)

;;; Code:

(require 'claude-code-ide)

(defgroup claude-code-ide-extras-files nil
  "File system metadata MCP tools for claude-code-ide."
  :group 'files
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-files-")

(defconst claude-code-ide-extras-files-version "0.0.4"
  "Version of claude-code-ide-extras-files.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-files-file-stat-tool-name
  "claude-code-ide-extras-files/file_stat"
  "MCP tool name for file_stat.")

(defconst claude-code-ide-extras-files-list-directory-tool-name
  "claude-code-ide-extras-files/list_directory"
  "MCP tool name for list_directory.")

(defconst claude-code-ide-extras-files-find-files-tool-name
  "claude-code-ide-extras-files/find_files"
  "MCP tool name for find_files.")

;;; Customization

(defcustom claude-code-ide-extras-files-file-stat-usage-prompt
  "Get metadata about a file without loading it into a buffer. Returns file name, directory, size, permissions, modification time, type, and symlink target if applicable."
  "Usage guidance for the file_stat MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-files)

(put 'claude-code-ide-extras-files-file-stat-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-files-file-stat-tool-name)

(defcustom claude-code-ide-extras-files-list-directory-usage-prompt
  "List contents of a directory with metadata. Optional glob pattern to filter entries. Optional recursive mode to traverse subdirectories (results are capped to avoid overwhelming output)."
  "Usage guidance for the list_directory MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-files)

(put 'claude-code-ide-extras-files-list-directory-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-files-list-directory-tool-name)

(defcustom claude-code-ide-extras-files-find-files-usage-prompt
  "Find files matching a regex pattern in a directory tree. Returns matching file paths with basic metadata. Results are capped at max_results (default 50)."
  "Usage guidance for the find_files MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-files)

(put 'claude-code-ide-extras-files-find-files-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-files-find-files-tool-name)

;;; Internal helpers

(defun claude-code-ide-extras-files--format-mode (mode)
  "Format file MODE integer as an rwx permission string."
  (let ((result (make-string 9 ?-)))
    (when (> (logand mode #o400) 0) (aset result 0 ?r))
    (when (> (logand mode #o200) 0) (aset result 1 ?w))
    (when (> (logand mode #o100) 0) (aset result 2 ?x))
    (when (> (logand mode #o040) 0) (aset result 3 ?r))
    (when (> (logand mode #o020) 0) (aset result 4 ?w))
    (when (> (logand mode #o010) 0) (aset result 5 ?x))
    (when (> (logand mode #o004) 0) (aset result 6 ?r))
    (when (> (logand mode #o002) 0) (aset result 7 ?w))
    (when (> (logand mode #o001) 0) (aset result 8 ?x))
    result))

(defun claude-code-ide-extras-files--format-size (size)
  "Format SIZE in bytes as a human-readable string."
  (if (fboundp 'file-size-human-readable)
      (file-size-human-readable size)
    (cond
     ((>= size (* 1024 1024 1024))
      (format "%.1fG" (/ size (* 1024.0 1024 1024))))
     ((>= size (* 1024 1024))
      (format "%.1fM" (/ size (* 1024.0 1024))))
     ((>= size 1024)
      (format "%.1fk" (/ size 1024.0)))
     (t (format "%d" size)))))

(defun claude-code-ide-extras-files--file-type-string (attrs)
  "Return a string describing the file type from ATTRS.
ATTRS is the result of `file-attributes'."
  (let ((type (file-attribute-type attrs)))
    (cond
     ((eq type t) "directory")
     ((stringp type) "symlink")
     (t "file"))))

(defun claude-code-ide-extras-files--format-entry (path)
  "Format a single directory entry at PATH with metadata."
  (let ((attrs (file-attributes path)))
    (if (not attrs)
        (format "  %s  (unreadable)" (file-name-nondirectory path))
      (let ((type (claude-code-ide-extras-files--file-type-string attrs))
            (size (file-attribute-size attrs))
            (mtime (file-attribute-modification-time attrs)))
        (format "  %-40s  %-9s  %8s  %s"
                (file-name-nondirectory path)
                type
                (if (string= type "directory") "-" (claude-code-ide-extras-files--format-size size))
                (format-time-string "%Y-%m-%d %H:%M:%S" mtime))))))

;;; Tool implementations

(defun claude-code-ide-extras-files--file-stat (file-path)
  "Get metadata about FILE-PATH without loading it into a buffer."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((path (expand-file-name file-path)))
          (unless (file-exists-p path)
            (error "File does not exist: %s" path))
          (unless (file-readable-p path)
            (error "File is not readable: %s" path))
          (let* ((attrs (file-attributes path))
                 (type (claude-code-ide-extras-files--file-type-string attrs))
                 (size (file-attribute-size attrs))
                 (mtime (file-attribute-modification-time attrs))
                 (modes (file-modes path))
                 (uid (file-attribute-user-id attrs))
                 (gid (file-attribute-group-id attrs))
                 (link-target (file-attribute-type attrs)))
            (concat
             (format "File: %s\n" (file-name-nondirectory path))
             (format "Directory: %s\n" (file-name-directory path))
             (format "Type: %s\n" type)
             (format "Size: %s (%d bytes)\n" (claude-code-ide-extras-files--format-size size) size)
             (format "Permissions: %s (%04o)\n" (claude-code-ide-extras-files--format-mode modes) modes)
             (format "Modified: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S %Z" mtime))
             (format "Owner: %s\n" (if (stringp uid) uid (format "%d" uid)))
             (format "Group: %s" (if (stringp gid) gid (format "%d" gid)))
             (when (stringp link-target)
               (format "\nSymlink target: %s" link-target)))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-files--list-directory (directory &optional pattern recursive)
  "List contents of DIRECTORY with metadata.
Optional PATTERN is a glob pattern to filter entries (e.g. \"*.el\").
Optional RECURSIVE when \"true\" traverses subdirectories."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((dir (expand-file-name directory)))
          (unless (file-directory-p dir)
            (error "Not a directory: %s" dir))
          (unless (file-readable-p dir)
            (error "Directory is not readable: %s" dir))
          (let* ((max-results 500)
                 (is-recursive (and recursive (string= recursive "true")))
                 (entries
                  (if is-recursive
                      (let* ((regexp (if (and pattern (not (string-empty-p pattern)))
                                         (wildcard-to-regexp pattern)
                                       ""))
                             (files (directory-files-recursively dir regexp nil)))
                        (seq-take files max-results))
                    (let* ((glob (or pattern nil))
                           (files (if glob
                                      (file-expand-wildcards
                                       (expand-file-name glob dir) t)
                                    (directory-files dir t nil t))))
                      ;; Remove . and .. entries
                      (seq-remove
                       (lambda (f)
                         (member (file-name-nondirectory f) '("." "..")))
                       files))))
                 (count (length entries))
                 (header (format "Directory: %s\nPattern: %s\nRecursive: %s\nEntries: %d%s\n\n%-42s  %-9s  %8s  %s\n%s\n"
                                 dir
                                 (or pattern "*")
                                 (if is-recursive "yes" "no")
                                 count
                                 (if (and is-recursive (>= count max-results))
                                     (format " (capped at %d)" max-results)
                                   "")
                                 "Name" "Type" "Size" "Modified"
                                 (make-string 80 ?-))))
            (concat header
                    (mapconcat #'claude-code-ide-extras-files--format-entry entries "\n"))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-files--find-files (directory pattern &optional max-results)
  "Find files matching PATTERN in DIRECTORY tree.
PATTERN is a regular expression to match against file names.
Optional MAX-RESULTS caps the number of results (default 50)."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((dir (expand-file-name directory))
               (limit (or (and max-results (truncate max-results)) 50))
               (results '())
               (count 0))
          (unless (file-directory-p dir)
            (error "Not a directory: %s" dir))
          (unless (file-readable-p dir)
            (error "Directory is not readable: %s" dir))
          (let ((all-files (directory-files-recursively dir pattern nil)))
            (setq results (seq-take all-files limit))
            (setq count (length results))
            (let ((total (length all-files)))
              (concat
               (format "Search directory: %s\nPattern: %s\nResults: %d%s\n\n%-42s  %8s  %s\n%s\n"
                       dir
                       pattern
                       count
                       (if (> total limit)
                           (format " of %d (capped at %d)" total limit)
                         "")
                       "Path" "Size" "Modified"
                       (make-string 70 ?-))
               (mapconcat
                (lambda (path)
                  (let ((attrs (file-attributes path)))
                    (if (not attrs)
                        (format "  %s  (unreadable)" path)
                      (let ((size (file-attribute-size attrs))
                            (mtime (file-attribute-modification-time attrs)))
                        (format "  %-40s  %8s  %s"
                                (file-relative-name path dir)
                                (claude-code-ide-extras-files--format-size size)
                                (format-time-string "%Y-%m-%d %H:%M:%S" mtime))))))
                results "\n")))))
      (error (format "Error: %s" (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-files-setup ()
  "Register all file system metadata MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-files--file-stat
   :name claude-code-ide-extras-files-file-stat-tool-name
   :description "Get metadata about a file without loading it into a buffer. Returns file name, directory, size, permissions, modification time, type (file/directory/symlink), and symlink target if applicable."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to inspect.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-files--list-directory
   :name claude-code-ide-extras-files-list-directory-tool-name
   :description "List contents of a directory with metadata (name, type, size, modification time). Optional glob pattern to filter entries. Optional recursive mode to traverse subdirectories."
   :args '((:name "directory"
            :type string
            :description "Absolute path to the directory to list.")
           (:name "pattern"
            :type string
            :description "Optional glob pattern to filter entries (e.g. \"*.el\", \"*.org\")."
            :optional t)
           (:name "recursive"
            :type string
            :description "Set to \"true\" to recursively list subdirectories. Results are capped to avoid overwhelming output."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-files--find-files
   :name claude-code-ide-extras-files-find-files-tool-name
   :description "Find files matching a regex pattern in a directory tree. Returns matching file paths with basic metadata (size, modification time). Results are capped at max_results."
   :args '((:name "directory"
            :type string
            :description "Absolute path to the directory to search in.")
           (:name "pattern"
            :type string
            :description "Regular expression to match against file names.")
           (:name "max_results"
            :type number
            :description "Maximum number of results to return (default 50)."
            :optional t)))

  (message "Claude Code IDE Extras: file system metadata tools registered"))

(provide 'claude-code-ide-extras-files)
;;; claude-code-ide-extras-files.el ends here
