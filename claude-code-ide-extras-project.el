;;; claude-code-ide-extras-project.el --- project.el MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;;         Tim Ransom
;; Keywords: tools, project, ai, claude, mcp
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

;; This package provides project.el-specific MCP (Model Context Protocol) tools
;; for claude-code-ide.el, enabling Claude to:
;;
;; - Execute project builds/tests with buffer-based output
;; - Run arbitrary shell commands in project context
;; - Search and query compilation/shell output
;; - Discover project configuration (dir-locals)
;; - Enumerate project files
;;
;; This is the built-in alternative to claude-code-ide-extras-projectile (which
;; requires projectile).  project.el is included with Emacs 28+.
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-project)
;;   (claude-code-ide-extras-project-setup)

;;; Code:

(require 'project)
(require 'compile)
(require 'claude-code-ide)
(require 'claude-code-ide-extras-common)
(require 'seq)

(defgroup claude-code-ide-extras-project nil
  "project.el MCP tools for claude-code-ide."
  :group 'project
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-project-")

(defconst claude-code-ide-extras-project-version "0.0.4"
  "Version of claude-code-ide-extras-project.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-project-get-project-buffer-local-keys-tool-name
  "claude-code-ide-extras-project/get_project_buffer_local_keys"
  "MCP tool name for get_project_buffer_local_keys.")

(defconst claude-code-ide-extras-project-get-project-buffer-local-variables-tool-name
  "claude-code-ide-extras-project/get_project_buffer_local_variables"
  "MCP tool name for get_project_buffer_local_variables.")

(defconst claude-code-ide-extras-project-read-project-dir-locals-tool-name
  "claude-code-ide-extras-project/read_project_dir_locals"
  "MCP tool name for read_project_dir_locals.")

(defconst claude-code-ide-extras-project-task-start-tool-name
  "claude-code-ide-extras-project/task_start"
  "MCP tool name for task_start.")

(defconst claude-code-ide-extras-project-task-wait-tool-name
  "claude-code-ide-extras-project/task_wait"
  "MCP tool name for task_wait.")

(defconst claude-code-ide-extras-project-task-query-tool-name
  "claude-code-ide-extras-project/task_query"
  "MCP tool name for task_query.")

(defconst claude-code-ide-extras-project-task-kill-tool-name
  "claude-code-ide-extras-project/task_kill"
  "MCP tool name for task_kill.")

(defconst claude-code-ide-extras-project-task-search-tool-name
  "claude-code-ide-extras-project/task_search"
  "MCP tool name for task_search.")

(defconst claude-code-ide-extras-project-get-project-files-tool-name
  "claude-code-ide-extras-project/get_project_files"
  "MCP tool name for get_project_files.")

;;; Customization

(defcustom claude-code-ide-extras-project-get-project-buffer-local-keys-usage-prompt
  "List buffer-local variable names for a project. Returns only names (lightweight discovery). Optional filter_regex (Emacs regex) to narrow results. IMPORTANT: Even unfiltered, this is much cheaper than getting full variables. Use this for discovery, then get_project_buffer_local_variables with filter for specific values."
  "Usage guidance for the get_project_buffer_local_keys MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-get-project-buffer-local-keys-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-get-project-buffer-local-keys-tool-name)

(defcustom claude-code-ide-extras-project-get-project-buffer-local-variables-usage-prompt
  "Get buffer-local variables with values for a project. Optional filter_regex (Emacs regex) to limit results. WARNING: Without filtering, this can be very context-expensive (10k+ tokens). STRONGLY RECOMMENDED: Use filter_regex to get only relevant variables."
  "Usage guidance for the get_project_buffer_local_variables MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-get-project-buffer-local-variables-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-get-project-buffer-local-variables-tool-name)

(defcustom claude-code-ide-extras-project-read-project-dir-locals-usage-prompt
  "DEPRECATED: Use get_project_buffer_local_keys/get_project_buffer_local_variables instead. WARNING: This tool returns ALL buffer-local variables which can be very context-expensive (often 10k+ tokens)."
  "Usage guidance for the read_project_dir_locals MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-read-project-dir-locals-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-read-project-dir-locals-tool-name)

(defcustom claude-code-ide-extras-project-task-start-usage-prompt
  "Launches builds, tests, or commands in the project directory. Use 'run' type for arbitrary shell commands."
  "Usage guidance for the task_start MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-task-start-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-task-start-tool-name)

(defcustom claude-code-ide-extras-project-task-wait-usage-prompt
  "Poll for task completion before querying output."
  "Usage guidance for the task_wait MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-task-wait-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-task-wait-tool-name)

(defcustom claude-code-ide-extras-project-task-query-usage-prompt
  "Retrieve task output. Use line ranges for large results."
  "Usage guidance for the task_query MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-task-query-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-task-query-tool-name)

(defcustom claude-code-ide-extras-project-task-search-usage-prompt
  "Search task output for patterns. Useful for finding errors."
  "Usage guidance for the task_search MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-task-search-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-task-search-tool-name)

(defcustom claude-code-ide-extras-project-task-kill-usage-prompt
  "Terminate running tasks that hang or take too long."
  "Usage guidance for the task_kill MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-task-kill-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-task-kill-tool-name)

(defcustom claude-code-ide-extras-project-get-project-files-usage-prompt
  "Enumerate all files in the current project. Uses project.el's file discovery, respecting .gitignore rules."
  "Usage guidance for the get_project_files MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-project)

(put 'claude-code-ide-extras-project-get-project-files-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-project-get-project-files-tool-name)

;;; Internal helpers

(defun claude-code-ide-extras-project--find-root (file-path)
  "Find the project root for FILE-PATH using project.el.
Returns the project root directory or nil."
  (let* ((default-directory (file-name-directory file-path))
         (proj (project-current nil default-directory)))
    (when proj
      (project-root proj))))

;;; Tool implementations

(defun claude-code-ide-extras-project--get-project-buffer-local-keys (file-path &optional filter-regex)
  "Get buffer-local variable names for the project containing FILE-PATH.
Finds the project root and returns list of buffer-local variable names.
Optional FILTER-REGEX (Emacs regex) filters the returned names."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((project-root (claude-code-ide-extras-project--find-root file-path)))
          (unless project-root
            (error "No project root found for file: %s" file-path))
          (unless (file-in-directory-p file-path project-root)
            (error "File %s is not under project root %s" file-path project-root))
          (let ((probe-file (expand-file-name ".dir-locals-probe" project-root)))
            (claude-code-ide-extras-common--get-buffer-local-keys probe-file filter-regex)))
      (error (format "Error reading project buffer-local keys: %s" (error-message-string err))))))

(defun claude-code-ide-extras-project--get-project-buffer-local-variables (file-path &optional filter-regex)
  "Get buffer-local variables with values for the project containing FILE-PATH.
Finds the project root and returns buffer-local-variables as a Lisp form.
Optional FILTER-REGEX (Emacs regex) filters variables by name before retrieving values."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((project-root (claude-code-ide-extras-project--find-root file-path)))
          (unless project-root
            (error "No project root found for file: %s" file-path))
          (unless (file-in-directory-p file-path project-root)
            (error "File %s is not under project root %s" file-path project-root))
          (let ((probe-file (expand-file-name ".dir-locals-probe" project-root)))
            (claude-code-ide-extras-common--get-buffer-local-variables probe-file filter-regex)))
      (error (format "Error reading project buffer-local variables: %s" (error-message-string err))))))

(defun claude-code-ide-extras-project--read-project-dir-locals (file-path)
  "Read effective dir-local variables for the project containing FILE-PATH.
DEPRECATED: Delegates to get-project-buffer-local-variables for compatibility."
  (claude-code-ide-extras-project--get-project-buffer-local-variables file-path nil))

(defun claude-code-ide-extras-project--task-start (task-type command file-path)
  "Start a project task (compile, test, configure, install, package, run).
Returns the compilation buffer name for later querying.

TASK-TYPE is one of: compile, test, configure, install, package, run.
COMMAND is the shell command to execute (required).
FILE-PATH is used to determine which project to operate on."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((project-root (claude-code-ide-extras-project--find-root file-path)))
      (if (not project-root)
          (format "Error: %s is not in a recognized project" file-path)
        (let ((valid-types '("compile" "test" "configure" "install" "package" "run")))
          (if (not (member task-type valid-types))
              (format "Error: Unknown task-type '%s'. Must be one of: %s"
                      task-type (string-join valid-types ", "))
            (let* ((default-directory project-root)
                   (compilation-read-command nil)
                   ;; Use project-prefixed buffer name for per-project isolation
                   (buffer-name (project-prefixed-buffer-name
                                 (format "compilation-%s" task-type)))
                   (compile-command command))
              (compile command)
              (format "Started %s in buffer: %s" task-type buffer-name))))))))

(defun claude-code-ide-extras-project--task-wait (buffer-name)
  "Check if compilation is finished and return size info when done.

BUFFER-NAME is the name of the compilation buffer to check.

Returns \\='running if still executing, or \\='finished with output size
\(lines and chars) when complete."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((buf (get-buffer buffer-name)))
      (if (not buf)
          (format "Error: Buffer not found: %s" buffer-name)
        (with-current-buffer buf
          (if (and (get-buffer-process buf)
                   (process-live-p (get-buffer-process buf)))
              (format "Status: running")
            (let* ((line-count (count-lines (point-min) (point-max)))
                   (char-count (- (point-max) (point-min))))
              (format "Status: finished\n\nOutput size:\n  Lines: %d\n  Characters: %d"
                      line-count char-count))))))))

(defun claude-code-ide-extras-project--task-query (buffer-name &optional start-line num-lines)
  "Retrieve output from a finished compilation buffer.

BUFFER-NAME is the name of the compilation buffer to query.
Optional START-LINE is the first line to retrieve (1-based, negative
counts from end).
Optional NUM-LINES is the number of lines to retrieve."
  (claude-code-ide-mcp-server-with-session-context nil
    (claude-code-ide-extras-common--buffer-query buffer-name start-line num-lines)))

(defun claude-code-ide-extras-project--task-kill (buffer-name)
  "Kill a running compilation in the specified buffer.
BUFFER-NAME is the name of the compilation buffer to kill.
Returns a status message."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((buf (get-buffer buffer-name)))
      (if (not buf)
          (format "Error: Buffer not found: %s" buffer-name)
        (with-current-buffer buf
          (if (not (and (get-buffer-process buf)
                        (process-live-p (get-buffer-process buf))))
              (format "No compilation running in buffer: %s" buffer-name)
            (kill-compilation)
            (format "Killed compilation in buffer: %s" buffer-name)))))))

(defun claude-code-ide-extras-project--task-search (buffer-name pattern &optional context-lines)
  "Search project task/compilation output for PATTERN.
BUFFER-NAME is the compilation buffer name (from task_start).
PATTERN is a regular expression to search for.
CONTEXT-LINES specifies number of lines before/after each match (default 0)."
  (claude-code-ide-mcp-server-with-session-context nil
    (claude-code-ide-extras-common--buffer-search buffer-name pattern context-lines)))

(defun claude-code-ide-extras-project--get-project-files (file-path)
  "Enumerate all files in the project containing FILE-PATH.

Uses project.el's file discovery. Returns files as a list of absolute paths.
The file list respects .gitignore rules."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((default-directory (file-name-directory file-path))
               (proj (project-current nil default-directory)))
          (if (not proj)
              (format "Error: %s is not in a recognized project" file-path)
            (project-files proj)))
      (error (format "Error getting project files: %s" (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-project-setup ()
  "Register all project.el MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--get-project-buffer-local-keys
   :name claude-code-ide-extras-project-get-project-buffer-local-keys-tool-name
   :description "Get buffer-local variable names for the project root containing a file. Returns only names (lightweight discovery). Optional filter_regex (Emacs regex) to narrow results."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project. Project root will be determined automatically.")
           (:name "filter_regex"
            :type string
            :description "Optional Emacs regular expression to filter variable names."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--get-project-buffer-local-variables
   :name claude-code-ide-extras-project-get-project-buffer-local-variables-tool-name
   :description "Get buffer-local variables with values for the project root containing a file. Optional filter_regex (Emacs regex) to limit results. WARNING: Without filtering, this can be very context-expensive (10k+ tokens)."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project. Project root will be determined automatically.")
           (:name "filter_regex"
            :type string
            :description "Optional Emacs regular expression to filter variables by name."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--read-project-dir-locals
   :name claude-code-ide-extras-project-read-project-dir-locals-tool-name
   :description "DEPRECATED: Use get_project_buffer_local_keys/get_project_buffer_local_variables instead. Returns ALL buffer-local-variables as a Lisp form. WARNING: Very context-expensive."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--task-start
   :name claude-code-ide-extras-project-task-start-tool-name
   :description "Start a project task (compile, test, configure, install, package, run). Non-blocking - returns immediately with the compilation buffer name. Use task_wait to poll for completion, then task_query to retrieve output."
   :args '((:name "task_type"
            :type string
            :description "The type of task to run: compile, test, configure, install, package, or run")
           (:name "command"
            :type string
            :description "The shell command to execute for this task.")
           (:name "file_path"
            :type string
            :description "Absolute path to a file in the project (used to determine which project to operate on).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--task-wait
   :name claude-code-ide-extras-project-task-wait-tool-name
   :description "Poll for project task completion and get output size. Returns 'running' if still executing, or 'finished' with line/character count when done."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to check (returned by task_start).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--task-query
   :name claude-code-ide-extras-project-task-query-tool-name
   :description "Retrieve compilation output from a finished task. Returns full output if no range specified, or limited output if start_line and num_lines are provided."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to query (returned by task_start).")
           (:name "start_line"
            :type number
            :description "First line to retrieve (1-based, negative counts from end). Must be provided with num_lines."
            :optional t)
           (:name "num_lines"
            :type number
            :description "Number of lines to retrieve starting from start_line. Must be provided with start_line."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--task-kill
   :name claude-code-ide-extras-project-task-kill-tool-name
   :description "Kill a running compilation in the specified buffer."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to kill.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--task-search
   :name claude-code-ide-extras-project-task-search-tool-name
   :description "Search for a pattern in project task/compilation output. Returns matching lines with optional context."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to search (returned by task_start).")
           (:name "pattern"
            :type string
            :description "Regular expression pattern to search for.")
           (:name "context_lines"
            :type number
            :description "Number of context lines to show before and after each match (optional, default 0)."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-project--get-project-files
   :name claude-code-ide-extras-project-get-project-files-tool-name
   :description "Enumerate all files in the project containing the given file. Returns a list of file paths. Uses project.el's file discovery, respecting .gitignore rules."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project. Project root will be determined automatically.")))

  (message "Claude Code IDE Extras: project.el tools registered"))

(provide 'claude-code-ide-extras-project)
;;; claude-code-ide-extras-project.el ends here
