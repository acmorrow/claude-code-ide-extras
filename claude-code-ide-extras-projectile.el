;;; claude-code-ide-extras-projectile.el --- Projectile MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;; Keywords: tools, projectile, ai, claude, mcp
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

;; This package provides Projectile-specific MCP (Model Context Protocol) tools
;; for claude-code-ide.el, enabling Claude to:
;;
;; - Execute project builds/tests with buffer-based output
;; - Run arbitrary shell commands in project context
;; - Search and query compilation/shell output
;; - Discover project configuration (dir-locals)
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-projectile)
;;   (claude-code-ide-extras-projectile-setup)

;;; Code:

(require 'projectile)
(require 'claude-code-ide)
(require 'claude-code-ide-extras-common)
(require 'seq)

(defgroup claude-code-ide-extras-projectile nil
  "Projectile MCP tools for claude-code-ide."
  :group 'projectile
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-projectile-")

(defconst claude-code-ide-extras-projectile-version "0.0.6"
  "Version of claude-code-ide-extras-projectile.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-projectile-read-project-dir-locals-tool-name
  "claude-code-ide-extras-projectile/read_project_dir_locals"
  "MCP tool name for read_project_dir_locals.")

(defconst claude-code-ide-extras-projectile-get-project-buffer-local-keys-tool-name
  "claude-code-ide-extras-projectile/get_project_buffer_local_keys"
  "MCP tool name for get_project_buffer_local_keys.")

(defconst claude-code-ide-extras-projectile-get-project-buffer-local-variables-tool-name
  "claude-code-ide-extras-projectile/get_project_buffer_local_variables"
  "MCP tool name for get_project_buffer_local_variables.")

(defconst claude-code-ide-extras-projectile-task-start-tool-name
  "claude-code-ide-extras-projectile/task_start"
  "MCP tool name for task_start.")

(defconst claude-code-ide-extras-projectile-task-wait-tool-name
  "claude-code-ide-extras-projectile/task_wait"
  "MCP tool name for task_wait.")

(defconst claude-code-ide-extras-projectile-task-query-tool-name
  "claude-code-ide-extras-projectile/task_query"
  "MCP tool name for task_query.")

(defconst claude-code-ide-extras-projectile-task-kill-tool-name
  "claude-code-ide-extras-projectile/task_kill"
  "MCP tool name for task_kill.")

(defconst claude-code-ide-extras-projectile-task-search-tool-name
  "claude-code-ide-extras-projectile/task_search"
  "MCP tool name for task_search.")

(defconst claude-code-ide-extras-projectile-get-project-files-tool-name
  "claude-code-ide-extras-projectile/get_project_files"
  "MCP tool name for get_project_files.")

(defconst claude-code-ide-extras-projectile-get-project-buffers-tool-name
  "claude-code-ide-extras-projectile/get_project_buffers"
  "MCP tool name for get_project_buffers.")

;;; Customization

(defcustom claude-code-ide-extras-projectile-read-project-dir-locals-usage-prompt
  "DEPRECATED: Use get_project_buffer_local_keys/get_project_buffer_local_variables instead. WARNING: This tool returns ALL buffer-local variables which can be very context-expensive (often 10k+ tokens). The new tools support filtering and discovery patterns."
  "Usage guidance for the read_project_dir_locals MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-read-project-dir-locals-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-read-project-dir-locals-tool-name)

(defcustom claude-code-ide-extras-projectile-get-project-buffer-local-keys-usage-prompt
  "List buffer-local variable names for a project. Returns only names (lightweight discovery). Optional filter_regex (Emacs regex) to narrow results. IMPORTANT: Even unfiltered, this is much cheaper than getting full variables. Use this for discovery, then get_project_buffer_local_variables with filter for specific values."
  "Usage guidance for the get_project_buffer_local_keys MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-get-project-buffer-local-keys-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-get-project-buffer-local-keys-tool-name)

(defcustom claude-code-ide-extras-projectile-get-project-buffer-local-variables-usage-prompt
  "Get buffer-local variables with values for a project. Optional filter_regex (Emacs regex) to limit results. WARNING: Without filtering, this can be very context-expensive (10k+ tokens). STRONGLY RECOMMENDED: Use filter_regex to get only relevant variables (e.g., \"^projectile-\" or \"^\\\\(projectile\\\\|lsp\\\\)-\"). Pattern: discover with get_project_buffer_local_keys first, then retrieve filtered values."
  "Usage guidance for the get_project_buffer_local_variables MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-get-project-buffer-local-variables-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-get-project-buffer-local-variables-tool-name)

(defcustom claude-code-ide-extras-projectile-task-start-usage-prompt
  "Launches builds, tests, or commands. Use 'run' type for arbitrary shell commands."
  "Usage guidance for the task_start MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-task-start-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-task-start-tool-name)

(defcustom claude-code-ide-extras-projectile-task-wait-usage-prompt
  "Poll for task completion before querying output."
  "Usage guidance for the task_wait MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-task-wait-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-task-wait-tool-name)

(defcustom claude-code-ide-extras-projectile-task-query-usage-prompt
  "Retrieve task output. Use line ranges for large results."
  "Usage guidance for the task_query MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-task-query-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-task-query-tool-name)

(defcustom claude-code-ide-extras-projectile-task-search-usage-prompt
  "Search task output for patterns. Useful for finding errors."
  "Usage guidance for the task_search MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-task-search-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-task-search-tool-name)

(defcustom claude-code-ide-extras-projectile-task-kill-usage-prompt
  "Terminate running tasks that hang or take too long."
  "Usage guidance for the task_kill MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-task-kill-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-task-kill-tool-name)

(defcustom claude-code-ide-extras-projectile-read-project-dir-locals-usage-prompt
  "Read project-wide configuration. Check for build commands and project settings."
  "Usage guidance for the read_project_dir_locals MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-read-project-dir-locals-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-read-project-dir-locals-tool-name)

(defcustom claude-code-ide-extras-projectile-get-project-files-usage-prompt
  "Enumerate all files in the current project. Fast using projectile cache. Optional filter_regex (Emacs regex) to filter by file path."
  "Usage guidance for the get_project_files MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-get-project-files-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-get-project-files-tool-name)

(defcustom claude-code-ide-extras-projectile-get-project-buffers-usage-prompt
  "List open buffers belonging to the current project. Returns name, mode, file, and modified status. Optional filter_regex (name), mode_filter (major mode), and files_only (file-visiting buffers only)."
  "Usage guidance for the get_project_buffers MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-projectile)

(put 'claude-code-ide-extras-projectile-get-project-buffers-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-projectile-get-project-buffers-tool-name)

;;; Tool implementations

  ;; Custom MCP tools for reading project dir-locals
  (defun claude-code-ide-extras-projectile--get-project-buffer-local-keys (file-path &optional filter-regex)
    "Get buffer-local variable names for the project containing FILE-PATH.
Finds the project root and returns list of buffer-local variable names.
Optional FILTER-REGEX (Emacs regex) filters the returned names."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((default-directory (file-name-directory file-path))
                 (project-root (or (projectile-project-root)
                                   (when-let* ((proj (project-current)))
                                     (project-root proj)))))
            ;; Validate project root exists
            (unless project-root
              (error "No project root found for file: %s" file-path))
            ;; Validate file is under project root
            (unless (file-in-directory-p file-path project-root)
              (error "File %s is not under project root %s" file-path project-root))
            ;; Use a probe file in project root to trigger dir-locals loading
            (let ((probe-file (expand-file-name ".dir-locals-probe" project-root)))
              ;; Delegate to the common implementation which handles buffer management
              (claude-code-ide-extras-common--get-buffer-local-keys probe-file filter-regex)))
        (error (format "Error reading project buffer-local keys: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-projectile--get-project-buffer-local-variables (file-path &optional filter-regex)
    "Get buffer-local variables with values for the project containing FILE-PATH.
Finds the project root and returns buffer-local-variables as a Lisp form.
Optional FILTER-REGEX (Emacs regex) filters variables by name before
retrieving values."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((default-directory (file-name-directory file-path))
                 (project-root (or (projectile-project-root)
                                   (when-let* ((proj (project-current)))
                                     (project-root proj)))))
            ;; Validate project root exists
            (unless project-root
              (error "No project root found for file: %s" file-path))
            ;; Validate file is under project root
            (unless (file-in-directory-p file-path project-root)
              (error "File %s is not under project root %s" file-path project-root))
            ;; Use a probe file in project root to trigger dir-locals loading
            (let ((probe-file (expand-file-name ".dir-locals-probe" project-root)))
              ;; Delegate to the common implementation which handles buffer management
              (claude-code-ide-extras-common--get-buffer-local-variables probe-file filter-regex)))
        (error (format "Error reading project buffer-local variables: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-projectile--read-project-dir-locals (file-path)
    "Read effective dir-local variables for the project containing FILE-PATH.
DEPRECATED: Delegates to get-project-buffer-local-variables for compatibility."
    ;; Simply delegate to the new function without filtering
    (claude-code-ide-extras-projectile--get-project-buffer-local-variables file-path nil))

  ;; Custom MCP tools for projectile task management (split architecture)

  ;; Tool 1: Start a projectile task (non-blocking)
  (defun claude-code-ide-extras-projectile--task-start (task-type command file-path)
    "Start a projectile task (compile, test, configure, install, package, run).
Returns the compilation buffer name for later querying.

TASK-TYPE is one of: compile, test, configure, install, package, run.
COMMAND is the shell command to execute (required).
FILE-PATH is used to determine which project to operate on."
    (claude-code-ide-mcp-server-with-session-context nil
      ;; Compilation buffers must be qualified by project, or two Claude sessions working in
      ;; different projects end up sharing one buffer and destroying each other's output.
      ;; Ask `projectile-compilation-buffer-scope' the function rather than reading the
      ;; variable of the same name: the function normalizes the t shorthand and folds in the
      ;; two obsolete booleans it replaced, so every spelling a user might have configured is
      ;; accepted here and no deprecated variable is named.
      (if (not (memq 'project (projectile-compilation-buffer-scope)))
          "Error: compilation buffers must be qualified by project for safe concurrent compilation. Add 'project to projectile-compilation-buffer-scope, e.g. (setq projectile-compilation-buffer-scope '(project)) or '(project command)."
        ;; Determine project from file-path
        (let* ((default-directory (file-name-directory file-path))
               (project-root (projectile-project-root)))
          (if (not project-root)
              (format "Error: %s is not in a projectile project" file-path)
            ;; Determine the task function and command map
            (let* ((task-info (pcase task-type
                               ("compile" (cons #'projectile-compile-project projectile-compilation-cmd-map))
                               ("test" (cons #'projectile-test-project projectile-test-cmd-map))
                               ("configure" (cons #'projectile-configure-project projectile-configure-cmd-map))
                               ("install" (cons #'projectile-install-project projectile-install-cmd-map))
                               ("package" (cons #'projectile-package-project projectile-package-cmd-map))
                               ("run" (cons #'projectile-run-project projectile-run-cmd-map))
                               (_ nil)))
                   (task-function (car task-info))
                   (command-map (cdr task-info))
                   (compilation-read-command nil) ;; Disable prompting
                   (compilation-dir (projectile-compilation-dir)))
              (if (not task-function)
                  (format "Error: Unknown task-type '%s'. Must be one of: compile, test, configure, install, package, run" task-type)
                ;; Cache the command in projectile's map
                (when command
                  (puthash compilation-dir command command-map))
                ;; Observe the buffer projectile creates rather than predicting its name.
                ;; When the scope includes `command' the name also depends on
                ;; `projectile--compilation-command-type', which projectile binds during the
                ;; call and which is nil before it, so a prediction made here would name a
                ;; buffer that never comes into existence. Capturing the process buffer is
                ;; correct under every scope and leaves the naming scheme entirely
                ;; projectile's business. The hook runs synchronously inside `compile', so
                ;; the capture is complete by the time the task function returns.
                (let* ((started nil)
                       (compilation-start-hook
                        (cons (lambda (proc) (setq started (process-buffer proc)))
                              compilation-start-hook)))
                  (funcall task-function nil)
                  (if (not (buffer-live-p started))
                      (format "Error: %s started no compilation process, so there is nothing to query" task-type)
                    (format "Started %s in buffer: %s" task-type (buffer-name started)))))))))))

  ;; Tool 2: Wait for projectile task completion and get size info
  (defun claude-code-ide-extras-projectile--task-wait (buffer-name)
    "Check if compilation is finished and return size info when done.

BUFFER-NAME is the name of the compilation buffer to check.

Returns \\='running if still executing, or \\='finished with output size
\(lines and chars) when complete. Use this to poll for completion and
decide whether to use head/tail limiting when calling
projectile_task_query."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (get-buffer buffer-name)))
        (if (not buf)
            (format "Error: Buffer not found: %s" buffer-name)
          (with-current-buffer buf
            (if (and (get-buffer-process buf)
                     (process-live-p (get-buffer-process buf)))
                (format "Status: running")
              ;; Compilation finished - return size info
              (let* ((line-count (count-lines (point-min) (point-max)))
                     (char-count (- (point-max) (point-min))))
                (format "Status: finished\n\nOutput size:\n  Lines: %d\n  Characters: %d"
                        line-count char-count))))))))

  ;; Tool 3: Query projectile task output (call after task-wait says finished)
  (defun claude-code-ide-extras-projectile--task-query (buffer-name &optional start-line num-lines)
    "Retrieve output from a finished compilation buffer.

BUFFER-NAME is the name of the compilation buffer to query.
Optional START-LINE is the first line to retrieve (1-based, negative
counts from end).
Optional NUM-LINES is the number of lines to retrieve.

START-LINE and NUM-LINES must both be provided or both be omitted.
If omitted, returns entire compilation output.

This should only be called after projectile_task_wait indicates the
task is finished.
Returns the compilation output, optionally limited by the line
range."
    (claude-code-ide-mcp-server-with-session-context nil
      (claude-code-ide-extras-common--buffer-query buffer-name start-line num-lines)))
  ;; Tool 4: Kill a running projectile task
  (defun claude-code-ide-extras-projectile--task-kill (buffer-name)
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
              ;; Use compilation-mode's built-in kill function
              (kill-compilation)
              (format "Killed compilation in buffer: %s" buffer-name)))))))


  ;; Tool 5: Search projectile task output
  (defun claude-code-ide-extras-projectile--task-search (buffer-name pattern &optional context-lines)
    "Search projectile task/compilation output for PATTERN.
BUFFER-NAME is the compilation buffer name (from task_start).
PATTERN is a regular expression to search for.
CONTEXT-LINES specifies number of lines before/after each match (default 0)."
    (claude-code-ide-mcp-server-with-session-context nil
      (claude-code-ide-extras-common--buffer-search buffer-name pattern context-lines)))

  ;; Project file and buffer enumeration
  (defun claude-code-ide-extras-projectile--get-project-files (&optional filter-regex)
    "Enumerate all files in the current project.

Uses projectile's cached file list for speed. Returns files as a list of
paths relative to the project root. This is much faster than using find
because projectile maintains an up-to-date cache of project files.

The file list respects projectile's ignore rules (from .projectile,
.gitignore, etc.), so generated files and dependencies are excluded.

Optional FILTER-REGEX (Emacs regex) filters the returned paths."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((project-root (projectile-project-root)))
            (if (not project-root)
                "Error: Not in a projectile project"
              (let ((files (projectile-current-project-files)))
                (if filter-regex
                    (seq-filter (lambda (f) (string-match-p filter-regex f)) files)
                  files))))
        (error (format "Error getting project files: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-projectile--get-project-buffers (&optional filter-regex mode-filter files-only)
    "Return open buffers belonging to the current project as a Lisp form.
Each entry is an alist with name, mode, file, and modified fields.

Uses projectile-project-buffers, which includes buffers visiting project
files and project-associated buffers such as compilation buffers.

Optional FILTER-REGEX (Emacs regex) filters by buffer name.
Optional MODE-FILTER (Emacs regex) filters by major mode name.
When FILES-ONLY is non-nil, only file-visiting buffers are included."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((project-root (projectile-project-root)))
            (if (not project-root)
                "Error: Not in a projectile project"
              (format "%S"
                      (delq nil
                            (mapcar
                             (lambda (buf)
                               (let* ((name (buffer-name buf))
                                      (mode (with-current-buffer buf major-mode))
                                      (file (buffer-file-name buf))
                                      (modified (buffer-modified-p buf)))
                                 (when (and (or (not filter-regex)
                                                (string-match-p filter-regex name))
                                            (or (not mode-filter)
                                                (string-match-p mode-filter (symbol-name mode)))
                                            (or (not files-only)
                                                file))
                                   `((name . ,name)
                                     (mode . ,mode)
                                     (file . ,file)
                                     (modified . ,modified)))))
                             (projectile-project-buffers))))))
        (error (format "Error listing project buffers: %s" (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-projectile-setup ()
  "Register all Projectile MCP tools with claude-code-ide."
  (interactive)

  ;; `projectile-compilation-buffer-scope' arrived in projectile 3.4.0, and task_start
  ;; depends on it to decide whether compilation buffers are safely qualified. Probe for the
  ;; function rather than comparing `projectile-version': it states the requirement itself
  ;; instead of a proxy for it, and cannot be misled by a fork or a snapshot build.
  (unless (fboundp 'projectile-compilation-buffer-scope)
    (error "Projectile 3.4.0 or later is required; projectile-compilation-buffer-scope is missing"))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--get-project-buffer-local-keys
   :name claude-code-ide-extras-projectile-get-project-buffer-local-keys-tool-name
   :description "Get buffer-local variable names for the project root containing a file. Returns only names (lightweight discovery). Optional filter_regex (Emacs regex) to narrow results. Much cheaper than getting full variables. Use for discovery, then get_project_buffer_local_variables with filter for specific values."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project. Project root will be determined automatically.")
           (:name "filter_regex"
            :type string
            :description "Optional Emacs regular expression to filter variable names (e.g., \"^projectile-\" or \"^\\\\(projectile\\\\|lsp\\\\)-\")."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--get-project-buffer-local-variables
   :name claude-code-ide-extras-projectile-get-project-buffer-local-variables-tool-name
   :description "Get buffer-local variables with values for the project root containing a file. Returns buffer-local-variables as a Lisp form. Optional filter_regex (Emacs regex) to limit results. WARNING: Without filtering, this can be very context-expensive (10k+ tokens). STRONGLY RECOMMENDED: Use filter_regex to get only relevant variables."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project. Project root will be determined automatically.")
           (:name "filter_regex"
            :type string
            :description "Optional Emacs regular expression to filter variables by name (e.g., \"^projectile-\" or \"^\\\\(projectile\\\\|lsp\\\\)-\")."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--read-project-dir-locals
   :name claude-code-ide-extras-projectile-read-project-dir-locals-tool-name
   :description "DEPRECATED: Use get_project_buffer_local_keys/get_project_buffer_local_variables instead. Read buffer-local variables for the project root containing a file. Finds the project root via projectile/project.el, then returns ALL buffer-local-variables as a Lisp form. WARNING: Very context-expensive (10k+ tokens). The new tools support filtering and discovery patterns."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project. Project root will be determined automatically.")))

  ;; Register the projectile task MCP tools
  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-start
   :name claude-code-ide-extras-projectile-task-start-tool-name
   :description "Start a projectile task (compile, test, configure, install, package, run) for a project. Non-blocking - returns immediately with the name of the compilation buffer that was actually created. Use projectile_task_wait to poll for completion, then projectile_task_query to retrieve output. Requires projectile-compilation-buffer-scope to include 'project, so that concurrent sessions in different projects cannot share a buffer. The buffer name is qualified by project, and additionally by command type when the scope includes 'command, so it varies with the user's configuration - always use the name this tool returns rather than assuming one."
   :args '((:name "task_type"
            :type string
            :description "The type of projectile task to run: compile, test, configure, install, package, or run")
           (:name "command"
            :type string
            :description "The shell command to execute for this task.")
           (:name "file_path"
            :type string
            :description "Absolute path to a file in the project (used to determine which project to operate on).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-wait
   :name claude-code-ide-extras-projectile-task-wait-tool-name
   :description "Poll for projectile task completion and get output size. Returns 'running' if still executing, or 'finished' with line/character count when done. Use this to poll after projectile_task_start, then use the size info to decide whether to retrieve full output or use head/tail limiting with projectile_task_query."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to check (returned by projectile_task_start).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-query
   :name claude-code-ide-extras-projectile-task-query-tool-name
   :description "Retrieve compilation output from a finished task. Should only be called after projectile_task_wait indicates the task is finished. Returns full output if no range specified, or limited output if start_line and num_lines are provided (both required). Supports negative start_line to count from end."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to query (returned by projectile_task_start).")
           (:name "start_line"
            :type number
            :description "First line to retrieve (1-based, negative counts from end). Must be provided with num_lines."
            :optional t)
           (:name "num_lines"
            :type number
            :description "Number of lines to retrieve starting from start_line. Must be provided with start_line."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-kill
   :name claude-code-ide-extras-projectile-task-kill-tool-name
   :description "Kill a running compilation in the specified buffer. Equivalent to pressing C-c C-k in the compilation buffer."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to kill.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-search
   :name claude-code-ide-extras-projectile-task-search-tool-name
   :description "Search for a pattern in projectile task/compilation output. Returns matching lines with optional context. Use this to find specific errors, warnings, or log output without retrieving the entire output."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to search (returned by projectile_task_start).")
           (:name "pattern"
            :type string
            :description "Regular expression pattern to search for (e.g., 'error:', 'warning:', 'undefined reference').")
           (:name "context_lines"
            :type number
            :description "Number of context lines to show before and after each match (optional, default 0)."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--get-project-files
   :name claude-code-ide-extras-projectile-get-project-files-tool-name
   :description "Enumerate all files in the current project. Returns a list of file paths relative to project root. Uses projectile's cached file list for speed, respecting ignore rules from .projectile and .gitignore. Optional filter_regex (Emacs regex) to narrow results."
   :args '((:name "filter_regex"
            :type string
            :description "Optional Emacs regex to filter file paths (e.g., \"\\.cpp$\" or \"^test/\")."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--get-project-buffers
   :name claude-code-ide-extras-projectile-get-project-buffers-tool-name
   :description "List open buffers belonging to the current project. Returns a Lisp list of alists, each with name, mode, file (nil if not visiting a file), and modified fields. Includes source buffers and project-associated buffers such as compilation buffers. Use filter_regex to match buffer names, mode_filter to match major mode, or files_only to restrict to file-visiting buffers."
   :args '((:name "filter_regex"
            :type string
            :description "Optional Emacs regex to filter by buffer name (e.g., \"\\*compilation\" or \"\\.cpp$\")."
            :optional t)
           (:name "mode_filter"
            :type string
            :description "Optional Emacs regex to filter by major mode name (e.g., \"c++-mode\" or \"compilation-mode\")."
            :optional t)
           (:name "files_only"
            :type boolean
            :description "When true, return only buffers that are visiting a file."
            :optional t)))

  (message "Claude Code IDE Extras: Projectile tools registered"))

(provide 'claude-code-ide-extras-projectile)
;;; claude-code-ide-extras-projectile.el ends here
