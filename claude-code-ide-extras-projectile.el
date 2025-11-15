;;; claude-code-ide-extras-projectile.el --- Projectile MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <acm@magnitude.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (projectile "2.8.0") (claude-code-ide "0.1.0"))
;; Keywords: tools, projectile, ai, claude, mcp
;; URL: https://github.com/yourusername/claude-code-ide-extras

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
;;   (package-vc-install-from-checkout "/path/to/claude-code-ide-extras"
;;                                      'claude-code-ide-extras-projectile)
;;   (require 'claude-code-ide-extras-projectile)
;;   (claude-code-ide-extras-projectile-setup)

;;; Code:

(require 'projectile)
(require 'claude-code-ide)

(defgroup claude-code-ide-extras-projectile nil
  "Projectile MCP tools for claude-code-ide."
  :group 'projectile
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-projectile-")

;;; Tool implementations

  ;; Custom MCP tools for reading dir-locals
  (defun claude-code-ide-extras-projectile--read-dir-locals (file-path)
    "Read effective dir-local variables for FILE-PATH.
Opens FILE-PATH and returns buffer-local-variables as a Lisp form."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((buffer (find-file-noselect file-path)))
            (unwind-protect
                (with-current-buffer buffer
                  (format "%S" (buffer-local-variables)))
              (kill-buffer buffer)))
        (error (format "Error reading dir-locals for %s: %s"
                      file-path
                      (error-message-string err))))))

  (defun claude-code-ide-extras-projectile--read-project-dir-locals (file-path)
    "Read effective dir-local variables for the project containing FILE-PATH.
Finds the project root and delegates to `claude-code-ide-extras-projectile--read-dir-locals`."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((default-directory (file-name-directory file-path))
                 (project-root (or (projectile-project-root)
                                  (when-let ((proj (project-current)))
                                    (project-root proj))
                                  default-directory))
                 ;; Use a dummy file in the project root
                 (probe-file (expand-file-name ".dir-locals-probe" project-root)))
            (claude-code-ide-extras-projectile--read-dir-locals probe-file))
        (error (format "Error reading project dir-locals: %s" (error-message-string err))))))

  ;; Custom MCP tools for projectile task management (split architecture)
  (require 'seq)  ; For seq-take-last in task output limiting

  ;; Tool 1: Start a projectile task (non-blocking)
  (defun claude-code-ide-extras-projectile--task-start (task-type command file-path)
    "Start a projectile task (compile, test, configure, install, package, run).
Returns the compilation buffer name for later querying.

TASK-TYPE is one of: compile, test, configure, install, package, run.
COMMAND is the shell command to execute (required).
FILE-PATH is used to determine which project to operate on."
    (claude-code-ide-mcp-server-with-session-context nil
      ;; Validate projectile-per-project-compilation-buffer is set
      (if (not projectile-per-project-compilation-buffer)
          "Error: projectile-per-project-compilation-buffer must be t for safe parallel compilation. Add (setq projectile-per-project-compilation-buffer t) to your Emacs config."
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
                ;; Compute the buffer name deterministically (respects per-project setting)
                (let ((buffer-name (projectile-compilation-buffer-name "compilation")))
                  ;; Call the projectile task function (non-blocking)
                  (funcall task-function nil)
                  ;; Return the buffer name for later querying
                  (format "Started %s in buffer: %s" task-type buffer-name)))))))))

  ;; Tool 2: Wait for projectile task completion and get size info
  (defun claude-code-ide-extras-projectile--task-wait (buffer-name)
    "Check if compilation is finished and return size info when done.

BUFFER-NAME is the name of the compilation buffer to check.

Returns 'running' if still executing, or 'finished' with output size (lines and chars)
when complete. Use this to poll for completion and decide whether to use head/tail
limiting when calling projectile_task_query."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (get-buffer buffer-name)))
        (if (not buf)
            (format "Error: Buffer not found: %s" buffer-name)
          (with-current-buffer buf
            (if (memq buf compilation-in-progress)
                (format "Status: running")
              ;; Compilation finished - return size info
              (let* ((line-count (count-lines (point-min) (point-max)))
                     (char-count (- (point-max) (point-min))))
                (format "Status: finished\n\nOutput size:\n  Lines: %d\n  Characters: %d"
                        line-count char-count))))))))

  ;; Tool 3: Query projectile task output (call after task-wait says finished)
  (defun claude-code-ide-extras-projectile--task-query (buffer-name &optional head-lines tail-lines)
    "Retrieve output from a finished compilation buffer.

BUFFER-NAME is the name of the compilation buffer to query.
Optional HEAD-LINES limits output to first N lines.
Optional TAIL-LINES limits output to last N lines.

This should only be called after projectile_task_wait indicates the task is finished.
Returns the compilation output, optionally limited by head-lines or tail-lines."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (get-buffer buffer-name)))
        (if (not buf)
            (format "Error: Buffer not found: %s" buffer-name)
          (with-current-buffer buf
            (let* ((full-output (buffer-substring-no-properties (point-min) (point-max)))
                   (lines (split-string full-output "\n")))
              (cond
               ;; Limit to first N lines
               (head-lines
                (string-join (seq-take lines head-lines) "\n"))
               ;; Limit to last N lines (use nbutlast or seq-drop since seq-take-last doesn't exist)
               (tail-lines
                (let ((drop-count (max 0 (- (length lines) tail-lines))))
                  (string-join (seq-drop lines drop-count) "\n")))
               ;; Return full output
               (t full-output))))))))

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
            (if (not (memq buf compilation-in-progress))
                (format "No compilation running in buffer: %s" buffer-name)
              ;; Use compilation-mode's built-in kill function
              (kill-compilation)
              (format "Killed compilation in buffer: %s" buffer-name)))))))

  ;; Generic buffer search utility
  (defun claude-code-ide-extras-projectile--search-buffer (buffer-name pattern &optional context-lines)
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
              (progn
                ;; Run occur - creates new *Occur*
                (with-current-buffer buf
                  (occur pattern (or context-lines 0)))
                ;; Read from the new *Occur* buffer
                (with-current-buffer "*Occur*"
                  (buffer-substring-no-properties (point-min) (point-max))))
            ;; Clean up: kill our *Occur*, restore saved one
            (when (get-buffer "*Occur*")
              (kill-buffer "*Occur*"))
            (when saved-occur-buf
              (with-current-buffer saved-occur-buf
                (rename-buffer "*Occur*"))))))))

  ;; Tool 5: Search projectile task output
  (defun claude-code-ide-extras-projectile--task-search (buffer-name pattern &optional context-lines)
    "Search projectile task/compilation output for PATTERN.
BUFFER-NAME is the compilation buffer name (from task_start).
PATTERN is a regular expression to search for.
CONTEXT-LINES specifies number of lines before/after each match (default 0)."
    (claude-code-ide-mcp-server-with-session-context nil
      (claude-code-ide-extras-projectile--search-buffer buffer-name pattern context-lines)))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-projectile-setup ()
  "Register all Projectile MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--read-dir-locals
   :name "claude-code-ide-extras-projectile/read_dir_locals"
   :description "Read buffer-local variables for a specific file path. Opens the file and returns buffer-local-variables as a Lisp form."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file or directory to read buffer-local variables for.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--read-project-dir-locals
   :name "claude-code-ide-extras-projectile/read_project_dir_locals"
   :description "Read buffer-local variables for the project root containing a file. Finds the project root via projectile/project.el, then returns buffer-local-variables as a Lisp form."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project. Project root will be determined automatically.")))

  ;; Register the projectile task MCP tools
  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-start
   :name "claude-code-ide-extras-projectile/task_start"
   :description "Start a projectile task (compile, test, configure, install, package, run) for a project. Non-blocking - returns immediately with the compilation buffer name. Use projectile_task_wait to poll for completion, then projectile_task_query to retrieve output. Requires projectile-per-project-compilation-buffer to be enabled."
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
   :name "claude-code-ide-extras-projectile/task_wait"
   :description "Poll for projectile task completion and get output size. Returns 'running' if still executing, or 'finished' with line/character count when done. Use this to poll after projectile_task_start, then use the size info to decide whether to retrieve full output or use head/tail limiting with projectile_task_query."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to check (returned by projectile_task_start).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-query
   :name "claude-code-ide-extras-projectile/task_query"
   :description "Retrieve compilation output from a finished task. Should only be called after projectile_task_wait indicates the task is finished. Returns full output by default, or limited output if head_lines or tail_lines is specified."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to query (returned by projectile_task_start).")
           (:name "head_lines"
            :type number
            :description "Limit output to first N lines (like 'head -n'). Recommended for checking errors at start of output."
            :optional t)
           (:name "tail_lines"
            :type number
            :description "Limit output to last N lines (like 'tail -n'). Recommended for checking summary/final errors."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-kill
   :name "claude-code-ide-extras-projectile/task_kill"
   :description "Kill a running compilation in the specified buffer. Equivalent to pressing C-c C-k in the compilation buffer."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to kill.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-projectile--task-search
   :name "claude-code-ide-extras-projectile/task_search"
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

  (message "Claude Code IDE Extras: Projectile tools registered"))

(provide 'claude-code-ide-extras-projectile)
;;; claude-code-ide-extras-projectile.el ends here
