;;; claude-code-ide-extras-git.el --- Git/VC MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;;         Tim Ransom
;; Keywords: tools, vc, git, ai, claude, mcp
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

;; This package provides git/VC MCP (Model Context Protocol) tools
;; for claude-code-ide.el, enabling Claude to:
;;
;; - View git status of the current project
;; - View git diffs (staged and unstaged)
;; - Browse recent commit history
;; - View git blame for files with optional line ranges
;;
;; Uses Emacs' built-in vc.el and vc-git.el (no magit dependency).
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-git)
;;   (claude-code-ide-extras-git-setup)

;;; Code:

(require 'claude-code-ide)
(require 'vc)
(require 'vc-git)

(defgroup claude-code-ide-extras-git nil
  "Git/VC MCP tools for claude-code-ide."
  :group 'vc
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-git-")

(defconst claude-code-ide-extras-git-version "0.0.4"
  "Version of claude-code-ide-extras-git.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-git-git-status-tool-name
  "claude-code-ide-extras-git/git_status"
  "MCP tool name for git_status.")

(defconst claude-code-ide-extras-git-git-diff-tool-name
  "claude-code-ide-extras-git/git_diff"
  "MCP tool name for git_diff.")

(defconst claude-code-ide-extras-git-git-log-tool-name
  "claude-code-ide-extras-git/git_log"
  "MCP tool name for git_log.")

(defconst claude-code-ide-extras-git-git-blame-tool-name
  "claude-code-ide-extras-git/git_blame"
  "MCP tool name for git_blame.")

(defconst claude-code-ide-extras-git-git-stash-tool-name
  "claude-code-ide-extras-git/git_stash"
  "MCP tool name for git_stash.")

(defconst claude-code-ide-extras-git-git-branch-tool-name
  "claude-code-ide-extras-git/git_branch"
  "MCP tool name for git_branch.")

;;; Customization

(defcustom claude-code-ide-extras-git-git-status-usage-prompt
  "Get git status of the current project. Shows modified, staged, and untracked files. Optional directory argument defaults to the current project root."
  "Usage guidance for the git_status MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-git)

(put 'claude-code-ide-extras-git-git-status-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-git-git-status-tool-name)

(defcustom claude-code-ide-extras-git-git-diff-usage-prompt
  "Get git diff output. Can diff a single file or the whole project. Use staged=\"true\" to see staged (cached) changes."
  "Usage guidance for the git_diff MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-git)

(put 'claude-code-ide-extras-git-git-diff-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-git-git-diff-tool-name)

(defcustom claude-code-ide-extras-git-git-log-usage-prompt
  "Get recent git commit history. Shows hash, author, date, and subject. Optional file_path limits to a single file. Optional count controls number of entries (default 10)."
  "Usage guidance for the git_log MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-git)

(put 'claude-code-ide-extras-git-git-log-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-git-git-log-tool-name)

(defcustom claude-code-ide-extras-git-git-blame-usage-prompt
  "Get git blame for a file. Shows commit hash, author, date, and line content. Optional start_line and end_line to limit the range."
  "Usage guidance for the git_blame MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-git)

(put 'claude-code-ide-extras-git-git-blame-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-git-git-blame-tool-name)

(defcustom claude-code-ide-extras-git-git-stash-usage-prompt
  "List, show, push, pop, or drop git stashes. Action defaults to \"list\". Use push to stash changes, pop to restore, show to inspect, drop to delete."
  "Usage guidance for the git_stash MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-git)

(put 'claude-code-ide-extras-git-git-stash-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-git-git-stash-tool-name)

(defcustom claude-code-ide-extras-git-git-branch-usage-prompt
  "List git branches or get details about the current branch. Shows local and remote tracking info."
  "Usage guidance for the git_branch MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-git)

(put 'claude-code-ide-extras-git-git-branch-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-git-git-branch-tool-name)

;;; Internal helpers

(defun claude-code-ide-extras-git--find-git-root (&optional directory)
  "Find the git root for DIRECTORY.
If DIRECTORY is nil, use `default-directory'.
Returns the git root directory or nil."
  (let ((dir (or directory default-directory)))
    (vc-git-root dir)))

(defun claude-code-ide-extras-git--run-git (root &rest args)
  "Run git command with ARGS in ROOT directory.
Returns the output as a string."
  (let ((default-directory root))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil args)
      (buffer-string))))

(defun claude-code-ide-extras-git--format-porcelain-status (porcelain-output)
  "Format PORCELAIN-OUTPUT from `git status --porcelain' into a readable string."
  (if (string-empty-p (string-trim porcelain-output))
      "Working tree clean — no modified, staged, or untracked files."
    (let ((staged '())
          (modified '())
          (untracked '())
          (lines (split-string porcelain-output "\n" t)))
      (dolist (line lines)
        (when (>= (length line) 3)
          (let ((index-status (aref line 0))
                (worktree-status (aref line 1))
                (file (substring line 3)))
            (cond
             ((char-equal index-status ??)
              (push file untracked))
             (t
              (unless (char-equal index-status ?\s)
                (push (cons (char-to-string index-status) file) staged))
              (unless (char-equal worktree-status ?\s)
                (push (cons (char-to-string worktree-status) file) modified)))))))
      (let ((sections '()))
        (when staged
          (push (concat "Staged:\n"
                        (mapconcat (lambda (pair)
                                     (format "  [%s] %s" (car pair) (cdr pair)))
                                   (nreverse staged) "\n"))
                sections))
        (when modified
          (push (concat "Modified (unstaged):\n"
                        (mapconcat (lambda (pair)
                                     (format "  [%s] %s" (car pair) (cdr pair)))
                                   (nreverse modified) "\n"))
                sections))
        (when untracked
          (push (concat "Untracked:\n"
                        (mapconcat (lambda (f) (format "  %s" f))
                                   (nreverse untracked) "\n"))
                sections))
        (mapconcat #'identity (nreverse sections) "\n\n")))))

;;; Tool implementations

(defun claude-code-ide-extras-git--git-status (&optional directory)
  "Get git status of the project at DIRECTORY.
DIRECTORY defaults to the current project root."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((root (claude-code-ide-extras-git--find-git-root directory)))
          (unless root
            (error "No git repository found for directory: %s" (or directory default-directory)))
          (let* ((porcelain (claude-code-ide-extras-git--run-git root "status" "--porcelain"))
                 (branch (string-trim (claude-code-ide-extras-git--run-git root "rev-parse" "--abbrev-ref" "HEAD"))))
            (format "Branch: %s\n\n%s"
                    branch
                    (claude-code-ide-extras-git--format-porcelain-status porcelain))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-git--git-diff (&optional file-path staged)
  "Get git diff output.
Optional FILE-PATH limits diff to a single file.
Optional STAGED (string \"true\"/\"false\") shows staged changes."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((dir (if file-path (file-name-directory file-path) default-directory))
               (root (claude-code-ide-extras-git--find-git-root dir)))
          (unless root
            (error "No git repository found for: %s" dir))
          (let* ((staged-p (and staged (string-equal staged "true")))
                 (args (append (list "diff")
                               (when staged-p (list "--cached"))
                               (when file-path (list "--" file-path))))
                 (output (apply #'claude-code-ide-extras-git--run-git root args)))
            (if (string-empty-p (string-trim output))
                (format "No %sdifferences found%s."
                        (if staged-p "staged " "")
                        (if file-path (format " for %s" file-path) ""))
              output)))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-git--git-log (&optional file-path count)
  "Get recent git commit history.
Optional FILE-PATH limits log to a single file.
Optional COUNT is the number of commits to show (default 10)."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((dir (if file-path (file-name-directory file-path) default-directory))
               (root (claude-code-ide-extras-git--find-git-root dir))
               (n (if count (number-to-string count) "10")))
          (unless root
            (error "No git repository found for: %s" dir))
          (let* ((format-str "--format=%h  %an  %ad  %s")
                 (args (append (list "log" format-str
                                     (format "-n%s" n)
                                     "--date=short")
                               (when file-path (list "--" file-path))))
                 (output (apply #'claude-code-ide-extras-git--run-git root args)))
            (if (string-empty-p (string-trim output))
                (format "No commits found%s."
                        (if file-path (format " for %s" file-path) ""))
              output)))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-git--git-blame (file-path &optional start-line end-line)
  "Get git blame for FILE-PATH.
Optional START-LINE and END-LINE limit the blame range."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((root (claude-code-ide-extras-git--find-git-root
                      (file-name-directory file-path))))
          (unless root
            (error "No git repository found for: %s" file-path))
          (unless (file-exists-p file-path)
            (error "File does not exist: %s" file-path))
          (let* ((line-args (when (and start-line end-line)
                              (list "-L"
                                    (format "%s,%s"
                                            (number-to-string start-line)
                                            (number-to-string end-line)))))
                 (args (append (list "blame" "--date=short")
                               line-args
                               (list "--" file-path)))
                 (output (apply #'claude-code-ide-extras-git--run-git root args)))
            (if (string-empty-p (string-trim output))
                (format "No blame output for %s." file-path)
              output)))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-git--git-stash (&optional action stash-ref message)
  "Manage git stashes.
ACTION is one of \"list\", \"show\", \"push\", \"pop\", \"drop\" (default \"list\").
STASH-REF is the stash reference for show/pop/drop (e.g. \"stash@{0}\").
MESSAGE is an optional message for push."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let* ((root (claude-code-ide-extras-git--find-git-root))
               (act (or action "list")))
          (unless root
            (error "No git repository found"))
          (cond
           ((string= act "list")
            (let ((output (claude-code-ide-extras-git--run-git root "stash" "list")))
              (if (string-empty-p (string-trim output))
                  "No stashes."
                output)))
           ((string= act "show")
            (let ((ref (or stash-ref "stash@{0}")))
              (claude-code-ide-extras-git--run-git root "stash" "show" "-p" ref)))
           ((string= act "push")
            (let ((args (append (list "stash" "push")
                                (when message (list "-m" message)))))
              (apply #'claude-code-ide-extras-git--run-git root args)))
           ((string= act "pop")
            (let ((ref (or stash-ref "stash@{0}")))
              (claude-code-ide-extras-git--run-git root "stash" "pop" ref)))
           ((string= act "drop")
            (let ((ref (or stash-ref "stash@{0}")))
              (claude-code-ide-extras-git--run-git root "stash" "drop" ref)))
           (t
            (format "Unknown stash action: %s. Use list, show, push, pop, or drop." act))))
      (error (format "Error: %s" (error-message-string err))))))

(defun claude-code-ide-extras-git--git-branch (&optional directory)
  "List git branches with tracking info.
DIRECTORY defaults to the current project root."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((root (claude-code-ide-extras-git--find-git-root directory)))
          (unless root
            (error "No git repository found for: %s" (or directory default-directory)))
          (let* ((current (string-trim
                           (claude-code-ide-extras-git--run-git root "rev-parse" "--abbrev-ref" "HEAD")))
                 (branches (claude-code-ide-extras-git--run-git
                            root "branch" "-vv" "--format"
                            "%(if)%(HEAD)%(then)* %(else)  %(end)%(refname:short) %(objectname:short) %(upstream:short) %(upstream:track)")))
            (format "Current branch: %s\n\n%s" current (string-trim branches))))
      (error (format "Error: %s" (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-git-setup ()
  "Register all git/VC MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-git--git-status
   :name claude-code-ide-extras-git-git-status-tool-name
   :description "Get git status of the current project. Shows branch name, staged, modified, and untracked files."
   :args '((:name "directory"
            :type string
            :description "Project root directory. Defaults to the current project root."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-git--git-diff
   :name claude-code-ide-extras-git-git-diff-tool-name
   :description "Get git diff output for a file or the whole project. Use staged=\"true\" to see staged (cached) changes instead of unstaged working tree changes."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file to diff. If omitted, diffs the entire project."
            :optional t)
           (:name "staged"
            :type string
            :description "Set to \"true\" to show staged (cached) changes. Defaults to unstaged diff."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-git--git-log
   :name claude-code-ide-extras-git-git-log-tool-name
   :description "Get recent git commit history showing hash, author, date, and subject. Optional file_path limits to a single file."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file to show history for. If omitted, shows project-wide history."
            :optional t)
           (:name "count"
            :type number
            :description "Number of commits to show (default 10)."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-git--git-blame
   :name claude-code-ide-extras-git-git-blame-tool-name
   :description "Get git blame for a file, showing commit hash, author, date, and line content. Optional start_line and end_line to limit the range."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to blame.")
           (:name "start_line"
            :type number
            :description "First line of the range to blame (1-based)."
            :optional t)
           (:name "end_line"
            :type number
            :description "Last line of the range to blame (1-based)."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-git--git-stash
   :name claude-code-ide-extras-git-git-stash-tool-name
   :description "Manage git stashes. List, show, push, pop, or drop stashes. Defaults to listing all stashes."
   :args '((:name "action"
            :type string
            :description "Stash action: \"list\", \"show\", \"push\", \"pop\", or \"drop\". Defaults to \"list\"."
            :optional t)
           (:name "stash_ref"
            :type string
            :description "Stash reference (e.g. \"stash@{0}\") for show/pop/drop. Defaults to most recent stash."
            :optional t)
           (:name "message"
            :type string
            :description "Message for the stash when action is \"push\"."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-git--git-branch
   :name claude-code-ide-extras-git-git-branch-tool-name
   :description "List git branches with current branch, commit hashes, and remote tracking info."
   :args '((:name "directory"
            :type string
            :description "Project root directory. Defaults to the current project root."
            :optional t)))

  (message "Claude Code IDE Extras: git tools registered"))

(provide 'claude-code-ide-extras-git)
;;; claude-code-ide-extras-git.el ends here
