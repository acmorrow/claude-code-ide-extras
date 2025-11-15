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

;; TODO: Add tool implementations here
;; - read_dir_locals
;; - read_project_dir_locals
;; - projectile_task_start/wait/query/kill/search
;; - projectile_shell_execute/wait/query/search
;; - buffer_search

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-projectile-setup ()
  "Register all Projectile MCP tools with claude-code-ide."
  (interactive)
  ;; TODO: Register all tools here
  (message "Claude Code IDE Extras: Projectile tools registered"))

(provide 'claude-code-ide-extras-projectile)
;;; claude-code-ide-extras-projectile.el ends here
