;;; claude-code-ide-extras-core.el --- Core Emacs MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <acm@magnitude.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (claude-code-ide "0.1.0"))
;; Keywords: tools, help, ai, claude, mcp
;; URL: https://github.com/yourusername/claude-code-ide-extras

;;; Commentary:

;; This package provides core Emacs introspection MCP (Model Context Protocol)
;; tools for claude-code-ide.el, enabling Claude to:
;;
;; - Discover Emacs functions, variables, and commands
;; - Read documentation for Emacs symbols
;; - Search Emacs help system
;; - Learn about the user's Emacs environment
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (package-vc-install-from-checkout "/path/to/claude-code-ide-extras"
;;                                      'claude-code-ide-extras-core)
;;   (require 'claude-code-ide-extras-core)
;;   (claude-code-ide-extras-core-setup)

;;; Code:

(require 'claude-code-ide)

(defgroup claude-code-ide-extras-core nil
  "Core Emacs introspection MCP tools for claude-code-ide."
  :group 'help
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-core-")

;;; Tool implementations

;; TODO: Add tool implementations here
;; - emacs_describe
;; - emacs_apropos
;; - emacs_apropos_command
;; - emacs_apropos_documentation

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-core-setup ()
  "Register all core Emacs MCP tools with claude-code-ide."
  (interactive)
  ;; TODO: Register all tools here
  (message "Claude Code IDE Extras: Core tools registered"))

(provide 'claude-code-ide-extras-core)
;;; claude-code-ide-extras-core.el ends here
