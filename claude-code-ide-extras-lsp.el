;;; claude-code-ide-extras-lsp.el --- LSP MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <acm@magnitude.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (lsp-mode "8.0.0") (claude-code-ide "0.1.0"))
;; Keywords: tools, lsp, ai, claude, mcp
;; URL: https://github.com/yourusername/claude-code-ide-extras

;;; Commentary:

;; This package provides LSP-specific MCP (Model Context Protocol) tools
;; for claude-code-ide.el, enabling Claude to:
;;
;; - Format buffers using LSP
;; - Get hover information (type signatures, documentation)
;; - Interact with LSP language servers
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (package-vc-install-from-checkout "/path/to/claude-code-ide-extras"
;;                                      'claude-code-ide-extras-lsp)
;;   (require 'claude-code-ide-extras-lsp)
;;   (claude-code-ide-extras-lsp-setup)

;;; Code:

(require 'lsp-mode)
(require 'claude-code-ide)

(defgroup claude-code-ide-extras-lsp nil
  "LSP MCP tools for claude-code-ide."
  :group 'lsp-mode
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-lsp-")

;;; Tool implementations

;; TODO: Add tool implementations here
;; - lsp_format_buffer
;; - lsp_describe_thing_at_point

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-lsp-setup ()
  "Register all LSP MCP tools with claude-code-ide."
  (interactive)
  ;; TODO: Register all tools here
  (message "Claude Code IDE Extras: LSP tools registered"))

(provide 'claude-code-ide-extras-lsp)
;;; claude-code-ide-extras-lsp.el ends here
