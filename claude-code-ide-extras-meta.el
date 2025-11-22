;;; claude-code-ide-extras-meta.el --- Meta MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

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

;; This package provides meta-level MCP tools for claude-code-ide - tools
;; about the MCP tools themselves.
;;
;; Currently provides:
;; - get_mcp_custom_advice: Read project-specific guidance for MCP tool usage
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-meta)
;;   (claude-code-ide-extras-meta-setup)

;;; Code:

(require 'claude-code-ide)

(defgroup claude-code-ide-extras-meta nil
  "Meta MCP tools for claude-code-ide."
  :group 'tools
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-meta-")

;;; Tool implementations

  ;; Custom MCP tool for reading tool customization advice
  (defun claude-code-ide-extras-meta--get-mcp-custom-advice ()
    "Get project-specific guidance for MCP tool usage.
Reads customization variables like `claude-code-ide-extras-projectile-tool-prompts'
and returns them in a readable format for Claude to incorporate into its
understanding of how to use the tools in this project."
    (claude-code-ide-mcp-server-with-session-context nil
      ;; Stub implementation - return lorem ipsum for now
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.

This is a stub implementation of get_mcp_custom_advice.

In the future, this will read customization variables like:
- claude-code-ide-extras-projectile-tool-prompts
- claude-code-ide-extras-core-tool-prompts
- claude-code-ide-extras-lsp-tool-prompts
- claude-code-ide-extras-meta-tool-prompts

And format them into readable guidance for Claude to follow when using
the MCP tools in this project.

Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."))

;;; Setup function

(defun claude-code-ide-extras-meta-setup ()
  "Register meta MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-meta--get-mcp-custom-advice
   :name "claude-code-ide-extras-meta/get_mcp_custom_advice"
   :description "Get project-specific guidance for using MCP tools. Reads customization variables (set via :custom or .dir-locals.el) that provide context about how to use the tools in this project. Call this at the start of a session to learn how the user wants to make use of the loaded claude-code-ide-extras MCPs."
   :args '())

  (message "Claude Code IDE Extras: Meta tools registered"))

(provide 'claude-code-ide-extras-meta)
;;; claude-code-ide-extras-meta.el ends here
