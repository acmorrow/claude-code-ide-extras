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

;;; MCP Tool Names

(defconst claude-code-ide-extras-meta-get-mcp-custom-advice-tool-name
  "claude-code-ide-extras-meta/get_mcp_custom_advice"
  "MCP tool name for get_mcp_custom_advice.")

;;; Customization

(defcustom claude-code-ide-extras-meta-get-mcp-custom-advice-usage-prompt
  "Call at session start to learn project-specific tool usage conventions and preferences."
  "Usage guidance for the get_mcp_custom_advice MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-meta)

(put 'claude-code-ide-extras-meta-get-mcp-custom-advice-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-meta-get-mcp-custom-advice-tool-name)

;;; Tool implementations

(defun claude-code-ide-extras-meta--get-mcp-custom-advice ()
  "Get project-specific guidance for MCP tool usage.

Reads usage-prompt customization variables for all registered
claude-code-ide-extras MCP tools and returns them in a readable format.

Only includes tools that are actually registered (via
`claude-code-ide-mcp-server-get-tool-names'), ensuring the guidance
matches the loaded tool set."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((prompts '())
          ;; Get all registered MCP tools
          (all-tools (claude-code-ide-mcp-server-get-tool-names)))
      ;; Find all defcustoms with our metadata property
      (mapatoms
       (lambda (sym)
         (when-let ((mcp-name (get sym 'claude-code-ide-extras-mcp-tool-name)))
           ;; Only include if the tool is actually registered
           (when (member mcp-name all-tools)
             (let ((value (symbol-value sym)))
               (when (and value (not (string-empty-p value)))
                 (push (cons mcp-name value) prompts)))))))

      ;; Format the output
      (if (null prompts)
          "No custom MCP tool usage guidance has been configured.\n\nYou can set guidance globally via :custom in use-package, or per-project via .dir-locals.el."
        (concat
         "# MCP Tool Usage Guidance\n\n"
         "The following custom guidance has been configured for MCP tools:\n\n"
         (mapconcat
          (lambda (pair)
            (let ((mcp-name (car pair))
                  (value (cdr pair)))
              (format "## %s\n%s\n" mcp-name value)))
          (sort prompts (lambda (a b) (string< (car a) (car b))))
          "\n"))))))

;;; Setup function

(defun claude-code-ide-extras-meta-setup ()
  "Register meta MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-meta--get-mcp-custom-advice
   :name claude-code-ide-extras-meta-get-mcp-custom-advice-tool-name
   :description "Get project-specific guidance for using MCP tools. Reads customization variables (set via :custom or .dir-locals.el) that provide context about how to use the tools in this project. Call this at the start of a session to learn how the user wants to make use of the loaded claude-code-ide-extras MCPs."
   :args '())

  (message "Claude Code IDE Extras: Meta tools registered"))

(provide 'claude-code-ide-extras-meta)
;;; claude-code-ide-extras-meta.el ends here
