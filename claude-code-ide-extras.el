;;; claude-code-ide-extras.el --- MCP tools suite for claude-code-ide  -*- lexical-binding: t; -*-

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

;; This is a meta-package that provides a convenient way to install and set up
;; all claude-code-ide-extras MCP tools at once.
;;
;; The suite includes:
;; - claude-code-ide-extras-emacs: Emacs introspection (describe, apropos, buffer access)
;; - claude-code-ide-extras-lsp: LSP integration (formatting, hover info)
;; - claude-code-ide-extras-meta: Tools about the MCP tools themselves
;; - claude-code-ide-extras-projectile: Project tasks and shell execution
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras)
;;   (claude-code-ide-extras-setup)
;;
;; This will register all available MCP tools with claude-code-ide.
;;
;; You can also install and set up individual packages if you only need
;; specific functionality. See each package's commentary for details.

;;; Code:

(require 'claude-code-ide-extras-emacs)
(require 'claude-code-ide-extras-lsp)
(require 'claude-code-ide-extras-meta)
(require 'claude-code-ide-extras-projectile)

(defgroup claude-code-ide-extras nil
  "MCP tools suite for claude-code-ide."
  :group 'tools
  :prefix "claude-code-ide-extras-")

(defconst claude-code-ide-extras-version "0.0.1"
  "Version of claude-code-ide-extras.")

;;;###autoload
(defun claude-code-ide-extras-setup ()
  "Set up all claude-code-ide-extras packages.
This registers all MCP tools from projectile, emacs, lsp, and meta packages."
  (interactive)
  (claude-code-ide-extras-emacs-setup)
  (claude-code-ide-extras-lsp-setup)
  (claude-code-ide-extras-meta-setup)
  (claude-code-ide-extras-projectile-setup)
  (message "Claude Code IDE Extras: All tools registered"))

(provide 'claude-code-ide-extras)
;;; claude-code-ide-extras.el ends here
