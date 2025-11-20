;;; claude-code-ide-extras-core.el --- Core Emacs MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;; Package-Requires: ((emacs "30.1") (claude-code-ide "0"))
;; Keywords: tools, help, ai, claude, mcp
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

  ;; Custom MCP tools for Emacs introspection
  (defun claude-code-ide-extras-core--describe (name type)
    "Describe an Emacs symbol/mode/package.
NAME is the symbol name as a string.
TYPE is one of: function, variable, mode, package, symbol."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((symbol (intern name))
                 (temp-buf (generate-new-buffer " *temp-help*")))
            (unwind-protect
                (save-window-excursion
                  (cl-letf (((symbol-function 'help-buffer)
                             (lambda () temp-buf)))
                    (pcase type
                      ("function" (describe-function symbol))
                      ("variable" (describe-variable symbol))
                      ("mode" (describe-function symbol))
                      ("package" (describe-package symbol))
                      ("symbol" (describe-symbol symbol))
                      (_ (error "Unknown type '%s'. Must be one of: function, variable, mode, package, symbol" type))))
                  (with-current-buffer temp-buf
                    (buffer-string)))
              (when (buffer-live-p temp-buf)
                (kill-buffer temp-buf))))
        (error (format "Error describing %s: %s" name (error-message-string err))))))

  (defun claude-code-ide-extras-core--apropos (pattern)
    "Search for all Emacs symbols matching PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (let ((show-all t))
              (apropos pattern show-all)
              ;; Capture content from *Apropos* buffer
              (with-current-buffer "*Apropos*"
                (prog1 (buffer-string)
                  (kill-buffer)))))
        (error (format "Error running apropos: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-core--apropos-command (pattern)
    "Search for interactive Emacs commands matching PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (apropos-command pattern t)
            ;; Capture content from *Apropos* buffer
            (with-current-buffer "*Apropos*"
              (prog1 (buffer-string)
                (kill-buffer))))
        (error (format "Error running apropos-command: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-core--apropos-documentation (pattern)
    "Search Emacs documentation for PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (apropos-documentation pattern)
            ;; Capture content from *Apropos* buffer
            (with-current-buffer "*Apropos*"
              (prog1 (buffer-string)
                (kill-buffer))))
        (error (format "Error running apropos-documentation: %s" (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-core-setup ()
  "Register all core Emacs MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-core--describe
   :name "claude-code-ide-extras-core/describe"
   :description "Get documentation for an Emacs symbol. Returns docstring, current value (for variables), arguments (for functions), and other metadata from the running Emacs session."
   :args '((:name "name"
            :type string
            :description "The name of the symbol to describe (e.g., 'projectile-compile-project', 'lsp-mode').")
           (:name "type"
            :type string
            :description "The type of thing to describe: function, variable, mode, package, or symbol. Use 'symbol' for a unified view of all aspects.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-core--apropos
   :name "claude-code-ide-extras-core/apropos"
   :description "Search for all Emacs symbols (functions, variables, faces, etc.) matching a pattern. Use for broad exploration."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match symbol names.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-core--apropos-command
   :name "claude-code-ide-extras-core/apropos_command"
   :description "Search for interactive Emacs commands (callable via M-x) matching a pattern. More focused than emacs_apropos - only returns commands users can invoke."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match command names.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-core--apropos-documentation
   :name "claude-code-ide-extras-core/apropos_documentation"
   :description "Search Emacs documentation text for a pattern. Finds functions/variables whose docstrings contain the pattern. Use for concept-based search (e.g., 'buffer naming', 'code formatting')."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match in documentation text.")))

  (message "Claude Code IDE Extras: Core tools registered"))

(provide 'claude-code-ide-extras-core)
;;; claude-code-ide-extras-core.el ends here
