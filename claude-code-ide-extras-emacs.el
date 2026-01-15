;;; claude-code-ide-extras-emacs.el --- Emacs introspection MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
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

;; This package provides Emacs introspection MCP (Model Context Protocol)
;; tools for claude-code-ide.el, enabling Claude to:
;;
;; - Discover Emacs functions, variables, and commands
;; - Read documentation for Emacs symbols
;; - Search Emacs help system
;; - Learn about the user's Emacs environment
;; - Execute elisp code for interactive development
;; - Manipulate buffers and point positions
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-emacs)
;;   (claude-code-ide-extras-emacs-setup)

;;; Security Considerations:
;;
;; This package includes tools that execute arbitrary elisp code
;; (eval-elisp, eval-region, eval-defun-at-point). These are powerful
;; debugging and development tools that assume a trusted relationship
;; between the user and the AI assistant.
;;
;; Enabling these tools grants Claude access to the Emacs session with full
;; user privileges. The tools can read, write, and delete files, make network
;; requests, spawn processes, and manipulate all Emacs state. This is appropriate
;; for trusted projects in personal or work environments. Do not use in multi-tenant
;; environments, shared Emacs sessions, or with untrusted AI systems.

;;; Code:

(require 'claude-code-ide)
(require 'claude-code-ide-extras-common)

(defgroup claude-code-ide-extras-emacs nil
  "Emacs introspection MCP tools for claude-code-ide."
  :group 'help
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-emacs-")

(defconst claude-code-ide-extras-emacs-version "0.0.3"
  "Version of claude-code-ide-extras-emacs.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-emacs-describe-tool-name
  "claude-code-ide-extras-emacs/describe"
  "MCP tool name for describe.")

(defconst claude-code-ide-extras-emacs-apropos-tool-name
  "claude-code-ide-extras-emacs/apropos"
  "MCP tool name for apropos.")

(defconst claude-code-ide-extras-emacs-apropos-command-tool-name
  "claude-code-ide-extras-emacs/apropos_command"
  "MCP tool name for apropos_command.")

(defconst claude-code-ide-extras-emacs-apropos-documentation-tool-name
  "claude-code-ide-extras-emacs/apropos_documentation"
  "MCP tool name for apropos_documentation.")

(defconst claude-code-ide-extras-emacs-buffer-query-tool-name
  "claude-code-ide-extras-emacs/buffer_query"
  "MCP tool name for buffer_query.")

(defconst claude-code-ide-extras-emacs-buffer-search-tool-name
  "claude-code-ide-extras-emacs/buffer_search"
  "MCP tool name for buffer_search.")

(defconst claude-code-ide-extras-emacs-read-dir-locals-tool-name
  "claude-code-ide-extras-emacs/read_dir_locals"
  "MCP tool name for read_dir_locals.")

(defconst claude-code-ide-extras-emacs-get-buffer-local-keys-tool-name
  "claude-code-ide-extras-emacs/get_buffer_local_keys"
  "MCP tool name for get_buffer_local_keys.")

(defconst claude-code-ide-extras-emacs-get-buffer-local-variables-tool-name
  "claude-code-ide-extras-emacs/get_buffer_local_variables"
  "MCP tool name for get_buffer_local_variables.")

(defconst claude-code-ide-extras-emacs-eval-elisp-tool-name
  "claude-code-ide-extras-emacs/eval_elisp"
  "MCP tool name for eval_elisp.")

(defconst claude-code-ide-extras-emacs-eval-region-tool-name
  "claude-code-ide-extras-emacs/eval_region"
  "MCP tool name for eval_region.")

(defconst claude-code-ide-extras-emacs-eval-defun-at-point-tool-name
  "claude-code-ide-extras-emacs/eval_defun_at_point"
  "MCP tool name for eval_defun_at_point.")

(defconst claude-code-ide-extras-emacs-find-file-tool-name
  "claude-code-ide-extras-emacs/find_file"
  "MCP tool name for find_file.")

(defconst claude-code-ide-extras-emacs-position-point-tool-name
  "claude-code-ide-extras-emacs/position_point"
  "MCP tool name for position_point.")

(defconst claude-code-ide-extras-emacs-select-region-tool-name
  "claude-code-ide-extras-emacs/select_region"
  "MCP tool name for select_region.")

(defconst claude-code-ide-extras-emacs-xref-find-definitions-at-point-tool-name
  "claude-code-ide-extras-emacs/xref_find_definitions_at_point"
  "MCP tool name for xref_find_definitions_at_point.")

(defconst claude-code-ide-extras-emacs-xref-find-references-at-point-tool-name
  "claude-code-ide-extras-emacs/xref_find_references_at_point"
  "MCP tool name for xref_find_references_at_point.")

;;; Customization

(defcustom claude-code-ide-extras-emacs-describe-usage-prompt
  "Get symbol documentation. Try 'symbol' type for complete info on functions, variables, and modes."
  "Usage guidance for the describe MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-describe-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-describe-tool-name)

(defcustom claude-code-ide-extras-emacs-apropos-usage-prompt
  "Search for symbols by name pattern. Use for broad exploration of available functionality."
  "Usage guidance for the apropos MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-apropos-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-apropos-tool-name)

(defcustom claude-code-ide-extras-emacs-apropos-command-usage-prompt
  "Find interactive commands only. More focused than apropos for discovering M-x commands."
  "Usage guidance for the apropos_command MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-apropos-command-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-apropos-command-tool-name)

(defcustom claude-code-ide-extras-emacs-apropos-documentation-usage-prompt
  "Search documentation by concept, not just symbol names. Useful for discovering related functionality."
  "Usage guidance for the apropos_documentation MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-apropos-documentation-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-apropos-documentation-tool-name)

(defcustom claude-code-ide-extras-emacs-buffer-query-usage-prompt
  "Read any Emacs buffer contents. Useful for *compilation*, *scratch*, *Messages*, etc."
  "Usage guidance for the buffer_query MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-buffer-query-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-buffer-query-tool-name)

(defcustom claude-code-ide-extras-emacs-buffer-search-usage-prompt
  "Search any buffer with regex. Complement to buffer_query for finding specific content."
  "Usage guidance for the buffer_search MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-buffer-search-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-buffer-search-tool-name)

(defcustom claude-code-ide-extras-emacs-read-dir-locals-usage-prompt
  "DEPRECATED: Use get_buffer_local_keys/get_buffer_local_variables instead. WARNING: This tool returns ALL buffer-local variables which can be very context-expensive (often 10k+ tokens). The new tools support filtering and discovery patterns."
  "Usage guidance for the read_dir_locals MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-read-dir-locals-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-read-dir-locals-tool-name)

(defcustom claude-code-ide-extras-emacs-get-buffer-local-keys-usage-prompt
  "List buffer-local variable names for a file. Returns only names (lightweight discovery). Optional filter_regex (Emacs regex) to narrow results. IMPORTANT: Even unfiltered, this is much cheaper than getting full variables. Use this for discovery, then get_buffer_local_variables with filter for specific values."
  "Usage guidance for the get_buffer_local_keys MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-get-buffer-local-keys-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-get-buffer-local-keys-tool-name)

(defcustom claude-code-ide-extras-emacs-get-buffer-local-variables-usage-prompt
  "Get buffer-local variables with values for a file. Optional filter_regex (Emacs regex) to limit results. WARNING: Without filtering, this can be very context-expensive (10k+ tokens). STRONGLY RECOMMENDED: Use filter_regex to get only relevant variables (e.g., \"^projectile-\" or \"^\\\\(projectile\\\\|lsp\\\\)-\"). Pattern: discover with get_buffer_local_keys first, then retrieve filtered values."
  "Usage guidance for the get_buffer_local_variables MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-get-buffer-local-variables-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-get-buffer-local-variables-tool-name)

(defcustom claude-code-ide-extras-emacs-eval-elisp-usage-prompt
  "Execute arbitrary elisp code and return the result. Powerful tool for exploring Emacs state and testing code."
  "Usage guidance for the eval_elisp MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-eval-elisp-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-eval-elisp-tool-name)

(defcustom claude-code-ide-extras-emacs-eval-region-usage-prompt
  "Evaluate a region of elisp code in a buffer. Useful for testing code snippets."
  "Usage guidance for the eval_region MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-eval-region-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-eval-region-tool-name)

(defcustom claude-code-ide-extras-emacs-eval-defun-at-point-usage-prompt
  "Evaluate the function definition at a specific point in a buffer. Essential for reloading function definitions during development."
  "Usage guidance for the eval_defun_at_point MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-eval-defun-at-point-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-eval-defun-at-point-tool-name)

(defcustom claude-code-ide-extras-emacs-find-file-usage-prompt
  "Open a file into an Emacs buffer. Required before using LSP tools like format on edited files."
  "Usage guidance for the find_file MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-find-file-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-find-file-tool-name)

(defcustom claude-code-ide-extras-emacs-position-point-usage-prompt
  "Move point in a buffer and save/restore positions. Enables workflows like: open file, move point, describe-at-point, restore."
  "Usage guidance for the position_point MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-position-point-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-position-point-tool-name)

(defcustom claude-code-ide-extras-emacs-select-region-usage-prompt
  "Select a region in a buffer. Region can be used with eval-elisp to call functions on it."
  "Usage guidance for the select_region MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-select-region-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-select-region-tool-name)

(defcustom claude-code-ide-extras-emacs-xref-find-definitions-at-point-usage-prompt
  "Find definition of symbol at specific location. Provides full semantic context for accurate results."
  "Usage guidance for the xref_find_definitions_at_point MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-xref-find-definitions-at-point-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-xref-find-definitions-at-point-tool-name)

(defcustom claude-code-ide-extras-emacs-xref-find-references-at-point-usage-prompt
  "Find all references to symbol at specific location. More reliable than string-based search."
  "Usage guidance for the xref_find_references_at_point MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-emacs)

(put 'claude-code-ide-extras-emacs-xref-find-references-at-point-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-emacs-xref-find-references-at-point-tool-name)

;;; Tool implementations

  ;; Custom MCP tools for Emacs introspection
  (defun claude-code-ide-extras-emacs--describe (name type)
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

  (defun claude-code-ide-extras-emacs--apropos (pattern)
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

  (defun claude-code-ide-extras-emacs--apropos-command (pattern)
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

  (defun claude-code-ide-extras-emacs--apropos-documentation (pattern)
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

  ;; Buffer access tools
  (defun claude-code-ide-extras-emacs--buffer-query (buffer-name &optional start-line num-lines)
    "Query buffer contents by line range.
BUFFER-NAME is the name of the buffer to query.
Optional START-LINE is the first line to retrieve (1-based, negative
counts from end).
Optional NUM-LINES is the number of lines to retrieve.
Both must be provided together or both omitted."
    (claude-code-ide-mcp-server-with-session-context nil
      (claude-code-ide-extras-common--buffer-query buffer-name start-line num-lines)))

  (defun claude-code-ide-extras-emacs--buffer-search (buffer-name pattern &optional context-lines)
    "Search buffer contents for pattern.
BUFFER-NAME is the name of the buffer to search.
PATTERN is a regular expression to search for.
Optional CONTEXT-LINES specifies lines of context before/after each match."
    (claude-code-ide-mcp-server-with-session-context nil
      (claude-code-ide-extras-common--buffer-search buffer-name pattern context-lines)))

  ;; Buffer-local variable tools
  (defun claude-code-ide-extras-emacs--get-buffer-local-keys (file-path &optional filter-regex)
    "Get buffer-local variable names for FILE-PATH.
Opens FILE-PATH and returns list of buffer-local variable names.
Optional FILTER-REGEX (Emacs regex) filters the returned names."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (claude-code-ide-extras-common--get-buffer-local-keys file-path filter-regex)
        (error (format "Error reading buffer-local keys for %s: %s"
                      file-path
                      (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--get-buffer-local-variables (file-path &optional filter-regex)
    "Get buffer-local variables with values for FILE-PATH.
Opens FILE-PATH and returns buffer-local-variables as a Lisp form.
Optional FILTER-REGEX (Emacs regex) filters variables by name before retrieving values."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (claude-code-ide-extras-common--get-buffer-local-variables file-path filter-regex)
        (error (format "Error reading buffer-local variables for %s: %s"
                      file-path
                      (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--read-dir-locals (file-path)
    "Read effective dir-local variables for FILE-PATH.
DEPRECATED: Delegates to get-buffer-local-variables for compatibility.
Opens FILE-PATH and returns buffer-local-variables as a Lisp form."
    ;; Simply delegate to the new function without filtering
    (claude-code-ide-extras-emacs--get-buffer-local-variables file-path nil))

  ;; Eval tools
  (defun claude-code-ide-extras-emacs--eval-elisp (code)
    "Execute arbitrary elisp CODE with FULL Emacs privileges.

Evaluates CODE with complete access to the filesystem, network, processes,
and all Emacs state. Operates under the assumption that Claude has been
explicitly granted access to the Emacs session for trusted projects.

CODE is a string containing elisp to evaluate (e.g., \"(+ 1 2)\").
Returns the result formatted as a string via %S.

Example:
  (claude-code-ide-extras-emacs--eval-elisp \"(buffer-list)\")
  => \"(#<buffer *scratch*> #<buffer init.el>)\"

Errors are caught and returned as formatted error strings."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((result (eval (read code))))
            (format "%S" result))
        (error (format "Error evaluating elisp: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--eval-region (buffer-name start-line start-column end-line end-column)
    "Evaluate elisp code in region of BUFFER-NAME with FULL Emacs privileges.

Region spans from START-LINE:START-COLUMN to END-LINE:END-COLUMN.
Lines are 1-based, columns are 0-based (consistent with Emacs conventions).

Uses the built-in `eval-region` function, which evaluates all forms in the
region for their side effects (like defining functions) but doesn't return
the value of the last form. Use this when reloading multiple function
definitions from a code file. Each defun is evaluated and the function is
redefined in Emacs, but the return values (which would just be the function
symbols) are discarded.

For computing something and getting the result back, use `eval-elisp` instead
with a progn form that returns the last expression's value. Both tools together
provide the right primitive for each use case."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((buffer (get-buffer buffer-name)))
            (if (not buffer)
                (format "Error: Buffer '%s' does not exist" buffer-name)
              (with-current-buffer buffer
                ;; Missing buffer is handled specially (clear error message) before
                ;; entering the evaluation context. All other errors (invalid elisp,
                ;; runtime errors during evaluation) are caught by the outer handler
                ;; and returned as formatted strings for Claude to interpret. This keeps
                ;; the MCP channel open even when elisp errors occur, allowing Claude
                ;; to see the error and adjust the approach.
                (let ((start-pos (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- start-line))
                                  (forward-char start-column)
                                  (point)))
                      (end-pos (save-excursion
                                (goto-char (point-min))
                                (forward-line (1- end-line))
                                (forward-char end-column)
                                (point))))
                  ;; eval-region evaluates each form in the region sequentially,
                  ;; installing side effects (like function definitions) but
                  ;; discarding return values. This is the right tool for
                  ;; reloading multiple defuns from a code file.
                  (eval-region start-pos end-pos)
                  (format "Evaluated region from %d:%d to %d:%d"
                         start-line start-column end-line end-column)))))
        (error (format "Error evaluating region: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--eval-defun-at-point (buffer-name line column)
    "Evaluate the defun at point in BUFFER-NAME at LINE:COLUMN with FULL Emacs privileges.

LINE is 1-based, COLUMN is 0-based (consistent with Emacs conventions).

Finds the top-level s-expression at the given position and evaluates it,
reloading the function definition. Essential for iterative development,
allowing Claude to modify a function definition and immediately reload it
without restarting Emacs or evaluating the entire file.

The point position doesn't need to be exact. As long as it's anywhere within
the function definition, eval-defun will find the enclosing top-level form
and evaluate it."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((buffer (get-buffer buffer-name)))
            (if (not buffer)
                (format "Error: Buffer '%s' does not exist" buffer-name)
              (with-current-buffer buffer
                (save-excursion
                  ;; Position point at requested location
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (forward-char column)
                  ;; eval-defun finds the top-level form enclosing point and
                  ;; evaluates it. This is more robust than requiring exact
                  ;; defun boundaries - Claude just needs to position within
                  ;; the function body.
                  (eval-defun nil)
                  (format "Evaluated defun at line %d, column %d" line column)))))
        (error (format "Error evaluating defun: %s" (error-message-string err))))))

  ;; Buffer manipulation tools
  (defun claude-code-ide-extras-emacs--find-file (file-path)
    "Open FILE-PATH into a buffer without displaying it to the user.

Uses find-file-noselect, which loads the file into a buffer but doesn't
change the user's window configuration. If the file is already open in a
buffer, reuses that existing buffer rather than creating a new one.

Primary use case is opening files before calling LSP operations like
lsp_format_buffer. When Claude edits a file using the Write tool, the file
exists on disk but may not be loaded in Emacs. LSP formatting requires an
active buffer with lsp-mode enabled. This tool ensures the file is loaded
so subsequent LSP operations work correctly.

The buffer will have all normal file-visiting behavior: major mode activation,
hooks, dir-locals, LSP attachment, etc. It's a real buffer, just not displayed
to the user.

FILE-PATH must be an absolute path to the file.
Returns a formatted string: \"Buffer: <buffer-name>\""
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((buffer (claude-code-ide-extras-common--prepare-buffer-for-file file-path)))
            (format "Buffer: %s" (buffer-name buffer)))
        (error (format "Error opening file %s: %s"
                      file-path
                      (error-message-string err))))))

  (defvar claude-code-ide-extras-emacs--point-markers (make-hash-table :test 'equal)
    "Hash table storing saved point positions as markers.

Markers are used (not integer positions) because markers automatically track
buffer changes. When text is inserted or deleted before a saved position, the
marker adjusts to maintain its logical location. Without this, restore would
jump to the wrong place after edits.

Keys are token strings (format: \"buffer-name-timestamp\"), values are marker
objects pointing into buffers. Markers are created on 'set' action via
point-marker, stored in the hash with a unique token, and cleaned up on
'restore' action via remhash.

Potential memory leak exists if Claude calls 'set' but never 'restore' (due
to error, user interruption, or forgetting). The marker persists in the hash
table, leaking a small amount of memory (marker object plus hash entry). This
is acceptable because typical usage is set/restore pairs within a single tool
call sequence, the leak rate is only about 100 bytes per leaked marker, and the
impact is negligible for normal sessions (less than 1MB even with 10000 leaks).
Markers are automatically invalidated if their buffer is killed, and restarting
Emacs clears the hash table.

Weak hash tables would enable auto-cleanup on GC, but Emacs doesn't support
weak keys for hash tables, and the implementation complexity doesn't justify
the benefit.")

  (defun claude-code-ide-extras-emacs--position-point (buffer-name action line column &optional token)
    "Position or restore point in BUFFER-NAME.
ACTION is either \"set\" or \"restore\".

For \"set\": Save current point position, move to LINE:COLUMN, return token.
  LINE is 1-based, COLUMN is 0-based (consistent with Emacs conventions).
  The saved position uses a marker that tracks buffer changes.

For \"restore\": Restore point to position saved with TOKEN.
  LINE and COLUMN are ignored for restore action.
  The marker is cleaned up after successful restore."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((buffer (get-buffer buffer-name)))
            (if (not buffer)
                (format "Error: Buffer '%s' does not exist" buffer-name)
              (with-current-buffer buffer
                (pcase action
                  ("set"
                   ;; Save current position as marker before moving.
                   ;; point-marker creates marker at current point that tracks
                   ;; buffer changes. This is essential for correct restoration
                   ;; after Claude makes edits between set and restore.
                   (let* ((old-marker (point-marker))
                          ;; Token includes timestamp for uniqueness. Multiple
                          ;; rapid set operations in same buffer need distinct tokens.
                          (token (format "%s-%d" buffer-name (float-time))))
                     (puthash token old-marker claude-code-ide-extras-emacs--point-markers)

                     ;; Move to requested position
                     (goto-char (point-min))
                     (forward-line (1- line))  ; 1-based to 0-based
                     (forward-char column)      ; 0-based column

                     (format "Point moved to line %d, column %d. Token: %s" line column token)))

                  ("restore"
                   (if (not token)
                       "Error: TOKEN required for restore action"
                     (let ((saved-marker (gethash token claude-code-ide-extras-emacs--point-markers)))
                       (if (not saved-marker)
                           (format "Error: Invalid token '%s'" token)
                         ;; Marker can become invalid if buffer was killed
                         (if (not (marker-position saved-marker))
                             (progn
                               ;; Clean up invalid marker
                               (remhash token claude-code-ide-extras-emacs--point-markers)
                               (format "Error: Saved position for token '%s' is no longer valid" token))
                           ;; Success path: restore point and cleanup
                           (goto-char saved-marker)
                           (remhash token claude-code-ide-extras-emacs--point-markers)
                           (format "Point restored using token %s" token))))))

                  ;; Invalid action
                  (_ (format "Error: ACTION must be 'set' or 'restore', got '%s'" action))))))
        ;; Top-level error handler for unexpected failures
        (error (format "Error positioning point: %s" (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--select-region (buffer-name start-line start-column end-line end-column)
    "Select region in BUFFER-NAME from START-LINE:START-COLUMN to END-LINE:END-COLUMN.
Lines are 1-based, columns are 0-based (consistent with Emacs conventions).

Sets mark at start position and moves point to end position, then activates
the region for subsequent operations."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((buffer (get-buffer buffer-name)))
            (if (not buffer)
                (format "Error: Buffer '%s' does not exist" buffer-name)
              (with-current-buffer buffer
                ;; Calculate both positions first to avoid redundant navigation
                (let ((start-pos (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- start-line))
                                  (forward-char start-column)
                                  (point)))
                      (end-pos (save-excursion
                                (goto-char (point-min))
                                (forward-line (1- end-line))
                                (forward-char end-column)
                                (point))))
                  ;; Set mark at start, move point to end, and activate
                  (goto-char start-pos)
                  (set-mark (point))
                  (goto-char end-pos)
                  (activate-mark)
                  (format "Region selected from %d:%d to %d:%d"
                         start-line start-column end-line end-column)))))
        (error (format "Error selecting region: %s" (error-message-string err))))))

  ;; Xref navigation tools
  (defun claude-code-ide-extras-emacs--xref-find-definitions-at-point (file-path line column)
    "Find definitions of symbol at FILE-PATH:LINE:COLUMN.

Positions point at the specified location, queries the xref backend for the
symbol's identifier at that point, then asks the backend for definitions.
This provides full semantic context, enabling accurate resolution of overloads
and namespace-qualified names that would be ambiguous as bare strings.

LINE is 1-based, COLUMN is 0-based (Emacs conventions).

Returns a list of definition locations formatted as \"file:line: summary\".
If no definitions found, returns a message indicating that."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (claude-code-ide-extras-common--prepare-buffer-for-file file-path)))
            (with-current-buffer target-buffer
              (save-excursion
                ;; Position point at the requested location to establish context
                (goto-char (point-min))
                (forward-line (1- line))
                (move-to-column column)

                ;; Get the xref backend for this buffer (LSP, etags, elisp-mode, etc.)
                (let ((backend (xref-find-backend)))
                  (if (not backend)
                      (format "No xref backend available for %s" file-path)
                    ;; Ask backend what identifier is at current point position.
                    ;; The backend understands the syntax and semantics of the current
                    ;; language, returning a string identifier it can work with.
                    (let ((identifier (xref-backend-identifier-at-point backend)))
                      (if (not identifier)
                          (format "No identifier at %s:%d:%d" file-path line column)
                        ;; Query backend for definitions of this identifier.
                        ;; Returns list of xref-item objects with location information.
                        (let ((xref-items (xref-backend-definitions backend identifier)))
                          (if xref-items
                              ;; Format each xref item as "file:line: summary"
                              (mapcar (lambda (item)
                                        (let* ((location (xref-item-location item))
                                               (file (xref-location-group location))
                                               (marker (xref-location-marker location))
                                               (line (with-current-buffer (marker-buffer marker)
                                                       (save-excursion
                                                         (goto-char marker)
                                                         (line-number-at-pos))))
                                               (summary (xref-item-summary item)))
                                          (format "%s:%d: %s" file line summary)))
                                      xref-items)
                            (format "No definitions found for '%s' at %s:%d:%d"
                                    identifier file-path line column))))))))))
        (error (format "Error finding definitions at %s:%d:%d: %s"
                       file-path line column (error-message-string err))))))

  (defun claude-code-ide-extras-emacs--xref-find-references-at-point (file-path line column)
    "Find references to symbol at FILE-PATH:LINE:COLUMN.

Positions point at the specified location, queries the xref backend for the
symbol's identifier at that point, then asks the backend for all references.
Using point position provides semantic context that disambiguates overloaded
names and resolves namespace-qualified identifiers correctly.

LINE is 1-based, COLUMN is 0-based (Emacs conventions).

Returns a list of reference locations formatted as \"file:line: summary\".
If no references found, returns a message indicating that."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((target-buffer (claude-code-ide-extras-common--prepare-buffer-for-file file-path)))
            (with-current-buffer target-buffer
              (save-excursion
                ;; Position point to establish semantic context
                (goto-char (point-min))
                (forward-line (1- line))
                (move-to-column column)

                ;; Get the xref backend appropriate for this buffer's language
                (let ((backend (xref-find-backend)))
                  (if (not backend)
                      (format "No xref backend available for %s" file-path)
                    ;; Ask backend for identifier at point with full semantic context
                    (let ((identifier (xref-backend-identifier-at-point backend)))
                      (if (not identifier)
                          (format "No identifier at %s:%d:%d" file-path line column)
                        ;; Query backend for all references to this identifier
                        (let ((xref-items (xref-backend-references backend identifier)))
                          (if xref-items
                              ;; Format results consistently with other xref tools
                              (mapcar (lambda (item)
                                        (let* ((location (xref-item-location item))
                                               (file (xref-location-group location))
                                               (marker (xref-location-marker location))
                                               (line (with-current-buffer (marker-buffer marker)
                                                       (save-excursion
                                                         (goto-char marker)
                                                         (line-number-at-pos))))
                                               (summary (xref-item-summary item)))
                                          (format "%s:%d: %s" file line summary)))
                                      xref-items)
                            (format "No references found for '%s' at %s:%d:%d"
                                    identifier file-path line column))))))))))
        (error (format "Error finding references at %s:%d:%d: %s"
                       file-path line column (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-emacs-setup ()
  "Register all Emacs introspection MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--describe
   :name claude-code-ide-extras-emacs-describe-tool-name
   :description "Get documentation for an Emacs symbol. Returns docstring, current value (for variables), arguments (for functions), and other metadata from the running Emacs session."
   :args '((:name "name"
            :type string
            :description "The name of the symbol to describe (e.g., 'projectile-compile-project', 'lsp-mode').")
           (:name "type"
            :type string
            :description "The type of thing to describe: function, variable, mode, package, or symbol. Use 'symbol' for a unified view of all aspects.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--apropos
   :name claude-code-ide-extras-emacs-apropos-tool-name
   :description "Search for all Emacs symbols (functions, variables, faces, etc.) matching a pattern. Use for broad exploration."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match symbol names.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--apropos-command
   :name claude-code-ide-extras-emacs-apropos-command-tool-name
   :description "Search for interactive Emacs commands (callable via M-x) matching a pattern. More focused than emacs_apropos - only returns commands users can invoke."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match command names.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--apropos-documentation
   :name claude-code-ide-extras-emacs-apropos-documentation-tool-name
   :description "Search Emacs documentation text for a pattern. Finds functions/variables whose docstrings contain the pattern. Use for concept-based search (e.g., 'buffer naming', 'code formatting')."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match in documentation text.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--buffer-query
   :name claude-code-ide-extras-emacs-buffer-query-tool-name
   :description "Read contents from any Emacs buffer by line range. Lines are 1-based (line 1 is first line). Negative start_line counts from end (-100 = 100th line from end). Lines longer than the configured maximum are truncated. Use for reading compilation output, scratch buffers, message logs, or any other buffer contents. Both start_line and num_lines must be provided together or both omitted."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer to read (e.g., '*scratch*', '*Messages*', '*compilation*').")
           (:name "start_line"
            :type number
            :description "First line to read (1-based, negative counts from end). Must be provided with num_lines."
            :optional t)
           (:name "num_lines"
            :type number
            :description "Number of lines to read starting from start_line. Must be provided with start_line."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--buffer-search
   :name claude-code-ide-extras-emacs-buffer-search-tool-name
   :description "Search any Emacs buffer for a pattern using regular expressions. Returns matching lines with optional context. Use for finding specific content in compilation output, logs, scratch buffers, or any other buffer."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer to search (e.g., '*scratch*', '*Messages*', '*compilation*').")
           (:name "pattern"
            :type string
            :description "Regular expression pattern to search for.")
           (:name "context_lines"
            :type number
            :description "Number of context lines before and after each match (optional, default 0)."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--get-buffer-local-keys
   :name claude-code-ide-extras-emacs-get-buffer-local-keys-tool-name
   :description "Get buffer-local variable names for a file. Returns only names (lightweight discovery). Optional filter_regex (Emacs regex) to narrow results. Much cheaper than getting full variables. Use for discovery, then get_buffer_local_variables with filter for specific values."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file to read buffer-local variable names for.")
           (:name "filter_regex"
            :type string
            :description "Optional Emacs regular expression to filter variable names (e.g., \"^projectile-\" or \"^\\\\(projectile\\\\|lsp\\\\)-\")."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--get-buffer-local-variables
   :name claude-code-ide-extras-emacs-get-buffer-local-variables-tool-name
   :description "Get buffer-local variables with values for a file. Returns buffer-local-variables as a Lisp form. Optional filter_regex (Emacs regex) to limit results. WARNING: Without filtering, this can be very context-expensive (10k+ tokens). STRONGLY RECOMMENDED: Use filter_regex to get only relevant variables."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file to read buffer-local variables for.")
           (:name "filter_regex"
            :type string
            :description "Optional Emacs regular expression to filter variables by name (e.g., \"^projectile-\" or \"^\\\\(projectile\\\\|lsp\\\\)-\")."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--read-dir-locals
   :name claude-code-ide-extras-emacs-read-dir-locals-tool-name
   :description "DEPRECATED: Use get_buffer_local_keys/get_buffer_local_variables instead. Read buffer-local variables for a specific file path. Opens the file and returns ALL buffer-local-variables as a Lisp form. WARNING: Very context-expensive (10k+ tokens). The new tools support filtering and discovery patterns."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file or directory to read buffer-local variables for.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--eval-elisp
   :name claude-code-ide-extras-emacs-eval-elisp-tool-name
   :description "Execute arbitrary elisp code and return the result. Powerful tool for exploring Emacs state, testing code, and iteratively redefining functions during development. WARNING: Executes arbitrary elisp with full Emacs privileges."
   :args '((:name "code"
            :type string
            :description "Elisp code as a string to evaluate (e.g., \"(+ 1 2)\", \"(buffer-list)\").")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--eval-region
   :name claude-code-ide-extras-emacs-eval-region-tool-name
   :description "Evaluate a region of elisp code in a buffer. Lines are 1-based, columns are 0-based (Emacs convention). Useful for testing code snippets. WARNING: Executes arbitrary elisp with full Emacs privileges."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer containing the code.")
           (:name "start_line"
            :type number
            :description "Starting line number (1-based).")
           (:name "start_column"
            :type number
            :description "Starting column number (0-based).")
           (:name "end_line"
            :type number
            :description "Ending line number (1-based).")
           (:name "end_column"
            :type number
            :description "Ending column number (0-based).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--eval-defun-at-point
   :name claude-code-ide-extras-emacs-eval-defun-at-point-tool-name
   :description "Evaluate the function definition at a specific point in a buffer. Essential for reloading function definitions during iterative development. Line is 1-based, column is 0-based (Emacs convention). WARNING: Executes arbitrary elisp with full Emacs privileges."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer containing the function.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--find-file
   :name claude-code-ide-extras-emacs-find-file-tool-name
   :description "Open a file into an Emacs buffer without displaying it to the user. Required before using LSP tools like format on files that have been edited but not yet opened. Returns the buffer name."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to open.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--position-point
   :name claude-code-ide-extras-emacs-position-point-tool-name
   :description "Move point in a buffer and save/restore positions. Use 'set' action to move point and get a token, then 'restore' action with the token to return to original position. Enables workflows like: find-file, position-point set, describe-at-point, position-point restore."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer.")
           (:name "action"
            :type string
            :description "Either 'set' to move and save position, or 'restore' to return to saved position.")
           (:name "line"
            :type number
            :description "Line number (1-based). Required for 'set' action, ignored for 'restore'.")
           (:name "column"
            :type number
            :description "Column number (0-based). Required for 'set' action, ignored for 'restore'.")
           (:name "token"
            :type string
            :description "Token returned by previous 'set' action. Required for 'restore' action."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--select-region
   :name claude-code-ide-extras-emacs-select-region-tool-name
   :description "Select a region in a buffer. Sets mark and point, activating the region for subsequent operations. Use with eval-elisp to call functions on the region."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer.")
           (:name "start_line"
            :type number
            :description "Starting line number (1-based).")
           (:name "start_column"
            :type number
            :description "Starting column number (0-based).")
           (:name "end_line"
            :type number
            :description "Ending line number (1-based).")
           (:name "end_column"
            :type number
            :description "Ending column number (0-based).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--xref-find-definitions-at-point
   :name claude-code-ide-extras-emacs-xref-find-definitions-at-point-tool-name
   :description "Find definitions of symbol at a specific location. Uses point position to provide full semantic context, enabling accurate resolution of overloads and namespace-qualified names. More reliable than string-based symbol search."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-emacs--xref-find-references-at-point
   :name claude-code-ide-extras-emacs-xref-find-references-at-point-tool-name
   :description "Find all references to symbol at a specific location. Uses point position for semantic disambiguation, making it more reliable than string-based identifier search. Essential for finding all usages of a symbol."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")))

  (message "Claude Code IDE Extras: Emacs tools registered"))

(provide 'claude-code-ide-extras-emacs)
;;; claude-code-ide-extras-emacs.el ends here
