# claude-code-ide-extras

Additional Emacs MCPs for [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el).

## "Author's" Note

This repository was written by, for, and with Claude Code, in Emacs,
using `claude-code-ide.el`. However, any mistakes are mine.

This project started when I got frustrated watching Claude Code
running in a `claude-code-ide.el` session repeatedly run
compilation/tests in `Bash` tool invocations while failing to converge
on a solution. I couldn't see the same results it saw. It seemed to me
that it would be better if it would just re-use my existing projectile
compilation buffer, so we had a shared understanding and I could
interrupt it and steer it in the right direction when it was going off
the rails. So, I started developing the `extras-projectile`
functionality. Given my limited elisp skills, it was natural to have
Claude Code do what it does.

While iterating on those tools, it became apparent that Claude Code
didn't always have a great understanding of the various Emacs
APIs. Given the self-documenting nature of Emacs, it again seemed
obvious that tools could be provided to allow Claude Code to interact
with the `describe` and `apropos` subsystems. This made it much easier
to steer Claude in the right direction while developing these tools. I
could ask it to study `projectile-per-project-compilation-buffer` and
it would now be able to make use of that symbol. So the `extras-emacs`
package started there. At some point, the buffer search and query
tools that had originally been developed as part of the
`extras-projectile` package moved here and were generalized to allow
interaction with any buffer: there were times I wanted Claude Code to
be able to meditate on content from the `*Messages*` buffer, for
instance.

The `extras-lsp` commands were natural extensions to the LSP MCPs that
come with `claude-code-ide.el`. In particular, giving Claude Code the
ability to run the formatter in Emacs avoids irritating back and forth
between Claude Code and the formatter.

At some point I noticed that I'd written fairly extensive guidance for
Claude Code on how to use these tools into my personal `CLAUDE.md`
files. Allowing the tool guidance to live in Emacs customizations was
an obvious next step: you do need to explain to Claude Code how and
when to use these tools, everyone will have different guidance, and
the guidance is for this package. Each tool has a customizable usage
guidance `defcustom`, and there is a meta MCP that collates the
content for each active tool. You can customize this globally using
`:custom`, and then also override it using `dir-locals` or augment it
with per-project notes in your `CLAUDE.md` files. You do need at least
one `CLAUDE.md` that exhorts Claude Code to run the meta MCP to learn
of the others, or you can put that in a custom slash command. The
default guidance is minimal and should almost certainly be customized.

Since the initial release, many of these ideas have been realized:
eval_elisp provides direct elisp evaluation, and Eglot support brings
additional LSP integrations (code actions, workspace symbols, rename,
and more) for users who prefer built-in Emacs packages. A project.el
backend was also added as an alternative to projectile.

Finally, a note on security: there is none. As I started writing these
tools, I kept writing down notes on how I should later come back and
"secure" various parts of this. In the end, I was convinced that this
was entirely security theater. As soon as Claude Code has access to
the `Bash` tool it is game over for anything in its reachable
environment. In particular, the `Bash` tool can run `emacsclient -e
EVIL` to run any evil elisp it wants inside your running Emacs
session. If Claude Code wants to own you or anything your Emacs
session can reach, it has had that full power from the first time you
ran it and granted it permissions to run the `Bash` tool. Take
appropriate precautions.

The remainder of this `README.md` was written by Claude Code.

## Overview

This repository provides eleven packages that extend claude-code-ide with additional MCP tools:

**claude-code-ide-extras** (meta-package)
Convenience package that installs and configures all extension packages at once. Automatically detects which optional dependencies are available and registers only their tools.

**claude-code-ide-extras-emacs**
Emacs introspection and buffer access:
- Function and variable documentation
- Command and symbol discovery
- Documentation search across all loaded packages
- Direct buffer read and search capabilities
- Elisp evaluation and function reloading
- Point/region manipulation
- Cross-reference navigation (definitions and references)
- Efficient buffer-local variable discovery and retrieval with filtering

**claude-code-ide-extras-projectile** (optional, requires projectile)
Project-aware development tools via Projectile integration:
- Asynchronous build and test execution
- Compilation output query and search
- Shell command execution in project context
- Efficient buffer-local variable discovery and retrieval with filtering
- Project file enumeration

**claude-code-ide-extras-project** (optional, requires project.el / Emacs 28+)
Project-aware development tools via built-in project.el:
- Asynchronous build and test execution
- Compilation output query and search
- Shell command execution in project context
- Efficient buffer-local variable discovery and retrieval with filtering
- Project file enumeration

**claude-code-ide-extras-lsp** (optional, requires lsp-mode)
Semantic code understanding via lsp-mode integration:
- Buffer formatting through language servers
- Hover information and type signatures
- Call hierarchy navigation (incoming callers / outgoing callees)
- Find implementations of interfaces and abstract methods
- Jump to type definitions

**claude-code-ide-extras-eglot** (optional, requires eglot / Emacs 29+)
Semantic code understanding via built-in Eglot integration:
- Buffer formatting through language servers
- Hover information and type signatures
- LSP code action listing and execution
- Workspace and document symbol search
- Semantic rename across the project
- Call hierarchy navigation (incoming callers / outgoing callees)
- Find implementations of interfaces and abstract methods
- Jump to type definitions

**claude-code-ide-extras-meta**
Meta-level tools about the MCP tools themselves:
- Customizable per-tool usage guidance
- Project-specific configuration via dir-locals

**claude-code-ide-extras-buffers**
Buffer listing and inspection:
- List all open buffers with mode and modification status
- Get detailed buffer info (encoding, process, minor modes)

**claude-code-ide-extras-diagnostics** (optional, requires flymake or flycheck)
Diagnostic error reporting:
- Retrieve flycheck/flymake diagnostics grouped by severity
- Summary counts across all buffers
- Trigger re-checks after code changes
- Parse compilation buffer errors into structured output

**claude-code-ide-extras-git** (optional, requires git)
Git integration via vc.el (no magit dependency):
- Repository status, diff, log, and blame
- Stash management (list, push, pop, show, drop)
- Branch listing with tracking info

**claude-code-ide-extras-testing**
Test discovery and execution:
- List and run ERT tests with pass/fail/skip reporting
- Buttercup BDD spec discovery and execution
- Batch test runs with summary and failure details

**claude-code-ide-extras-files**
File system metadata without loading buffers:
- File stat (size, permissions, type, modification time)
- Directory listing with optional glob filtering
- Recursive file search by regex pattern

**claude-code-ide-extras-common** (internal library)
Shared utilities used by other packages. Not intended for direct use.

## Design Philosophy

These tools operate on Emacs buffers rather than external processes, providing uniform query interfaces for compilation output, test results, and shell commands. The buffer-centric approach creates a consistent interaction model for both human users and AI assistants.

## Requirements

- Emacs 29.1+
- claude-code-ide 0.1+

Optional (modules load automatically when available):
- projectile 2.9.1+ (for projectile package)
- project.el (built-in with Emacs 28+, for project package)
- lsp-mode 20251112.625+ (for lsp package)
- eglot (built-in with Emacs 29+, for eglot package)
- flymake (built-in) or flycheck (for diagnostics package)
- buttercup (for BDD-style testing support)
- git (for git package)

## Installation

### Local Development

Install from local checkout using package-vc:

```elisp
(let ((extras-dir (expand-file-name "dev/claude-code-ide-extras" user-emacs-directory)))
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-emacs")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-lsp")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-eglot")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-meta")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-projectile")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-project")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-buffers")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-diagnostics")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-git")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-testing")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-files")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras"))

(use-package claude-code-ide-extras
  :after claude-code-ide
  :demand t
  :config
  (claude-code-ide-extras-setup))
```

### Via MELPA

Once published to MELPA:

```elisp
(use-package claude-code-ide-extras
  :ensure t
  :after claude-code-ide
  :demand t
  :config
  (claude-code-ide-extras-setup))
```

### Individual Packages

Install only the packages you need:

```elisp
;; Core Emacs introspection (always available)
(use-package claude-code-ide-extras-emacs
  :after claude-code-ide
  :demand t
  :config
  (claude-code-ide-extras-emacs-setup))

;; Eglot LSP tools (for built-in Eglot users)
(use-package claude-code-ide-extras-eglot
  :after (claude-code-ide eglot)
  :demand t
  :config
  (claude-code-ide-extras-eglot-setup))

;; project.el tools (for built-in project.el users)
(use-package claude-code-ide-extras-project
  :after claude-code-ide
  :demand t
  :config
  (claude-code-ide-extras-project-setup))
```

## Available Tools

### Emacs (17 tools)

**describe** - Get documentation for Emacs symbols
Supports functions, variables, modes, packages, and symbols.

**apropos** - Search all symbols by name pattern

**apropos_command** - Search interactive commands only

**apropos_documentation** - Search symbol documentation text

**buffer_query** - Read contents from any Emacs buffer
Line-range queries with 1-based indexing and negative offset support. Enables scratch buffer collaboration and message log analysis.

**buffer_search** - Search any Emacs buffer with regex
Search compilation, scratch, messages, or any other buffer with optional context lines.

**get_buffer_local_keys** - List buffer-local variable names for a file
Lightweight discovery returning only variable names. Optional Emacs regex filtering. Much cheaper than getting full variables.

**get_buffer_local_variables** - Get buffer-local variables with values for a file
Returns variables as Lisp form. Optional Emacs regex filtering. WARNING: Without filtering, can be very context-expensive (10k+ tokens).

**read_dir_locals** - DEPRECATED: Read buffer-local variables for a file
Use get_buffer_local_keys/get_buffer_local_variables instead. Returns ALL variables (very expensive).

**eval_elisp** - Execute arbitrary elisp code
Powerful tool for exploring Emacs state, testing code, and iteratively redefining functions.

**eval_region** - Evaluate elisp region in a buffer
Takes buffer name, start/end line and column, evaluates the code and returns result.

**eval_defun_at_point** - Evaluate function definition at point
Essential for reloading function definitions during iterative development.

**find_file** - Open file into buffer
Opens file without displaying it. Required before using LSP tools on edited files.

**position_point** - Move and restore point position
Set action moves point and returns token. Restore action returns point to saved position.

**select_region** - Select region in buffer
Sets mark and point, activating region. Use with eval-elisp to call functions on the region.

**xref_find_definitions_at_point** - Find definitions of symbol at location
Uses point position for full semantic context, enabling accurate resolution of overloads and namespace-qualified names. More reliable than string-based symbol search.

**xref_find_references_at_point** - Find all references to symbol at location
Point-based for semantic disambiguation. Essential for finding all usages of a symbol.

### Projectile (9 tools, optional)

**task_start** - Launch project tasks (compile, test, configure, install, package, run)
Returns immediately with buffer name while task runs asynchronously.

**task_wait** - Poll for task completion and retrieve output size
Returns status and line/character counts when finished.

**task_query** - Retrieve compilation output
Supports line range queries (start + count) with negative indexing for tail access.

**task_search** - Search compilation output with regex patterns
Returns matching lines with optional context.

**task_kill** - Terminate running compilation

**get_project_buffer_local_keys** - List buffer-local variable names for project root
Lightweight discovery returning only variable names. Optional Emacs regex filtering. Much cheaper than getting full variables.

**get_project_buffer_local_variables** - Get buffer-local variables with values for project root
Returns variables as Lisp form. Optional Emacs regex filtering. WARNING: Without filtering, can be very context-expensive (10k+ tokens).

**read_project_dir_locals** - DEPRECATED: Query project-level directory-local variables
Use get_project_buffer_local_keys/get_project_buffer_local_variables instead. Returns ALL variables (very expensive).

**get_project_files** - Enumerate all files in project
Returns list of relative paths. Uses projectile cache for speed, respects ignore rules from .projectile and .gitignore.

### project.el (9 tools, optional)

**task_start** - Launch project tasks (compile, test, configure, install, package, run)
Returns immediately with buffer name while task runs asynchronously.

**task_wait** - Poll for task completion and retrieve output size
Returns status and line/character counts when finished.

**task_query** - Retrieve compilation output
Supports line range queries (start + count) with negative indexing for tail access.

**task_search** - Search compilation output with regex patterns
Returns matching lines with optional context.

**task_kill** - Terminate running compilation

**get_project_buffer_local_keys** - List buffer-local variable names for project root
Lightweight discovery returning only variable names. Optional Emacs regex filtering.

**get_project_buffer_local_variables** - Get buffer-local variables with values for project root
Returns variables as Lisp form. Optional Emacs regex filtering. WARNING: Without filtering, can be very context-expensive (10k+ tokens).

**read_project_dir_locals** - DEPRECATED: Query project-level directory-local variables
Use get_project_buffer_local_keys/get_project_buffer_local_variables instead.

**get_project_files** - Enumerate all files in project
Returns list of file paths. Uses project.el's file discovery, respects .gitignore rules.

### LSP (5 tools, optional)

**format_buffer** - Format file using LSP formatting

**describe_thing_at_point** - Get hover information at specific location
Returns type signatures, parameter lists, and documentation.

**call_hierarchy** - Show incoming callers or outgoing callees
Uses the two-step LSP call hierarchy protocol. Specify direction as "incoming" or "outgoing".

**find_implementations** - Find implementations of interfaces and abstract methods
Returns file:line locations for all implementations.

**type_definition** - Jump to type definition of a symbol
Returns the file:line location where the type is defined.

### Eglot (9 tools, optional)

**format_buffer** - Format file using Eglot LSP formatting

**describe_thing_at_point** - Get hover information at specific location
Returns type signatures, parameter lists, and documentation.

**code_actions** - List or execute LSP code actions at a position
Without action_title, lists available actions. With action_title, executes the matching action. Optional action_kind filter (e.g. "quickfix", "refactor.extract").

**workspace_symbols** - Search for symbols across the workspace
Returns matching symbol names, kinds, locations, and containers.

**document_symbols** - Get structured outline of symbols in a file
Returns hierarchical symbol names, kinds, and line ranges.

**rename** - Rename a symbol across the project
Applies changes to all references and saves affected buffers.

**call_hierarchy** - Show incoming callers or outgoing callees
Uses the two-step LSP call hierarchy protocol. Specify direction as "incoming" or "outgoing".

**find_implementations** - Find implementations of interfaces and abstract methods
Returns file:line locations for all implementations.

**type_definition** - Jump to type definition of a symbol
Returns the file:line location where the type is defined.

### Buffers (2 tools)

**list_buffers** - List all open buffers with metadata
Shows buffer name, file path, major mode, modified status, and size. Optional filters for major mode (regex) and modified-only.

**get_buffer_info** - Get detailed info about a specific buffer
Returns name, file path, major mode, active minor modes, modified/read-only status, size, line count, encoding, and associated process.

### Diagnostics (4 tools, optional)

**get_diagnostics** - Get all diagnostics for a file or buffer
Returns errors, warnings, and info/notes from flymake or flycheck, grouped by severity. Format: file:line:col: level: message (checker).

**get_diagnostics_summary** - Get diagnostic counts
Summary of error/warning/info counts for a specific file or across all buffers.

**recheck_diagnostics** - Trigger a diagnostic re-check
Forces flymake or flycheck to re-run its checkers on a file or buffer. Use after making changes.

**get_compilation_errors** - Parse errors from compilation buffers
Extracts structured error locations from `*compilation*` or other compilation-mode buffers using Emacs' built-in compilation parser.

### Git (6 tools, optional)

**git_status** - Get git status of the project
Shows current branch, staged, modified, and untracked files.

**git_diff** - Get git diff output
Supports file-specific or project-wide diffs. Use staged="true" for cached changes.

**git_log** - Get recent commit history
Shows hash, author, date, and subject. Optional file filter and count limit.

**git_blame** - Get git blame for a file
Shows commit hash, author, date, and line content. Optional line range.

**git_stash** - Manage git stashes
List, show, push, pop, or drop stashes. Supports stash messages.

**git_branch** - List git branches
Shows all branches with commit hashes and remote tracking info.

### Testing (4 tools)

**list_tests** - Discover test functions
Lists ERT tests and Buttercup specs. Optionally load a file first, filter by regex.

**run_test** - Run a single ERT test
Returns pass/fail/skip status with failure condition and backtrace.

**run_tests** - Run multiple tests matching a pattern
Supports both ERT and Buttercup. Returns summary with counts and failure details.

**run_buttercup_spec** - Run a single Buttercup spec by name
Returns pass/fail/pending status with failure details.

### Files (3 tools)

**file_stat** - Get file metadata
Returns name, directory, type, size, permissions, modification time, owner, and symlink target.

**list_directory** - List directory contents
Shows name, type, size, and modification time. Optional glob pattern filter and recursive mode.

**find_files** - Find files matching a pattern
Searches a directory tree by regex with configurable result limit.

### Meta (1 tool)

**get_mcp_custom_advice** - Retrieve project-specific tool usage guidance
Reads customization variables configured via :custom or .dir-locals.el, returning guidance for all registered MCP tools.

## Usage Examples

### Building and Testing

Claude can execute builds asynchronously and examine the results:

```
User: "Build the project and show me any errors"

Claude uses:
1. read_project_dir_locals - discover build command
2. task_start - launch build
3. task_wait - poll until complete
4. task_search - find error patterns in output
```

### Code Understanding

Claude can query LSP for semantic information:

```
User: "What's the type of that variable?"

Claude uses:
1. describe_thing_at_point - get hover info at cursor position
```

### Emacs Learning

Claude can discover and understand Emacs functionality:

```
User: "How does compilation-mode work?"

Claude uses:
1. apropos - find compilation-related symbols
2. describe - read documentation for specific functions
```

### Interactive Development

Claude can iteratively develop and test elisp functions:

```
User: "Add error handling to the parse-config function"

Claude uses:
1. find_file - open the elisp file into buffer
2. position_point (set) - move to function, get token
3. Read tool - view function definition
4. Edit tool - modify function with error handling
5. eval_defun_at_point - reload the modified function
6. eval_elisp - test the function with sample data
7. position_point (restore) - return point to original position
8. lsp/format_buffer - format the modified code
```

### Region Operations

Claude can select and operate on code regions:

```
User: "Comment out lines 50-60 in buffer.el"

Claude uses:
1. find_file - ensure file is open
2. select_region - mark lines 50-60
3. eval_elisp - call (comment-region (region-beginning) (region-end))
```

## Customizing Tool Guidance

The meta package provides a customization system for providing Claude with project-specific guidance on how to use MCP tools effectively.

### Per-Tool Guidance

Each tool has a `-usage-prompt` defcustom that you can set to provide usage guidance. For example:

```elisp
(use-package claude-code-ide-extras-projectile
  :custom
  (claude-code-ide-extras-projectile-task-start-usage-prompt
   "Launch builds with 'compile' type, tests with 'test' type"))
```

### Cross-Cutting Guidance

The `claude-code-ide-extras-meta-get-mcp-custom-advice-header` defcustom provides guidance that applies across multiple tools (workflows, tool selection principles, etc.):

```elisp
(use-package claude-code-ide-extras-meta
  :custom
  (claude-code-ide-extras-meta-get-mcp-custom-advice-header
   "# MCP Tools\n\nPrefer semantic tools over text search..."))
```

### Loading from Files

You can load guidance from markdown files for easier editing:

```elisp
(defun my/load-guidance (filename)
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename user-emacs-directory))
    (buffer-string)))

(use-package claude-code-ide-extras-meta
  :custom
  (claude-code-ide-extras-meta-get-mcp-custom-advice-header
   (my/load-guidance "claude-mcp-guidance/meta/header.md")))
```

Claude retrieves this guidance by calling `get_mcp_custom_advice()` at session start.

## Security Model

This package grants Claude access to:
- **Execute arbitrary shell commands** in project context via `task_start`
- **Read any Emacs buffer** including scratch buffers, compilation output, and logs
- **Evaluate arbitrary elisp** in your running Emacs session via `eval_elisp`
- **Format, rename, and modify files** via LSP integration (lsp-mode or Eglot)
- **Run git commands** including stash push/pop and branch operations
- **Execute tests** and inspect results (ERT and Buttercup)
- **Access file system metadata** and directory listings

**Intended use**: Personal development environments and work machines with trusted projects and codebases.

**Not suitable for**: Shared machines, untrusted codebases, production servers, or environments with sensitive data in Emacs buffers.

### Trust Model

The security philosophy is: "Claude is your pair programmer with the same access you have."

- **No sandboxing**: Shell commands execute with full user privileges
- **No secrets filtering**: Could read `.env`, credentials files, or sensitive buffers if accessed
- **No audit trail**: Commands are not logged (relies on shell history)
- **Trust-based execution**: Assumes you trust both Claude and the projects you work on

This model is appropriate for personal development workflows where you would grant the same access to a human pair programmer.

## Architecture

The package suite uses a layered architecture:

- Common library provides shared buffer utilities (search, query)
- Individual packages implement domain-specific MCP tools
- Optional modules (projectile/project.el, lsp-mode/eglot, diagnostics, git) load conditionally based on available dependencies
- Always-available modules (buffers, files, testing) provide core functionality without external dependencies
- Meta-package provides unified installation and setup with automatic detection

Users choose one project backend (projectile or project.el) and one LSP backend (lsp-mode or eglot) based on their existing configuration. The meta-package handles this automatically.

All tool implementations use `claude-code-ide-mcp-server-with-session-context` to integrate with the claude-code-ide framework.

## License

GPL-3.0-or-later

See [LICENSE](LICENSE) for full license text.
