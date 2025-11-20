# claude-code-ide-extras

MCP tool extensions for [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el).

## Overview

This repository provides four packages that extend claude-code-ide with additional MCP tools:

**claude-code-ide-extras** (meta-package)
Convenience package that installs and configures all extension packages at once.

**claude-code-ide-extras-projectile**
Project-aware development tools via Projectile integration:
- Asynchronous build and test execution
- Compilation output query and search
- Shell command execution in project context
- Project configuration discovery via dir-locals

**claude-code-ide-extras-lsp**
Semantic code understanding via LSP integration:
- Buffer formatting through language servers
- Hover information and type signatures
- Documentation lookup at point

**claude-code-ide-extras-core**
Emacs introspection and help system access:
- Function and variable documentation
- Command and symbol discovery
- Documentation search across all loaded packages

**claude-code-ide-extras-common** (internal library)
Shared utilities used by other packages. Not intended for direct use.

## Design Philosophy

These tools operate on Emacs buffers rather than external processes, providing uniform query interfaces for compilation output, test results, and shell commands. The buffer-centric approach creates a consistent interaction model for both human users and AI assistants.

## Requirements

- Emacs 30.1+
- claude-code-ide (unversioned)
- projectile 2.9.1+ (for projectile package)
- lsp-mode 20251112.625+ (for lsp package)

## Installation

### Local Development

Install from local checkout using package-vc:

```elisp
(let ((extras-dir (expand-file-name "dev/claude-code-ide-extras" user-emacs-directory)))
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-core")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-lsp")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-projectile")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras"))

(use-package claude-code-ide-extras
  :after (projectile lsp-mode claude-code-ide)
  :demand t
  :config
  (claude-code-ide-extras-setup))
```

### Via MELPA

Once published to MELPA:

```elisp
(use-package claude-code-ide-extras
  :ensure t
  :after (projectile lsp-mode claude-code-ide)
  :demand t
  :config
  (claude-code-ide-extras-setup))
```

### Individual Packages

Install only the packages you need:

```elisp
(use-package claude-code-ide-extras-core
  :after claude-code-ide
  :demand t
  :config
  (claude-code-ide-extras-core-setup))
```

## Available Tools

### Projectile (6 tools)

**task_start** - Launch project tasks (compile, test, configure, install, package, run)
Returns immediately with buffer name while task runs asynchronously.

**task_wait** - Poll for task completion and retrieve output size
Returns status and line/character counts when finished.

**task_query** - Retrieve compilation output
Supports head/tail limiting for large outputs.

**task_search** - Search compilation output with regex patterns
Returns matching lines with optional context.

**task_kill** - Terminate running compilation

**read_dir_locals** / **read_project_dir_locals** - Query Emacs directory-local variables
Discovers project-specific build commands and configuration.

### LSP (2 tools)

**format_buffer** - Format file using LSP formatting

**describe_thing_at_point** - Get hover information at specific location
Returns type signatures, parameter lists, and documentation.

### Core (4 tools)

**describe** - Get documentation for Emacs symbols
Supports functions, variables, modes, packages, and symbols.

**apropos** - Search all symbols by name pattern

**apropos_command** - Search interactive commands only

**apropos_documentation** - Search symbol documentation text

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

## Architecture

The package suite uses a layered architecture:

- Common library provides shared buffer utilities (search, query)
- Individual packages implement domain-specific MCP tools
- Meta-package provides unified installation and setup

All tool implementations use `claude-code-ide-mcp-server-with-session-context` to integrate with the claude-code-ide framework.

## License

GPL-3.0-or-later
