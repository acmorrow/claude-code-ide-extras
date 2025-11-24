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

I have ideas for several other tools to give Claude Code deeper
integration with Emacs, including the ability to directly create and
edit buffers, additional LSP integrations, and potentially just
directly eval elisp.

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

**claude-code-ide-extras-emacs**
Emacs introspection and buffer access:
- Function and variable documentation
- Command and symbol discovery
- Documentation search across all loaded packages
- Direct buffer read and search capabilities

**claude-code-ide-extras-meta**
Meta-level tools about the MCP tools themselves:
- Customizable per-tool usage guidance
- Project-specific configuration via dir-locals

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
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-emacs")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-lsp")
  (package-vc-install-from-checkout extras-dir "claude-code-ide-extras-meta")
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
(use-package claude-code-ide-extras-emacs
  :after claude-code-ide
  :demand t
  :config
  (claude-code-ide-extras-emacs-setup))
```

## Available Tools

### Projectile (7 tools)

**task_start** - Launch project tasks (compile, test, configure, install, package, run)
Returns immediately with buffer name while task runs asynchronously.

**task_wait** - Poll for task completion and retrieve output size
Returns status and line/character counts when finished.

**task_query** - Retrieve compilation output
Supports line range queries (start + count) with negative indexing for tail access.

**task_search** - Search compilation output with regex patterns
Returns matching lines with optional context.

**task_kill** - Terminate running compilation

**read_dir_locals** / **read_project_dir_locals** - Query Emacs directory-local variables
Discovers project-specific build commands and configuration.

### LSP (2 tools)

**format_buffer** - Format file using LSP formatting

**describe_thing_at_point** - Get hover information at specific location
Returns type signatures, parameter lists, and documentation.

### Emacs (6 tools)

**describe** - Get documentation for Emacs symbols
Supports functions, variables, modes, packages, and symbols.

**apropos** - Search all symbols by name pattern

**apropos_command** - Search interactive commands only

**apropos_documentation** - Search symbol documentation text

**buffer_query** - Read contents from any Emacs buffer
Line-range queries with 1-based indexing and negative offset support. Enables scratch buffer collaboration and message log analysis.

**buffer_search** - Search any Emacs buffer with regex
Search compilation, scratch, messages, or any other buffer with optional context lines.

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
- **Format and modify files** via LSP integration

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
- Meta-package provides unified installation and setup

All tool implementations use `claude-code-ide-mcp-server-with-session-context` to integrate with the claude-code-ide framework.

## License

GPL-3.0-or-later

See [LICENSE](LICENSE) for full license text.
