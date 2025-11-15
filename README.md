# claude-code-ide-extras

MCP (Model Context Protocol) tool extensions for [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el).

## Status

🚧 **Under Active Development** - Package structure in place, implementations coming soon.

## Overview

This repository provides three extension packages that add additional MCP tools to claude-code-ide:

### claude-code-ide-extras-projectile

Projectile integration for project-aware development:
- Project task execution (build, test, configure)
- Shell command execution in project context
- Compilation/shell output querying and search
- Project configuration discovery (dir-locals)

### claude-code-ide-extras-lsp

LSP integration for semantic code understanding:
- Code formatting via LSP
- Hover information (type signatures, documentation)
- Language server interaction

### claude-code-ide-extras-core

Core Emacs introspection for environment learning:
- Function/variable documentation
- Command discovery
- Documentation search
- Help system integration

## Philosophy

These tools follow the principle that everything should happen in Emacs buffers with uniform query interfaces, rather than external processes with ephemeral output. Buffers become the universal interface for human-AI interaction.

## Requirements

- Emacs 29.1+
- claude-code-ide
- projectile 2.8.0+ (for projectile package)
- lsp-mode 8.0.0+ (for LSP package)

## License

GPL-3.0-or-later
