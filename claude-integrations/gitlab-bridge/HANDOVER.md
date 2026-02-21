<!--
SPDX-License-Identifier: PMPL-1.0-or-later-or-later
SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
-->

# Claude-GitLab Bridge Project Handover Document

## Project Identity

- **Name**: Claude-GitLab Bridge
- **Repository**: `gitlab.com/hyperpolymath/claude-gitlab-bridge`
- **Purpose**: Bidirectional interface enabling Claude AI to interact with GitLab APIs
- **License**: MIT
- **Author**: Jonathan D.A. Jewell

## Core Boundaries

### IN SCOPE

1. **GitLab API Operations**
   - Project management (CRUD)
   - Repository file operations
   - CI/CD pipeline control
   - Issue and merge request management
   - Release and tag creation
   - Wiki page management

2. **Bridge Capabilities**
   - Command generation for Claude
   - Batch operation execution
   - Dry-run mode for safety
   - Comprehensive logging
   - Error handling and retries

3. **Integration Modes**
   - Interactive CLI interface
   - Single command execution
   - Batch JSON operations
   - Project deployment automation
   - Repository synchronization

4. **Security Features**
   - Personal access token management
   - TLS communication
   - Operation auditing
   - Rate limiting respect

### OUT OF SCOPE

- Direct Claude API integration
- GitLab self-hosted instances (without modification)
- Git operations beyond GitLab API
- Repository content analysis
- Code review automation
- GitLab administration tasks

## Technical Architecture

### Core Components

```
claude-gitlab-bridge/
├── src/
│   ├── bridge.ts           # Main bridge class
│   ├── commands/           # Command implementations
│   │   ├── project.ts     # Project operations
│   │   ├── repository.ts  # Repo operations
│   │   ├── ci_cd.ts       # Pipeline operations
│   │   └── issues.ts      # Issue management
│   ├── generator.ts        # Claude command generator
│   └── utils/              # Utilities
├── templates/              # Command templates
├── config/                 # Configuration files
└── tests/                  # Test suite
```

### Dependencies

- Runtime: Node.js 18+ / Deno 1.40+
- Languages: ReScript (primary), TypeScript (legacy)
- Packages: anthropic-sdk, gitlab-api client
- Optional: Redis (for caching)

## State Management

Configuration is managed via `STATE.scm` using Scheme syntax. Key settings include:

- **Connection**: GitLab URL, API version, token environment variable
- **Settings**: Dry-run mode, max retries, timeout, caching
- **Logging**: Log level, file location, rotation settings
- **Rate Limiting**: Request limits, backoff strategy

## Ecosystem Integration

The project integrates with:

### Internal
- All hyperpolymath projects via GitLab API for deployment and management

### External
- **GitLab**: Primary API endpoint (REST API v4)
- **Claude**: Command generation target (JSON commands, bidirectional)

### APIs
- **CLI**: Deno-based CLI with interactive, execute, and batch modes
- **Internal API**: Optional HTTP endpoint on port 8888

## Commands Reference

### Project Operations
- `create`, `update`, `delete`, `list`, `get`

### Repository Operations
- `clone`, `push`, `create-branch`, `merge-request`

### File Operations
- `create`, `update`, `delete`, `get`, `upload-directory`

### CI/CD Operations
- `create-pipeline`, `trigger-pipeline`, `get-status`, `cancel-pipeline`

### Issue Operations
- `create`, `update`, `list`, `close`, `reopen`, `add-comment`, `boards`

### Merge Request Operations
- `create`, `update`, `merge`, `approve`, `list`

### Release Operations
- `create`, `tag`, `list`, `delete`

### Wiki Operations
- `create`, `update`, `delete`, `list`

## Security Configuration

- **Authentication**: Personal access tokens via environment variables
- **TLS**: Required for all communications
- **Audit Logging**: Full operation audit trail
- **Token Scopes**: `api`, `read_repository`, `write_repository`
- **Webhook Validation**: Enabled for incoming webhooks

## Development Guidelines

### Language Policy (per RSR)

- **Primary**: ReScript for new code
- **Fallback**: TypeScript for legacy compatibility
- **Banned**: Python, Ruby, Perl (per RSR language policy)

### Code Style

- Functional programming paradigm
- Type-safe implementations
- Comprehensive error handling
- Detailed logging for debugging

### Testing Requirements

- Unit tests: `vitest tests/unit/`
- Integration tests: `vitest tests/integration/`
- E2E tests: `vitest tests/e2e/`
- Coverage target: 80%
- Mock mode available for offline testing

## Deployment Options

1. **Standalone**: `deno run --allow-net --allow-env src/main.ts`
2. **Docker**: `docker run claude-gitlab-bridge`
3. **Systemd Service**: Available for production deployments

## Next Steps

1. Define TypeScript/ReScript interfaces for core types
2. Implement GitLab API client wrapper
3. Set up test infrastructure with mocking
4. Implement command generation logic
5. Add batch execution support
6. Create interactive CLI mode

## Related Documentation

- [META.scm](META.scm) - Project metadata and architecture decisions
- [ECOSYSTEM.scm](ECOSYSTEM.scm) - Integration definitions
- [STATE.scm](STATE.scm) - Current state and configuration
- [CLAUDE.md](CLAUDE.md) - Claude-specific instructions
- [README.adoc](README.adoc) - User-facing documentation

## Contact

- **Author**: Jonathan D.A. Jewell
- **Organization**: hyperpolymath
- **Email**: hello@hyperpolymath.dev
