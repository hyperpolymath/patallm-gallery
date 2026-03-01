<!--
SPDX-License-Identifier: PMPL-1.0-or-later-or-later
SPDX-FileCopyrightText: 2024 hyperpolymath
-->

# Claude GitLab Bridge

## Project Overview

This project provides a bridge between Claude (Anthropic's AI assistant) and GitLab, enabling Claude to interact with GitLab issues, merge requests, and repositories. It allows Claude to assist with development tasks directly within the GitLab workflow.

## Purpose

The Claude GitLab Bridge enables:
- Automated responses to GitLab issues and merge requests
- Code analysis and review on GitLab repositories
- Task automation within GitLab projects
- Integration of Claude's capabilities into GitLab CI/CD pipelines
- Direct interaction with GitLab API for project management

## Architecture

### Core Components

- **GitLab Integration**: Handles authentication and communication with GitLab API
- **Claude Integration**: Manages interactions with Anthropic's Claude API
- **Event Processing**: Processes GitLab webhooks and events
- **Context Management**: Maintains conversation context across interactions
- **Branch Management**: Handles automatic branch creation and commits

## Development Guidelines

### Code Style

- Use TypeScript for type safety
- Follow functional programming principles where appropriate
- Write comprehensive error handling
- Include detailed logging for debugging
- Keep functions small and focused

### Testing

- Write unit tests for all core functionality
- Include integration tests for GitLab API interactions
- Test webhook handling thoroughly
- Mock external API calls in tests

### Security Considerations

- Never commit API keys or tokens
- Use environment variables for sensitive configuration
- Validate all webhook payloads
- Implement proper authentication for all endpoints
- Follow principle of least privilege for GitLab tokens

### Git Workflow

- Create feature branches for all changes
- Use descriptive commit messages
- Keep commits atomic and focused
- Branch naming convention: `claude/feature-name-<session-id>`
- Always push to the designated feature branch

## Configuration

### Environment Variables

```
GITLAB_TOKEN - GitLab personal access token or project token
GITLAB_URL - GitLab instance URL (default: https://gitlab.com)
ANTHROPIC_API_KEY - Claude API key
WEBHOOK_SECRET - Secret for validating GitLab webhooks
PORT - Server port (default: 3000)
```

### GitLab Permissions Required

- `api` - Full API access
- `read_repository` - Read repository content
- `write_repository` - Create branches and commits

## API Integration

### GitLab API

Uses GitLab REST API v4 for:
- Repository operations
- Issue management
- Merge request handling
- User information
- Project metadata

### Claude API

Integrates with Anthropic's Claude API for:
- Natural language understanding
- Code analysis and generation
- Task completion
- Context-aware responses

## Common Tasks

### Adding New Features

1. Create a feature branch following the naming convention
2. Implement the feature with appropriate tests
3. Update documentation as needed
4. Commit changes with descriptive messages
5. Push to the feature branch

### Debugging

- Check application logs for errors
- Verify environment variables are set correctly
- Test GitLab webhook delivery in GitLab settings
- Use GitLab API explorer for manual API testing

### Deployment

- Ensure all environment variables are configured
- Run tests before deployment
- Deploy to a server with HTTPS support
- Configure GitLab webhooks to point to deployment URL

## Project Structure

```
/src
  /api          - API route handlers
  /services     - Business logic and integrations
  /models       - Data models and types
  /utils        - Utility functions
  /middleware   - Express middleware
/tests          - Test files
/config         - Configuration files
```

## Dependencies

### Core Dependencies
- Node.js runtime
- TypeScript for type safety
- Express.js for HTTP server (if web-based)
- Anthropic SDK for Claude integration
- GitLab SDK or axios for GitLab API

### Development Dependencies
- Jest or Vitest for testing
- ESLint for linting
- Prettier for code formatting
- TypeScript compiler

## Resources

- [GitLab API Documentation](https://docs.gitlab.com/ee/api/)
- [Anthropic Claude API Documentation](https://docs.anthropic.com/)
- [GitLab Webhooks](https://docs.gitlab.com/ee/user/project/integrations/webhooks.html)

## Troubleshooting

### Common Issues

**GitLab Authentication Failures**
- Verify GITLAB_TOKEN is valid and has required scopes
- Check token hasn't expired
- Ensure correct GITLAB_URL for self-hosted instances

**Claude API Errors**
- Verify ANTHROPIC_API_KEY is set correctly
- Check API rate limits
- Ensure proper error handling for API responses

**Webhook Not Triggering**
- Verify webhook URL is accessible from internet
- Check webhook secret matches configuration
- Review GitLab webhook delivery logs

## Contributing

When working on this project:
1. Always work on the designated Claude feature branch
2. Write clear, descriptive commit messages
3. Include tests for new functionality
4. Update documentation for significant changes
5. Follow the existing code style and patterns

## Notes for Claude

- This project bridges GitLab with Claude's capabilities
- Always develop on the branch specified in the task context
- Commit changes with clear messages describing what was done
- Push to origin with `-u` flag for new branches
- Follow the git retry logic for network failures (exponential backoff: 2s, 4s, 8s, 16s)
- Branch names must start with 'claude/' and end with the session ID
- Never push to main or other branches without permission
