# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.2.x   | :white_check_mark: |
| 0.1.x   | :x:                |

## Reporting a Vulnerability

If you discover a security vulnerability, please:

1. **Do not** open a public issue
2. Email security concerns to the maintainers
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Suggested fix (if any)

We will respond within 48 hours and work with you to resolve the issue.

## Security Considerations

### Database

- State database is stored locally by default
- No authentication on local database access
- For production deployments, consider:
  - File system permissions
  - Encrypted storage
  - Network isolation

### GraphQL API

- Enable authentication for production use
- Configure CORS appropriately
- Use TLS for network deployments
- Set appropriate depth and complexity limits

### External Tools

- pandoc and tesseract are invoked as subprocesses
- Validate and sanitize inputs before processing
- Consider sandboxing for untrusted inputs

## Best Practices

1. Keep dependencies updated
2. Use the `audit` and `deny` just recipes
3. Review agent capabilities in production
4. Monitor event logs for suspicious activity
5. Regular backups with `db backup`
