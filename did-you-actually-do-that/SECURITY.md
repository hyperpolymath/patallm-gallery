# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

If you discover a security vulnerability in did-you-actually-do-that, please report it responsibly:

1. **Do NOT open a public issue** for security vulnerabilities
2. Email security concerns to: hyperpolymath@proton.me
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Suggested fix (if any)

## Response Timeline

- **Acknowledgment**: Within 48 hours
- **Initial assessment**: Within 1 week
- **Fix timeline**: Depends on severity, typically 2-4 weeks

## Security Considerations

### Command Execution

The `CommandSucceeds` evidence type executes arbitrary commands. Users should:

- Never load claim files from untrusted sources
- Review claim files before verification
- Run with minimal privileges when possible
- Consider sandboxing in CI/CD environments

### File System Access

Evidence checks read files and directories. Be aware that:

- Path traversal in claim files could expose sensitive data
- Hash verification reads entire file contents into memory
- Large files may cause memory issues

### Recommendations

1. **Validate claim sources**: Only verify claims from trusted agents
2. **Restrict paths**: In production, consider allowlisting verification paths
3. **Audit custom checkers**: Review any registered custom checkers for security
4. **CI/CD isolation**: Run verification in isolated environments

## Threat Model

This tool assumes:

- Claim files may be malicious (untrusted input)
- The verification environment is trusted
- File system and command execution are inherently privileged operations

The tool does NOT protect against:

- Malicious claim files exploiting command execution
- Time-of-check to time-of-use (TOCTOU) races
- Side-channel attacks via timing differences
