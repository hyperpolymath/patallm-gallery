# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 1.x.x   | :white_check_mark: |

## Reporting a Vulnerability

**DO NOT** report security vulnerabilities through public GitHub issues.

### How to Report

Please report security vulnerabilities by:

1. **Email**: security@hyperpolymath.dev
2. **GitHub Security Advisory**: [Create Advisory](https://github.com/hyperpolymath/claude-gitlab-bridge/security/advisories/new)

### What to Include

Please include as much of the following information as possible:

* **Type of vulnerability** (e.g., authentication bypass, SQL injection, XSS)
* **Full paths of source file(s)** related to the vulnerability
* **Location of the affected source code** (tag/branch/commit or direct URL)
* **Step-by-step instructions** to reproduce the issue
* **Proof-of-concept or exploit code** (if possible)
* **Impact of the vulnerability** (what an attacker could do)
* **Suggested remediation** (if you have one)

### Response Timeline

* **Acknowledgment**: Within 24 hours
* **Initial Assessment**: Within 72 hours
* **Status Updates**: Weekly
* **Resolution Target**: 30-90 days depending on severity

## Disclosure Policy

We follow **coordinated disclosure**:

1. Report received and acknowledged
2. Issue verified and assessed for severity
3. Fix developed and tested
4. Fix released to production
5. Public disclosure 30 days after fix (or by mutual agreement)

## Security Best Practices

### For Users

* **Use strong tokens**: Generate GitLab tokens with minimal required scopes
* **Rotate tokens regularly**: Change API tokens every 90 days
* **Monitor access logs**: Review GitLab audit logs for unexpected activity
* **Use HTTPS**: Always access via encrypted connections
* **Keep updated**: Install security updates promptly

### For Developers

* **Never commit secrets**: Use `.env` for sensitive data
* **Validate input**: Sanitize all user input
* **Use prepared statements**: Prevent injection attacks
* **Keep dependencies updated**: Run `npm audit` regularly
* **Follow OWASP Top 10**: Address common vulnerabilities
* **Code review security**: All PRs reviewed for security issues

## Security Features

### Authentication & Authorization

* ✅ Secure token storage
* ✅ Minimum required permissions
* ✅ Token expiration support
* ✅ Audit logging

### Data Protection

* ✅ HTTPS only
* ✅ No plain text password storage
* ✅ Encrypted data transmission
* ✅ Minimal data retention

### Input Validation

* ✅ Input sanitization
* ✅ XSS prevention
* ✅ SQL injection protection
* ✅ Command injection prevention

### Dependency Management

* ✅ Regular dependency updates
* ✅ Automated vulnerability scanning
* ✅ `npm audit` in CI/CD
* ✅ Dependabot alerts enabled

## Security Scope

### In Scope

* Claude GitLab Bridge application code
* GitLab integration components
* Claude API integration
* Authentication and authorization
* Data handling and storage
* Dependencies and third-party libraries

### Out of Scope

* Third-party services (GitLab, Anthropic Claude)
* Social engineering attacks
* Denial of Service (DoS) attacks
* Issues already reported
* Issues in deprecated versions

## Security Updates

Security updates are released as:

* **Critical**: Within 24 hours
* **High**: Within 72 hours
* **Medium**: Within 14 days
* **Low**: Next regular release

## Recognition

We appreciate security researchers and offer:

* Acknowledgment in SECURITY.md (with permission)
* Credit in release notes (if desired)
* Public recognition in README (optional)

## Security Advisories

All security advisories are published at:

https://github.com/hyperpolymath/claude-gitlab-bridge/security/advisories

Subscribe to notifications to stay informed.

## Security-Related Configuration

### Environment Variables

Sensitive configuration via environment variables:

```env
# Required - Never commit these!
GITLAB_TOKEN=glpat-xxxxxxxxxxxxxxxxxxxx
ANTHROPIC_API_KEY=sk-ant-xxxxxxxxxxxxxxxxxxxx
WEBHOOK_SECRET=your-webhook-secret-here
```

### GitLab Token Scopes

**Minimum required scopes**:
* `api` - Full API access
* `read_repository` - Read repository content
* `write_repository` - Create branches and commits

**Avoid**:
* `sudo` - Administrative access
* `admin_mode` - Admin mode access

### Rate Limiting

Configure rate limits to prevent abuse:

```typescript
{
  maxRequestsPerMinute: 60,
  maxRequestsPerHour: 1000
}
```

## Incident Response

### If You Suspect a Security Breach

1. **Immediately**: Revoke API tokens
2. **Review logs**: Check for unauthorized access
3. **Contact us**: security@hyperpolymath.dev
4. **Preserve evidence**: Don't delete logs
5. **Change credentials**: Update all affected tokens

### Our Response Process

1. **Triage**: Assess severity and impact
2. **Containment**: Limit exposure and damage
3. **Investigation**: Determine root cause
4. **Remediation**: Implement fixes
5. **Recovery**: Restore normal operations
6. **Lessons learned**: Update processes

## Compliance

This project follows:

* ✅ OWASP Top 10
* ✅ CWE/SANS Top 25
* ✅ NIST Cybersecurity Framework
* ✅ GDPR (data protection)
* ✅ RFC 9116 (security.txt)

## Security Contacts

* **General Security**: security@hyperpolymath.dev
* **Code of Conduct Issues**: conduct@hyperpolymath.dev
* **General Inquiries**: hello@hyperpolymath.dev

## Additional Resources

* [OWASP Top 10](https://owasp.org/www-project-top-ten/)
* [CWE Top 25](https://cwe.mitre.org/top25/)
* [GitLab Security](https://about.gitlab.com/security/)
* [Anthropic Security](https://www.anthropic.com/security)

---

**Last Updated**: 2024-11-28
**Version**: 1.0
**Contact**: security@hyperpolymath.dev
