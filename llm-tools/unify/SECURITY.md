# Security Policy

## Reporting Security Vulnerabilities

We take security seriously. If you discover a security vulnerability in LLM Unify, please report it responsibly.

### Contact

**Email:** security@llm-unify.dev
**PGP Key:** See `.well-known/security.txt`

### Response Timeline

- **Critical vulnerabilities** (RCE, privilege escalation, data exposure): 7 days
- **High severity** (authentication bypass, injection): 14 days
- **Medium severity** (DoS, information disclosure): 30 days
- **Low severity** (configuration issues, minor leaks): 60 days

### What to Include

1. **Description** of the vulnerability
2. **Steps to reproduce** the issue
3. **Proof of concept** (if applicable)
4. **Impact assessment** (what can be compromised)
5. **Suggested fix** (if you have one)

## Security Considerations

### Local-First Architecture

LLM Unify is designed with a local-first architecture:

- **No network calls** - All data stays on your machine
- **SQLite storage** - Encrypted at rest with user's file system permissions
- **No telemetry** - Zero data collection or phone-home behavior

### Known Security Measures

1. **SQL Injection Prevention**
   - All database queries use parameterized statements (SQLx)
   - No raw SQL string concatenation
   - Type-safe query builders

2. **Input Validation**
   - File path sanitization for import/export operations
   - JSON parsing with strict schema validation
   - Message content size limits (configurable)

3. **Memory Safety**
   - **Zero unsafe blocks** - Rust compile-time guarantees
   - No manual memory management
   - Thread-safe concurrency (SQLite connection pooling)

4. **Dependency Management**
   - Regular `cargo audit` scans in CI/CD
   - Minimal dependency footprint
   - Vetted crates only (serde, sqlx, ratatui, clap)

### Known Limitations

1. **Database Encryption**
   - SQLite database is **not encrypted** by default in v0.1
   - Relies on file system permissions
   - Plan to add encryption in v0.2 (see roadmap)

2. **Export Format**
   - Exported JSON files contain plaintext conversations
   - Users should secure export files appropriately

3. **Parser Trust**
   - Import parsers trust input data structure
   - Malformed exports may cause parse errors (non-exploitable)

## Security Audit History

| Date       | Auditor | Scope            | Findings | Status    |
|------------|---------|------------------|----------|-----------|
| 2025-11-28 | Internal| Initial release  | None     | Clean     |

**External audit:** Planned for v0.2.0 release (Q1 2025)

## Security Features Roadmap

### v0.2.0 (Planned Q1 2025)
- [ ] SQLite database encryption (SQLCipher integration)
- [ ] Export file encryption (age/GPG options)
- [ ] Secure key management
- [ ] Formal threat modeling

### v0.3.0 (Planned Q2 2025)
- [ ] External security audit
- [ ] Penetration testing results
- [ ] Security.txt automation

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |
| < 0.1   | :x:                |

## Security Best Practices for Users

1. **File Permissions**: Protect your database file (`llm-unify.db`)
   ```bash
   chmod 600 llm-unify.db
   ```

2. **Backup Security**: Encrypt backups before cloud storage
   ```bash
   llm-unify backup backup.db
   gpg -c backup.db  # Encrypt with passphrase
   ```

3. **Import Safety**: Only import exports from trusted sources

4. **Update Regularly**: Keep LLM Unify updated for security patches

## Compliance

This security policy follows:
- **RFC 9116** - `security.txt` standard
- **OWASP Top 10** - Web application security risks
- **Rhodium Standard Repository** - Security documentation requirements
