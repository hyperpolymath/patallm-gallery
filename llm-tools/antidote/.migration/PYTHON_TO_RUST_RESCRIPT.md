# Python → Rust/ReScript Migration Guide

## Policy
This repo contains Python code that should be migrated to:
- **Rust** for systems/ML/backend code
- **ReScript** for web/frontend/scripting

## Why
- Python has dynamic typing and runtime errors
- Rust provides memory safety and performance
- ReScript provides type safety and JS interop

## Migration Steps
1. Identify Python files by purpose (systems vs web)
2. Create equivalent Rust modules in `src/` or ReScript in `src/*.res`
3. Use `cargo` or `rescript` build systems
4. Remove Python files after migration
5. Update CI/CD

## Exceptions
- SaltStack configurations (exempt)
- One-time scripts (convert to shell/Rust)

## Status: PENDING MIGRATION
