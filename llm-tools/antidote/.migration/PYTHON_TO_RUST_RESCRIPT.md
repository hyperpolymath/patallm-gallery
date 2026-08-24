<!--
SPDX-License-Identifier: CC-BY-SA-4.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# Python → Rust/ Migration Guide

## Policy
This repo contains Python code that should be migrated to:
- **Rust** for systems/ML/backend code
- **** for web/frontend/scripting

## Why
- Python has dynamic typing and runtime errors
- Rust provides memory safety and performance
-  provides type safety and JS interop

## Migration Steps
1. Identify Python files by purpose (systems vs web)
2. Create equivalent Rust modules in `src/` or  in `src/*.res`
3. Use `cargo` or `` build systems
4. Remove Python files after migration
5. Update CI/CD

## Exceptions
- SaltStack configurations (exempt)
- One-time scripts (convert to shell/Rust)

## Status: PENDING MIGRATION
