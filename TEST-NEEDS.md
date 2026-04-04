# TEST-NEEDS.md — patallm-gallery

## CRG Grade: C — ACHIEVED 2026-04-04

## Current Test State

| Category | Count | Notes |
|----------|-------|-------|
| ExUnit tests (Elixir) | 8+ | `dyadt_brain/test/` suite |
| Fuzzing targets (Rust) | 1 | `dyadt_brain/fuzz/fuzz_targets/fuzz_evidence_spec.rs` |
| Config tests | Present | `dyadt_brain/config/test.exs` |

## What's Covered

- [x] Elixir unit/integration tests (claims, gRPC, patterns, schemas)
- [x] Rust fuzzing infrastructure
- [x] Regression baseline tests
- [x] ExUnit framework

## Still Missing (for CRG B+)

- [ ] Property-based testing (StreamData)
- [ ] Load testing
- [ ] End-to-end gallery tests
- [ ] Performance benchmarks

## Run Tests

```bash
cd /var/mnt/eclipse/repos/patallm-gallery/did-you-actually-do-that/dyadt_brain && mix test
```
