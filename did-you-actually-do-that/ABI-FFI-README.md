# DYADT ABI/FFI Documentation

## Overview

This library follows the **Hyperpolymath RSR Standard** for ABI and FFI design:

- **ABI (Application Binary Interface)** defined in **Idris2** with formal proofs
- **FFI (Foreign Function Interface)** implemented in **Zig** for C compatibility
- **Generated C headers** bridge Idris2 ABI to Zig FFI
- **Any language** can call through standard C ABI

## Architecture

```
┌─────────────────────────────────────────────┐
│  ABI Definitions (Idris2)                   │
│  src/abi/                                   │
│  - Types.idr      (DYADT type definitions)  │
│  - Layout.idr     (Memory layout proofs)    │
│  - Foreign.idr    (FFI declarations)        │
└─────────────────┬───────────────────────────┘
                  │
                  │ generates (at compile time)
                  ▼
┌─────────────────────────────────────────────┐
│  C Headers (auto-generated)                 │
│  generated/abi/dyadt.h                      │
└─────────────────┬───────────────────────────┘
                  │
                  │ imported by
                  ▼
┌─────────────────────────────────────────────┐
│  FFI Implementation (Zig)                   │
│  ffi/zig/src/main.zig                       │
│  - Implements C-compatible functions        │
│  - Zero-cost abstractions                   │
│  - Memory-safe by default                   │
└─────────────────┬───────────────────────────┘
                  │
                  │ compiled to libdyadt.so/.a
                  ▼
┌─────────────────────────────────────────────┐
│  Any Language via C ABI                     │
│  - Rust, Elixir (NIF), ReScript, Julia     │
└─────────────────────────────────────────────┘
```

## DYADT-Specific Types

The ABI defines these domain types with formal proofs:

| Type | Idris2 | Zig | C int | Description |
|------|--------|-----|-------|-------------|
| `Verdict` | `Confirmed \| Refuted \| Inconclusive \| Unverifiable` | `enum(u32)` | 0-3 | Verification outcome |
| `VoteDecision` | `Approve \| Reject \| Abstain` | `enum(u32)` | 0-2 | SLM vote decision |
| `ViolationCategory` | 12 constructors | `enum(u32)` | 1-12 | Cognitive-gating categories |
| `VerificationLayer` | 12 constructors | `u32` | 0-11 | Pipeline layer IDs |
| `Result` | 7 constructors | `enum(c_int)` | 0-6 | Operation result codes |

### FFI Structs (C-compatible, formally verified)

| Struct | Size | Align | Fields |
|--------|------|-------|--------|
| `LayerResultFFI` | 16B | 8B | layer(u32), verdict(u32), confidence(f64) |
| `VoteFFI` | 24B | 8B | decision(u32), _pad(u32), confidence(f64), weight(f64) |
| `EnsembleResultFFI` | 32B | 8B | approve_weight(f64), reject_weight(f64), abstain_count(u32), suggested_verdict(u32), vote_count(u32), _pad(u32) |
| `ReportSummary` | 24B | 8B | verdict(u32), layer_count(u32), confidence(f64), violation_count(u32), _pad(u32) |

All struct sizes and alignments are formally proven in `Layout.idr` and verified at compile-time in Zig via `@sizeOf` / `@alignOf` tests.

## Directory Structure

```
did-you-actually-do-that/
├── src/
│   ├── abi/                    # ABI definitions (Idris2)
│   │   ├── Types.idr           # Verdict, VoteDecision, ViolationCategory, etc.
│   │   ├── Layout.idr          # Memory layout proofs for FFI structs
│   │   └── Foreign.idr         # FFI function declarations (dyadt_*)
│   ├── slm/                    # SLM ensemble (Rust)
│   └── verifiers/              # 12-layer verification pipeline (Rust)
│
├── ffi/
│   └── zig/                    # FFI implementation (Zig)
│       ├── build.zig           # Build: libdyadt.so / libdyadt.a
│       ├── src/
│       │   └── main.zig        # C-compatible FFI: dyadt_init, dyadt_verify_claim, etc.
│       ├── test/
│       │   └── integration_test.zig  # 25+ integration tests
│       └── include/
│           └── dyadt.h         # C header (optional)
│
├── generated/                  # Auto-generated files
│   └── abi/
│       └── dyadt.h             # Generated from Idris2 ABI
│
└── dyadt_brain/                # Elixir brain (Tier 3 arbiter)
```

## FFI Functions

### Lifecycle

| Function | Signature | Description |
|----------|-----------|-------------|
| `dyadt_init` | `() -> ?*Handle` | Initialize engine, returns handle |
| `dyadt_free` | `(?*Handle) -> void` | Free engine resources |
| `dyadt_is_initialized` | `(?*Handle) -> u32` | Check if engine is ready |
| `dyadt_version` | `() -> [*:0]const u8` | Get version string |
| `dyadt_build_info` | `() -> [*:0]const u8` | Get build info |

### Verification

| Function | Signature | Description |
|----------|-----------|-------------|
| `dyadt_verify_claim` | `(handle, json_ptr, json_len) -> Result` | Verify a claim |
| `dyadt_get_verdict` | `(handle) -> u32` | Get last verdict |
| `dyadt_get_report` | `(handle) -> ?[*:0]const u8` | Get report as JSON |

### SLM Ensemble

| Function | Signature | Description |
|----------|-----------|-------------|
| `dyadt_evaluate_slm` | `(handle, ctx_ptr, ctx_len) -> Result` | Run SLM ensemble |
| `dyadt_get_ensemble_result` | `(handle) -> ?[*:0]const u8` | Get ensemble result JSON |
| `dyadt_get_suggested_verdict` | `(handle) -> u32` | Get suggested verdict |

### Claim Extraction

| Function | Signature | Description |
|----------|-----------|-------------|
| `dyadt_extract_claims` | `(handle, text_ptr, text_len) -> Result` | Extract claims from text |
| `dyadt_get_claims` | `(handle) -> ?[*:0]const u8` | Get extracted claims JSON |

### Utility

| Function | Signature | Description |
|----------|-----------|-------------|
| `dyadt_free_string` | `(?[*:0]const u8) -> void` | Free library-allocated string |
| `dyadt_last_error` | `() -> ?[*:0]const u8` | Get last error message |
| `dyadt_backend_count` | `(handle) -> u32` | Number of SLM backends |
| `dyadt_layer_count` | `(handle) -> u32` | Number of verification layers |

## Building

### Build FFI Library

```bash
cd ffi/zig
zig build                         # Build debug
zig build -Doptimize=ReleaseFast  # Build optimized
zig build test                    # Run unit tests
zig build test-integration        # Run integration tests
```

### Cross-Compile

```bash
cd ffi/zig
zig build -Dtarget=x86_64-linux
zig build -Dtarget=aarch64-macos
zig build -Dtarget=x86_64-windows
```

## Usage Examples

### From C

```c
#include "dyadt.h"

int main() {
    void* handle = dyadt_init();
    if (!handle) return 1;

    const char* claim = "{\"description\":\"Added login\"}";
    int result = dyadt_verify_claim(handle, claim, strlen(claim));
    if (result != 0) {
        const char* err = dyadt_last_error();
        fprintf(stderr, "Error: %s\n", err);
        dyadt_free_string(err);
    }

    uint32_t verdict = dyadt_get_verdict(handle);
    printf("Verdict: %u\n", verdict);

    dyadt_free(handle);
    return 0;
}
```

### From Idris2

```idris
import DYADT.ABI.Foreign

main : IO ()
main = do
  Just handle <- init
    | Nothing => putStrLn "Failed to initialize"

  True <- isInitialized handle
    | False => putStrLn "Engine not ready"

  backends <- backendCount handle
  layers <- layerCount handle
  putStrLn $ "DYADT: " ++ show backends ++ " backends, " ++ show layers ++ " layers"

  free handle
```

### From Rust (via C FFI)

```rust
#[link(name = "dyadt")]
extern "C" {
    fn dyadt_init() -> *mut std::ffi::c_void;
    fn dyadt_free(handle: *mut std::ffi::c_void);
    fn dyadt_verify_claim(handle: *mut std::ffi::c_void, data: *const u8, len: u32) -> i32;
    fn dyadt_get_verdict(handle: *mut std::ffi::c_void) -> u32;
}

fn main() {
    unsafe {
        let handle = dyadt_init();
        assert!(!handle.is_null());

        let claim = b"{\"description\":\"test\"}";
        let result = dyadt_verify_claim(handle, claim.as_ptr(), claim.len() as u32);
        assert_eq!(result, 0);

        let verdict = dyadt_get_verdict(handle);
        println!("Verdict: {}", verdict);

        dyadt_free(handle);
    }
}
```

## License

SPDX-License-Identifier: PMPL-1.0-or-later

## See Also

- [Idris2 Documentation](https://idris2.readthedocs.io)
- [Zig Documentation](https://ziglang.org/documentation/master/)
- [Rhodium Standard Repositories](https://github.com/hyperpolymath/rhodium-standard-repositories)
