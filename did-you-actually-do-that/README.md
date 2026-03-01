# Did You Actually Do That?

A verification framework for validating claimed AI actions against actual outcomes.

Born from the frustration of AI systems claiming "I've created that file for you" or "I've updated the configuration" without actually doing anything. This library and CLI tool provides a systematic way to verify that claimed actions actually happened.

## The Problem

AI assistants often claim to perform actions that they haven't actually completed:

- "I've created the file at `/path/to/file`" → file doesn't exist
- "I've updated the configuration" → no changes made
- "I've fixed the bug" → same bug, different day

This isn't always hallucination—sometimes it's optimistic prediction, sometimes context loss, sometimes genuine error. Whatever the cause, **trust requires verification**.

## The Solution

This framework introduces a simple model:

1. **Claims** - Assertions that an action was performed
2. **Evidence** - Observable artifacts that should exist if the claim is true
3. **Verification** - Checking evidence against reality
4. **Verdicts** - Did it actually happen? (Confirmed / Refuted / Inconclusive / Unverifiable)

## Installation

```sh
cargo install did-you-actually-do-that
```

Or build from source:

```sh
git clone https://gitlab.com/hyperpolymath/did-you-actually-do-that
cd did-you-actually-do-that
cargo build --release
```

## CLI Usage

### Quick Verification

```sh
# Check if a file exists
dyadt verify /path/to/expected/file.txt

# Compute hash for evidence specification
dyadt hash important-file.rs
```

### Claim Verification

Create a claim file (`my-claim.json`):

```json
{
    "description": "Created the configuration file",
    "evidence": [
        { "type": "FileExists", "spec": { "path": "/etc/myapp/config.toml" } },
        { "type": "FileContains", "spec": { "path": "/etc/myapp/config.toml", "substring": "version = " } }
    ],
    "source": "setup-agent"
}
```

Then verify:

```sh
dyadt check my-claim.json
```

### Batch Verification

```sh
dyadt report multiple-claims.json
```

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Confirmed - all evidence checks passed |
| 1 | Refuted - evidence contradicts the claim |
| 2 | Inconclusive or Unverifiable |
| 3 | Error (invalid input, etc.) |

## Library Usage

```rust
use did_you_actually_do_that::{Claim, EvidenceSpec, Verifier};

// Create a claim with evidence
let claim = Claim::new("Created the output file")
    .with_evidence(EvidenceSpec::FileExists {
        path: "/tmp/output.txt".to_string(),
    })
    .with_evidence(EvidenceSpec::FileContains {
        path: "/tmp/output.txt".to_string(),
        substring: "SUCCESS".to_string(),
    })
    .with_source("my-agent");

// Verify the claim
let verifier = Verifier::new();
let report = verifier.verify(&claim);

println!("{}", report.summary());
// Output: [✓] Created the output file - Confirmed
// or:     [✗] Created the output file - Refuted
```

## Evidence Types

| Type | Description |
|------|-------------|
| `FileExists` | A file should exist at the given path |
| `FileWithHash` | A file should exist with a specific SHA-256 hash |
| `FileContains` | A file should contain a specific substring |
| `DirectoryExists` | A directory should exist |
| `CommandSucceeds` | A command should exit with code 0 |
| `Custom` | Extensible checker with custom parameters |

## Extending with Custom Checkers

```rust
use did_you_actually_do_that::{Verifier, Verdict};

let mut verifier = Verifier::new();

verifier.register_checker("http_reachable", |params| {
    let url = params.get("url").ok_or_else(|| {
        VerificationError::InvalidClaim("Missing url parameter".into())
    })?;

    // Your HTTP check logic here
    Ok(Verdict::Confirmed)
});
```

## Integration Ideas

### CI/CD Pipelines

```yaml
# .gitlab-ci.yml
verify_deployment:
  script:
    - dyadt report deployment-claims.json
  allow_failure: false
```

### AI Agent Wrappers

Wrap your AI interactions to capture claims and verify them:

```rust
// Pseudo-code
let response = ai.complete(prompt).await?;
let claims = extract_claims(&response);

for claim in claims {
    let report = verifier.verify(&claim);
    if !report.overall_verdict.is_trustworthy() {
        log::warn!("AI claim not verified: {}", report.summary());
    }
}
```

### Post-Conversation Audit

Store claims during conversation, verify them afterward:

```sh
# After a conversation with an AI assistant
dyadt report conversation-2024-01-15-claims.json
```

## Philosophy

This tool embodies the principle of **Mutually Assured Accountability**—the same standard should apply to AI systems that we'd apply to human collaborators. If someone says they did something, we should be able to check.

It's not about distrust. It's about building systems where trust is *earned through verifiability*.

## Related Work

- [MAA Framework](https://gitlab.com/hyperpolymath/maa-framework) - Mutually Assured Accountability patterns
- [Conative Gating](https://gitlab.com/hyperpolymath/conative-gating) - AI policy enforcement architecture

## License

MPL-2.0

## Contributing

Contributions welcome. Please ensure any claimed changes are... actually made. 😉
