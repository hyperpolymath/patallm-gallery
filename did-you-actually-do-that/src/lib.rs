// SPDX-License-Identifier: PMPL-1.0-or-later
//! # Did You Actually Do That?
//!
//! A verification framework for validating claimed AI actions against actual outcomes.
//!
//! Born from the frustration of AI systems claiming to perform actions without
//! actually doing them. This library provides types and verification logic to
//! hold systems (and ourselves) accountable.
//!
//! ## Core Concepts
//!
//! - **Claim**: An assertion that an action was performed
//! - **Evidence**: Observable artifacts that should exist if the claim is true
//! - **Verification**: The process of checking evidence against claims
//! - **Verdict**: The outcome of verification (Confirmed, Refuted, Inconclusive)
//!
//! ## Quick Start
//!
//! ```rust
//! use did_you_actually_do_that::{Claim, EvidenceSpec, Verifier, Verdict};
//!
//! // Create a claim with evidence
//! let claim = Claim::new("Created configuration file")
//!     .with_evidence(EvidenceSpec::FileExists {
//!         path: "/tmp/config.json".to_string(),
//!     })
//!     .with_source("my-ai-assistant");
//!
//! // Verify the claim
//! let verifier = Verifier::new();
//! let report = verifier.verify(&claim);
//!
//! // Check the result
//! match report.overall_verdict {
//!     Verdict::Confirmed => println!("Claim verified!"),
//!     Verdict::Refuted => println!("Claim is false!"),
//!     Verdict::Inconclusive => println!("Could not determine"),
//!     Verdict::Unverifiable => println!("Cannot verify this claim"),
//! }
//! ```
//!
//! ## Evidence Types
//!
//! The library supports many evidence types:
//!
//! - `FileExists` - Check if a file exists
//! - `FileWithHash` - Verify file exists with specific SHA-256 hash
//! - `FileContains` - Check if file contains a substring
//! - `FileMatchesRegex` - Check if file matches a regex pattern
//! - `FileJsonPath` - Verify JSON value at path
//! - `DirectoryExists` - Check if directory exists
//! - `CommandSucceeds` - Run a command and check it succeeds
//! - `GitClean` - Check if git working directory is clean
//! - `GitCommitExists` - Verify a git commit exists
//! - `GitBranchExists` - Verify a git branch exists
//! - `FileModifiedAfter` - Check file was modified after timestamp
//! - `EnvVar` - Check environment variable value
//! - `Custom` - Extensible custom checks
//!
//! ## Features
//!
//! - `async` - Enable async verification for network-based evidence checks (HTTP, TCP)
//! - `watch` - Enable watch mode for continuous verification

#[cfg(feature = "async")]
pub mod async_checks;

#[cfg(feature = "watch")]
pub mod watch;

pub mod claim_extractor;
pub mod hooks;
pub mod mcp_server;
pub mod port_protocol;
pub mod slm;
pub mod verifiers;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur during verification
#[derive(Error, Debug)]
pub enum VerificationError {
    #[error("Evidence not found: {0}")]
    EvidenceNotFound(String),

    #[error("Evidence mismatch: expected {expected}, found {found}")]
    EvidenceMismatch { expected: String, found: String },

    #[error("Verification timed out after {0} seconds")]
    Timeout(u64),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Invalid claim structure: {0}")]
    InvalidClaim(String),
}

/// The verdict of a verification check
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Verdict {
    /// All evidence confirms the claim
    Confirmed,
    /// Evidence contradicts the claim
    Refuted,
    /// Insufficient evidence to determine truth
    Inconclusive,
    /// Verification could not be performed
    Unverifiable,
}

impl Verdict {
    pub fn is_trustworthy(&self) -> bool {
        matches!(self, Verdict::Confirmed)
    }
}

/// Types of evidence that can be checked
///
/// Each variant represents a different kind of verifiable evidence. Evidence
/// specifications are serializable to JSON and can be loaded from files.
///
/// # Examples
///
/// ```rust
/// use did_you_actually_do_that::EvidenceSpec;
///
/// // File existence
/// let file_evidence = EvidenceSpec::FileExists {
///     path: "/path/to/file.txt".to_string(),
/// };
///
/// // File with specific hash
/// let hash_evidence = EvidenceSpec::FileWithHash {
///     path: "/path/to/file.txt".to_string(),
///     sha256: "abc123...".to_string(),
/// };
///
/// // File contains text
/// let content_evidence = EvidenceSpec::FileContains {
///     path: "/path/to/file.txt".to_string(),
///     substring: "expected text".to_string(),
/// };
///
/// // Command succeeds
/// let cmd_evidence = EvidenceSpec::CommandSucceeds {
///     command: "cargo".to_string(),
///     args: vec!["test".to_string()],
/// };
///
/// // Git repository is clean
/// let git_evidence = EvidenceSpec::GitClean {
///     repo_path: Some("/path/to/repo".to_string()),
/// };
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "spec")]
pub enum EvidenceSpec {
    /// A file should exist at the given path
    FileExists { path: String },

    /// A file should exist with specific content hash
    FileWithHash { path: String, sha256: String },

    /// A file should contain the given substring
    FileContains { path: String, substring: String },

    /// A file should match a regular expression pattern
    FileMatchesRegex { path: String, pattern: String },

    /// A JSON file should have a value at the given path
    /// Path format: `.field.nested\[0\].value`
    FileJsonPath {
        path: String,
        json_path: String,
        expected: serde_json::Value,
    },

    /// A directory should exist
    DirectoryExists { path: String },

    /// A command should succeed (exit code 0)
    CommandSucceeds { command: String, args: Vec<String> },

    /// Git working directory should be clean (no uncommitted changes)
    GitClean {
        /// Path to repository (defaults to current directory)
        #[serde(default)]
        repo_path: Option<String>,
    },

    /// A specific git commit should exist
    GitCommitExists {
        /// Commit hash (full or short)
        commit: String,
        /// Path to repository (defaults to current directory)
        #[serde(default)]
        repo_path: Option<String>,
    },

    /// Git branch should exist
    GitBranchExists {
        branch: String,
        #[serde(default)]
        repo_path: Option<String>,
    },

    /// File should have been modified after a given timestamp
    FileModifiedAfter {
        path: String,
        /// ISO 8601 timestamp
        after: String,
    },

    /// Environment variable should have expected value
    EnvVar { name: String, expected: String },

    /// Custom predicate (for extensibility)
    Custom {
        name: String,
        params: HashMap<String, String>,
    },

    /// Panic-attack attestation chain verification.
    ///
    /// Verifies a panic-attack `.attestation.json` sidecar file against
    /// the corresponding scan report. Checks hash chain integrity, nonce
    /// consistency, temporal ordering, plausibility metrics, and optionally
    /// the Ed25519 signature.
    PanicAttackAttestation {
        /// Path to the `.attestation.json` sidecar file.
        attestation_path: String,
        /// Path to the corresponding scan report JSON.
        report_path: String,
        /// Optional hex-encoded Ed25519 public key for signature verification.
        #[serde(default)]
        public_key: Option<String>,
    },
}

/// A claim that some action was performed
///
/// Claims are the core unit of verification. Each claim has a description
/// and a list of evidence specifications that should be true if the claim
/// is accurate.
///
/// # Examples
///
/// ```rust
/// use did_you_actually_do_that::{Claim, EvidenceSpec};
///
/// // Simple claim with one piece of evidence
/// let claim = Claim::new("Created test file")
///     .with_evidence(EvidenceSpec::FileExists {
///         path: "/tmp/test.txt".to_string(),
///     });
///
/// // Claim with multiple evidence pieces
/// let detailed_claim = Claim::new("Set up project structure")
///     .with_evidence(EvidenceSpec::DirectoryExists {
///         path: "/tmp/myproject/src".to_string(),
///     })
///     .with_evidence(EvidenceSpec::FileExists {
///         path: "/tmp/myproject/Cargo.toml".to_string(),
///     })
///     .with_source("my-ai-assistant");
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Claim {
    /// Unique identifier for this claim (auto-generated if not provided)
    #[serde(default = "Claim::default_id")]
    pub id: String,

    /// Human-readable description of what was claimed
    pub description: String,

    /// When the claim was made (defaults to now if not provided)
    #[serde(default = "Utc::now")]
    pub timestamp: DateTime<Utc>,

    /// Evidence that should exist if the claim is true
    pub evidence: Vec<EvidenceSpec>,

    /// Optional context about who/what made the claim
    pub source: Option<String>,
}

impl Claim {
    pub fn new(description: impl Into<String>) -> Self {
        let description_string = description.into();
        let claim_id = Self::generate_id(&description_string);
        Self {
            id: claim_id,
            description: description_string,
            timestamp: Utc::now(),
            evidence: Vec::new(),
            source: None,
        }
    }

    fn default_id() -> String {
        Self::generate_id("unnamed-claim")
    }

    pub fn with_evidence(mut self, evidence: EvidenceSpec) -> Self {
        self.evidence.push(evidence);
        self
    }

    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    fn generate_id(description: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(description.as_bytes());
        hasher.update(Utc::now().timestamp().to_le_bytes());
        hex::encode(&hasher.finalize()[..8])
    }
}

/// Result of verifying a single piece of evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvidenceResult {
    pub spec: EvidenceSpec,
    pub verdict: Verdict,
    pub details: Option<String>,
}

/// Complete verification report for a claim
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationReport {
    pub claim: Claim,
    pub evidence_results: Vec<EvidenceResult>,
    pub overall_verdict: Verdict,
    pub verified_at: DateTime<Utc>,
}

impl VerificationReport {
    /// Summary suitable for display
    pub fn summary(&self) -> String {
        let emoji = match self.overall_verdict {
            Verdict::Confirmed => "✓",
            Verdict::Refuted => "✗",
            Verdict::Inconclusive => "?",
            Verdict::Unverifiable => "⊘",
        };
        format!(
            "[{}] {} - {:?}",
            emoji, self.claim.description, self.overall_verdict
        )
    }
}

/// The main verifier that checks claims against reality
///
/// The Verifier is responsible for checking evidence and determining verdicts.
/// It supports all built-in evidence types and can be extended with custom
/// checkers for domain-specific verification.
///
/// # Examples
///
/// ## Basic Verification
///
/// ```rust
/// use did_you_actually_do_that::{Claim, EvidenceSpec, Verifier, Verdict};
///
/// let verifier = Verifier::new();
///
/// let claim = Claim::new("File should exist")
///     .with_evidence(EvidenceSpec::FileExists {
///         path: "/etc/passwd".to_string(),
///     });
///
/// let report = verifier.verify(&claim);
/// assert_eq!(report.overall_verdict, Verdict::Confirmed);
/// ```
///
/// ## Custom Checker
///
/// ```rust
/// use did_you_actually_do_that::{Claim, EvidenceSpec, Verifier, Verdict};
/// use std::collections::HashMap;
///
/// let mut verifier = Verifier::new();
///
/// // Register a custom checker
/// verifier.register_checker("is_even", |params| {
///     if let Some(num_str) = params.get("number") {
///         if let Ok(num) = num_str.parse::<i32>() {
///             return Ok(if num % 2 == 0 {
///                 Verdict::Confirmed
///             } else {
///                 Verdict::Refuted
///             });
///         }
///     }
///     Ok(Verdict::Unverifiable)
/// });
///
/// let mut params = HashMap::new();
/// params.insert("number".to_string(), "42".to_string());
///
/// let claim = Claim::new("Number is even")
///     .with_evidence(EvidenceSpec::Custom {
///         name: "is_even".to_string(),
///         params,
///     });
///
/// let report = verifier.verify(&claim);
/// assert_eq!(report.overall_verdict, Verdict::Confirmed);
/// ```
pub struct Verifier {
    /// Custom evidence checkers for extensibility
    #[allow(clippy::type_complexity)]
    custom_checkers: HashMap<
        String,
        Box<dyn Fn(&HashMap<String, String>) -> Result<Verdict, VerificationError>>,
    >,
}

impl Default for Verifier {
    fn default() -> Self {
        Self::new()
    }
}

impl Verifier {
    pub fn new() -> Self {
        Self {
            custom_checkers: HashMap::new(),
        }
    }

    /// Register a custom evidence checker
    pub fn register_checker<F>(&mut self, name: impl Into<String>, checker: F)
    where
        F: Fn(&HashMap<String, String>) -> Result<Verdict, VerificationError> + 'static,
    {
        self.custom_checkers.insert(name.into(), Box::new(checker));
    }

    /// Verify a single piece of evidence.
    ///
    /// Delegates to the appropriate verification layer module. Custom evidence
    /// types are handled directly here since they require the Verifier's
    /// registered checkers.
    pub fn check_evidence(&self, evidence: &EvidenceSpec) -> EvidenceResult {
        // Custom checks need access to self.custom_checkers, so handle here
        if let EvidenceSpec::Custom { name, params } = evidence {
            let (verdict, details) = if let Some(checker) = self.custom_checkers.get(name) {
                match checker(params) {
                    Ok(v) => (v, None),
                    Err(e) => (Verdict::Unverifiable, Some(e.to_string())),
                }
            } else {
                (
                    Verdict::Unverifiable,
                    Some(format!("No checker for: {}", name)),
                )
            };
            return EvidenceResult {
                spec: evidence.clone(),
                verdict,
                details,
            };
        }

        // All other evidence types delegate to layer modules
        let (_layer, result) = verifiers::check_evidence_by_layer(evidence);
        result
    }

    /// Verify a complete claim
    pub fn verify(&self, claim: &Claim) -> VerificationReport {
        if claim.evidence.is_empty() {
            return VerificationReport {
                claim: claim.clone(),
                evidence_results: vec![],
                overall_verdict: Verdict::Unverifiable,
                verified_at: Utc::now(),
            };
        }

        let evidence_results: Vec<EvidenceResult> = claim
            .evidence
            .iter()
            .map(|e| self.check_evidence(e))
            .collect();

        // Overall verdict: all must confirm for Confirmed, any refuted = Refuted
        let overall_verdict = if evidence_results
            .iter()
            .all(|r| r.verdict == Verdict::Confirmed)
        {
            Verdict::Confirmed
        } else if evidence_results
            .iter()
            .any(|r| r.verdict == Verdict::Refuted)
        {
            Verdict::Refuted
        } else if evidence_results
            .iter()
            .all(|r| r.verdict == Verdict::Unverifiable)
        {
            Verdict::Unverifiable
        } else {
            Verdict::Inconclusive
        };

        VerificationReport {
            claim: claim.clone(),
            evidence_results,
            overall_verdict,
            verified_at: Utc::now(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_claim_creation() {
        let claim = Claim::new("Created a file")
            .with_evidence(EvidenceSpec::FileExists {
                path: "/tmp/test.txt".to_string(),
            })
            .with_source("test-agent");

        assert!(!claim.id.is_empty());
        assert_eq!(claim.evidence.len(), 1);
        assert_eq!(claim.source, Some("test-agent".to_string()));
    }

    #[test]
    fn test_verdict_trustworthiness() {
        assert!(Verdict::Confirmed.is_trustworthy());
        assert!(!Verdict::Refuted.is_trustworthy());
        assert!(!Verdict::Inconclusive.is_trustworthy());
        assert!(!Verdict::Unverifiable.is_trustworthy());
    }
}
