// SPDX-License-Identifier: PMPL-1.0-or-later
//! Verification layers for the 12-layer verification pipeline.
//!
//! Each module implements one or more verification layers that check claims
//! against observable evidence. The layers are:
//!
//! | # | Layer              | Language     | Module                |
//! |---|--------------------|--------------|-----------------------|
//! | 1 | File Existence     | Rust         | `file_existence`      |
//! | 2 | Content Hash       | Rust         | `content_hash`        |
//! | 3 | Syntactic Validity | Rust/Chapel  | `syntactic`           |
//! | 4 | Semantic Integrity | Rust         | `semantic`            |
//! | 5 | Test Execution     | Elixir       | (brain-side)          |
//! | 6 | Diff Coherence     | Rust         | `diff_coherence`      |
//! | 7 | Dependency Resolve | Rust         | `dependency`          |
//! | 8 | Cross-Reference    | Elixir/Rust  | `cross_reference`     |
//! | 9 | Completeness Audit | Rust         | `completeness`        |
//! |10 | Regression Guard   | Elixir       | (brain-side)          |
//! |11 | SLM Consensus      | Rust/Elixir  | (sentinel-side)       |
//! |12 | Pattern Learning   | Elixir       | (brain-side)          |

pub mod attestation;
pub mod completeness;
pub mod content_hash;
pub mod cross_reference;
pub mod dependency;
pub mod diff_coherence;
pub mod file_existence;
pub mod semantic;
pub mod syntactic;

use crate::{EvidenceResult, EvidenceSpec, Verdict};
use serde::{Deserialize, Serialize};

/// Identifies which verification layer produced a result.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Layer {
    FileExistence = 1,
    ContentHash = 2,
    SyntacticValidity = 3,
    SemanticIntegrity = 4,
    TestExecution = 5,
    DiffCoherence = 6,
    DependencyResolution = 7,
    CrossReference = 8,
    CompletenessAudit = 9,
    RegressionGuard = 10,
    SlmConsensus = 11,
    PatternLearning = 12,
}

impl Layer {
    pub fn name(&self) -> &'static str {
        match self {
            Layer::FileExistence => "File Existence",
            Layer::ContentHash => "Content Hash",
            Layer::SyntacticValidity => "Syntactic Validity",
            Layer::SemanticIntegrity => "Semantic Integrity",
            Layer::TestExecution => "Test Execution",
            Layer::DiffCoherence => "Diff Coherence",
            Layer::DependencyResolution => "Dependency Resolution",
            Layer::CrossReference => "Cross-Reference",
            Layer::CompletenessAudit => "Completeness Audit",
            Layer::RegressionGuard => "Regression Guard",
            Layer::SlmConsensus => "SLM Consensus",
            Layer::PatternLearning => "Pattern Learning",
        }
    }

    pub fn number(&self) -> u32 {
        *self as u32
    }
}

/// Result from a specific verification layer, enriched with layer metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LayerResult {
    pub layer: Layer,
    pub evidence_result: EvidenceResult,
}

/// Trait for verification layer implementations.
///
/// Each layer checks a specific aspect of a claim. Layers that cannot handle
/// a given evidence type should return `false` from `can_handle()`.
pub trait VerificationLayer: Send + Sync {
    /// The layer identifier.
    fn layer(&self) -> Layer;

    /// Whether this layer can verify the given evidence type.
    fn can_handle(&self, evidence: &EvidenceSpec) -> bool;

    /// Verify a single piece of evidence. Only called if `can_handle()` returns true.
    fn check(&self, evidence: &EvidenceSpec) -> EvidenceResult;
}

/// Dispatch an evidence spec to the appropriate layer checker.
///
/// This is the central routing function that maps evidence types to layers.
/// It preserves the exact same behavior as the original `Verifier::check_evidence()`.
pub fn check_evidence_by_layer(evidence: &EvidenceSpec) -> (Layer, EvidenceResult) {
    match evidence {
        EvidenceSpec::FileExists { .. } | EvidenceSpec::DirectoryExists { .. } => {
            (Layer::FileExistence, file_existence::check(evidence))
        }

        EvidenceSpec::FileWithHash { .. } => {
            (Layer::ContentHash, content_hash::check(evidence))
        }

        EvidenceSpec::FileContains { .. }
        | EvidenceSpec::FileMatchesRegex { .. }
        | EvidenceSpec::FileJsonPath { .. }
        | EvidenceSpec::EnvVar { .. }
        | EvidenceSpec::FileModifiedAfter { .. } => {
            (Layer::SemanticIntegrity, semantic::check(evidence))
        }

        EvidenceSpec::CommandSucceeds { .. } => {
            // Commands are semantic checks (does running X produce expected result?)
            (Layer::SemanticIntegrity, semantic::check(evidence))
        }

        EvidenceSpec::GitClean { .. }
        | EvidenceSpec::GitCommitExists { .. }
        | EvidenceSpec::GitBranchExists { .. } => {
            (Layer::DiffCoherence, diff_coherence::check(evidence))
        }

        EvidenceSpec::PanicAttackAttestation { .. } => {
            // Attestation verification enriches ContentHash (Layer 2) with
            // cryptographic evidence that the scan was actually performed.
            (Layer::ContentHash, attestation::check(evidence))
        }

        EvidenceSpec::Custom { .. } => {
            // Custom checks route to cross-reference layer by default
            (
                Layer::CrossReference,
                EvidenceResult {
                    spec: evidence.clone(),
                    verdict: Verdict::Unverifiable,
                    details: Some("Custom checks require Verifier context".to_string()),
                },
            )
        }
    }
}
