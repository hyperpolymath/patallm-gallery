// SPDX-License-Identifier: PMPL-1.0-or-later
//! SLM Ensemble for Layer 11: SLM Consensus verification.
//!
//! Multiple small language models independently evaluate whether an LLM
//! actually performed claimed work. Uses asymmetric weighting (1.5x for
//! rejections) from the cognitive-gating three-tier model.
//!
//! ## Architecture
//!
//! ```text
//! Tier 1: Policy Oracle   → Deterministic rule checking (Nickel configs)
//! Tier 2: SLM Evaluator   → Multiple small models vote
//! Tier 3: Consensus Arbiter → Weighted vote aggregation (Elixir-side)
//! ```
//!
//! The Rust side handles Tiers 1 and 2. Tier 3 runs in the Elixir brain.

pub mod ensemble;
pub mod graph_of_thought;
pub mod mixture_of_experts;
pub mod policy_oracle;

use crate::Verdict;
use serde::{Deserialize, Serialize};

/// A vote from a single SLM or policy check.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vote {
    /// Which voter produced this (model name or "policy_oracle")
    pub voter: String,
    /// The vote decision
    pub decision: VoteDecision,
    /// Confidence in the decision (0.0 to 1.0)
    pub confidence: f64,
    /// Brief reasoning for the decision
    pub reasoning: String,
    /// Weight applied to this vote (1.0 normal, 1.5 for rejections)
    pub weight: f64,
}

/// The three possible vote outcomes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VoteDecision {
    /// The work appears genuinely completed
    Approve,
    /// The work appears incomplete, fabricated, or sloppy
    Reject,
    /// Cannot determine (insufficient evidence)
    Abstain,
}

impl VoteDecision {
    /// Convert to a verification Verdict.
    pub fn to_verdict(self) -> Verdict {
        match self {
            VoteDecision::Approve => Verdict::Confirmed,
            VoteDecision::Reject => Verdict::Refuted,
            VoteDecision::Abstain => Verdict::Inconclusive,
        }
    }
}

/// A claim context to be evaluated by the SLM ensemble.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvaluationContext {
    /// What the LLM claimed to have done
    pub claim_text: String,
    /// Evidence gathered by other layers (serialized results)
    pub evidence_summary: String,
    /// The actual code/content produced (if available, truncated)
    pub artifact_sample: Option<String>,
    /// Diff of changes (if available, truncated)
    pub diff_sample: Option<String>,
}

/// Result of the full SLM ensemble evaluation (Tiers 1 + 2).
///
/// The Elixir brain's consensus arbiter (Tier 3) uses this to make
/// the final GO/NO-GO decision.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnsembleResult {
    /// All votes from policy oracle and SLMs
    pub votes: Vec<Vote>,
    /// Summary statistics
    pub approve_weight: f64,
    pub reject_weight: f64,
    pub abstain_count: u32,
    /// Suggested verdict (before Tier 3 arbiter)
    pub suggested_verdict: Verdict,
}

/// Trait for SLM backends. Implementations provide model inference.
pub trait SlmBackend: Send + Sync {
    /// Name of the model (e.g. "TinyLlama-1.1B")
    fn model_name(&self) -> &str;

    /// Evaluate a claim context and return a vote.
    fn evaluate(&self, context: &EvaluationContext) -> Vote;

    /// Whether this backend is ready (model loaded, etc.)
    fn is_ready(&self) -> bool;
}

/// The 12 violation categories from cognitive-gating, adapted for verification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ViolationCategory {
    IncompleteImplementation = 1,
    MissingErrorHandling = 2,
    UntestedCodePaths = 3,
    StubPlaceholderLeftInPlace = 4,
    ClaimedButNonExistentFile = 5,
    BrokenImportsExports = 6,
    RegressionIntroduced = 7,
    DocumentationMismatch = 8,
    DependencyNotAdded = 9,
    ConfigurationNotUpdated = 10,
    TestNotActuallyRun = 11,
    ScopeSilentlyReduced = 12,
}

impl ViolationCategory {
    pub fn name(&self) -> &'static str {
        match self {
            Self::IncompleteImplementation => "Incomplete implementation",
            Self::MissingErrorHandling => "Missing error handling",
            Self::UntestedCodePaths => "Untested code paths",
            Self::StubPlaceholderLeftInPlace => "Stub/placeholder left in place",
            Self::ClaimedButNonExistentFile => "Claimed but non-existent file",
            Self::BrokenImportsExports => "Broken imports/exports",
            Self::RegressionIntroduced => "Regression introduced",
            Self::DocumentationMismatch => "Documentation mismatch",
            Self::DependencyNotAdded => "Dependency not added",
            Self::ConfigurationNotUpdated => "Configuration not updated",
            Self::TestNotActuallyRun => "Test not actually run",
            Self::ScopeSilentlyReduced => "Scope silently reduced",
        }
    }
}

/// A policy violation detected by the policy oracle.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyViolation {
    pub category: ViolationCategory,
    pub description: String,
    pub severity: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vote_decision_to_verdict() {
        assert_eq!(VoteDecision::Approve.to_verdict(), Verdict::Confirmed);
        assert_eq!(VoteDecision::Reject.to_verdict(), Verdict::Refuted);
        assert_eq!(VoteDecision::Abstain.to_verdict(), Verdict::Inconclusive);
    }

    #[test]
    fn test_violation_category_names() {
        assert_eq!(
            ViolationCategory::IncompleteImplementation.name(),
            "Incomplete implementation"
        );
        assert_eq!(
            ViolationCategory::ScopeSilentlyReduced.name(),
            "Scope silently reduced"
        );
    }

    #[test]
    fn test_vote_serialization() {
        let vote = Vote {
            voter: "test-model".to_string(),
            decision: VoteDecision::Reject,
            confidence: 0.85,
            reasoning: "Stub functions detected".to_string(),
            weight: 1.5,
        };
        let json = serde_json::to_string(&vote).unwrap();
        let back: Vote = serde_json::from_str(&json).unwrap();
        assert_eq!(back.voter, "test-model");
        assert_eq!(back.decision, VoteDecision::Reject);
        assert_eq!(back.weight, 1.5);
    }
}
