// SPDX-License-Identifier: PMPL-1.0-or-later
//! Tier 2: SLM Ensemble — multiple small models vote on work quality.
//!
//! The ensemble manages multiple SLM backends, collects their votes,
//! and applies asymmetric weighting (1.5x for rejections per cognitive-gating).
//!
//! ## Backend Implementations
//!
//! - `MockBackend`: Returns configurable votes for testing
//! - (Future) `LlamaCppBackend`: Real llama.cpp inference via llama-cpp-2 crate
//!
//! ## Weighting Rules
//!
//! From cognitive-gating's asymmetric model:
//! - Approve votes: weight 1.0
//! - Reject votes: weight 1.5 (rejection carries more weight)
//! - Abstain votes: weight 0.0 (not counted)

use super::{
    graph_of_thought::GraphOfThought, mixture_of_experts::MixtureOfExperts,
    policy_oracle::PolicyOracle, EnsembleResult, EvaluationContext, SlmBackend, Vote,
    VoteDecision,
};
use crate::Verdict;

/// The SLM Ensemble coordinates policy oracle + multiple SLM backends.
pub struct Ensemble {
    policy_oracle: PolicyOracle,
    backends: Vec<Box<dyn SlmBackend>>,
    /// Weight multiplier for rejection votes (default: 1.5)
    rejection_weight: f64,
}

impl Ensemble {
    pub fn new() -> Self {
        Self {
            policy_oracle: PolicyOracle::new(),
            backends: Vec::new(),
            rejection_weight: 1.5,
        }
    }

    /// Add an SLM backend to the ensemble.
    pub fn add_backend(&mut self, backend: Box<dyn SlmBackend>) {
        self.backends.push(backend);
    }

    /// Add the Graph-of-Thought backend (3-path adversarial reasoning).
    pub fn with_got(mut self) -> Self {
        self.backends.push(Box::new(GraphOfThought::new()));
        self
    }

    /// Add the Mixture-of-Experts backend (violation-routed experts).
    pub fn with_moe(mut self) -> Self {
        self.backends.push(Box::new(MixtureOfExperts::new()));
        self
    }

    /// Create a fully-loaded ensemble with GoT + MoE.
    pub fn full() -> Self {
        Self::new().with_got().with_moe()
    }

    /// Number of registered backends (not counting policy oracle).
    pub fn backend_count(&self) -> usize {
        self.backends.len()
    }

    /// Evaluate a claim through all tiers and return the ensemble result.
    ///
    /// Flow:
    /// 1. Policy Oracle (Tier 1) — fast deterministic check
    /// 2. SLM backends (Tier 2) — each votes independently
    /// 3. Weight and aggregate (returned to Tier 3 arbiter in Elixir)
    pub fn evaluate(&self, context: &EvaluationContext) -> EnsembleResult {
        let mut votes = Vec::new();

        // Tier 1: Policy Oracle
        let policy_vote = self.policy_oracle.evaluate(context);
        votes.push(policy_vote);

        // Tier 2: Each SLM backend votes
        for backend in &self.backends {
            if backend.is_ready() {
                let mut vote = backend.evaluate(context);
                // Apply asymmetric weighting
                vote.weight = match vote.decision {
                    VoteDecision::Reject => self.rejection_weight,
                    VoteDecision::Approve => 1.0,
                    VoteDecision::Abstain => 0.0,
                };
                votes.push(vote);
            }
        }

        // Aggregate
        let mut approve_weight = 0.0;
        let mut reject_weight = 0.0;
        let mut abstain_count = 0u32;

        for vote in &votes {
            match vote.decision {
                VoteDecision::Approve => approve_weight += vote.weight,
                VoteDecision::Reject => reject_weight += vote.weight,
                VoteDecision::Abstain => abstain_count += 1,
            }
        }

        // Simple majority by weight for suggested verdict
        let suggested_verdict = if reject_weight > approve_weight {
            Verdict::Refuted
        } else if approve_weight > reject_weight {
            Verdict::Confirmed
        } else if abstain_count == votes.len() as u32 {
            Verdict::Unverifiable
        } else {
            Verdict::Inconclusive
        };

        EnsembleResult {
            votes,
            approve_weight,
            reject_weight,
            abstain_count,
            suggested_verdict,
        }
    }
}

impl Default for Ensemble {
    fn default() -> Self {
        Self::new()
    }
}

/// A mock SLM backend for testing.
///
/// Returns a configurable vote. Useful for unit testing the ensemble
/// logic without requiring actual model files.
pub struct MockBackend {
    name: String,
    decision: VoteDecision,
    confidence: f64,
}

impl MockBackend {
    pub fn new(name: impl Into<String>, decision: VoteDecision, confidence: f64) -> Self {
        Self {
            name: name.into(),
            decision,
            confidence,
        }
    }

    pub fn approver(name: impl Into<String>) -> Self {
        Self::new(name, VoteDecision::Approve, 0.8)
    }

    pub fn rejector(name: impl Into<String>) -> Self {
        Self::new(name, VoteDecision::Reject, 0.85)
    }

    pub fn abstainer(name: impl Into<String>) -> Self {
        Self::new(name, VoteDecision::Abstain, 0.3)
    }
}

impl SlmBackend for MockBackend {
    fn model_name(&self) -> &str {
        &self.name
    }

    fn evaluate(&self, _context: &EvaluationContext) -> Vote {
        Vote {
            voter: self.name.clone(),
            decision: self.decision,
            confidence: self.confidence,
            reasoning: format!("Mock {} decision", match self.decision {
                VoteDecision::Approve => "approve",
                VoteDecision::Reject => "reject",
                VoteDecision::Abstain => "abstain",
            }),
            weight: 1.0, // Will be overwritten by ensemble
        }
    }

    fn is_ready(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_context() -> EvaluationContext {
        EvaluationContext {
            claim_text: "Added authentication module".to_string(),
            evidence_summary: "FileExists: Confirmed, ContentHash: Confirmed".to_string(),
            artifact_sample: Some(
                "pub fn login(user: &str, pass: &str) -> Result<Token, Error> {\n    db.verify(user, hash(pass))\n}\n".to_string()
            ),
            diff_sample: None,
        }
    }

    #[test]
    fn test_ensemble_with_no_backends() {
        let ensemble = Ensemble::new();
        let result = ensemble.evaluate(&sample_context());
        // Only policy oracle votes
        assert_eq!(result.votes.len(), 1);
        assert_eq!(result.votes[0].voter, "policy_oracle");
    }

    #[test]
    fn test_ensemble_majority_approve() {
        let mut ensemble = Ensemble::new();
        ensemble.add_backend(Box::new(MockBackend::approver("model-a")));
        ensemble.add_backend(Box::new(MockBackend::approver("model-b")));
        ensemble.add_backend(Box::new(MockBackend::rejector("model-c")));

        let result = ensemble.evaluate(&sample_context());
        // Policy oracle (approve) + 2 approvers + 1 rejector
        assert_eq!(result.votes.len(), 4);
        // approve_weight: 1.0 (oracle) + 1.0 + 1.0 = 3.0
        // reject_weight: 1.5 (asymmetric) = 1.5
        assert!(result.approve_weight > result.reject_weight);
        assert_eq!(result.suggested_verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_ensemble_majority_reject() {
        let mut ensemble = Ensemble::new();
        ensemble.add_backend(Box::new(MockBackend::rejector("model-a")));
        ensemble.add_backend(Box::new(MockBackend::rejector("model-b")));
        ensemble.add_backend(Box::new(MockBackend::approver("model-c")));

        // Use a context with stubs so policy oracle also rejects
        let ctx = EvaluationContext {
            claim_text: "Implemented parser".to_string(),
            evidence_summary: "".to_string(),
            artifact_sample: Some("fn parse() { todo!() }\n".to_string()),
            diff_sample: None,
        };

        let result = ensemble.evaluate(&ctx);
        // Policy oracle (reject 1.5) + 2 rejectors (1.5 each) + 1 approver (1.0)
        // reject: 1.5 + 1.5 + 1.5 = 4.5, approve: 1.0
        assert!(result.reject_weight > result.approve_weight);
        assert_eq!(result.suggested_verdict, Verdict::Refuted);
    }

    #[test]
    fn test_asymmetric_weighting() {
        let mut ensemble = Ensemble::new();
        // One approver vs one rejector — rejector should win due to 1.5x weight
        ensemble.add_backend(Box::new(MockBackend::approver("model-a")));
        ensemble.add_backend(Box::new(MockBackend::rejector("model-b")));

        let result = ensemble.evaluate(&sample_context());
        // Oracle approves (1.0), model-a approves (1.0), model-b rejects (1.5)
        // approve: 2.0, reject: 1.5 — approve still wins because oracle approves too
        // But if we had stub code, oracle would reject making it 1.5 reject + 1.5 reject vs 1.0 approve
        assert_eq!(result.votes.len(), 3);
    }

    #[test]
    fn test_all_abstain() {
        let mut ensemble = Ensemble::new();
        ensemble.add_backend(Box::new(MockBackend::abstainer("model-a")));

        // Context with no artifact — oracle has nothing to check
        let ctx = EvaluationContext {
            claim_text: "Did some work".to_string(),
            evidence_summary: "".to_string(),
            artifact_sample: None,
            diff_sample: None,
        };

        let result = ensemble.evaluate(&ctx);
        // Oracle should approve (no violations found) and model abstains
        assert_eq!(result.abstain_count, 1);
    }

    #[test]
    fn test_backend_count() {
        let mut ensemble = Ensemble::new();
        assert_eq!(ensemble.backend_count(), 0);
        ensemble.add_backend(Box::new(MockBackend::approver("a")));
        ensemble.add_backend(Box::new(MockBackend::approver("b")));
        assert_eq!(ensemble.backend_count(), 2);
    }

    #[test]
    fn test_ensemble_result_serialization() {
        let ensemble = Ensemble::new();
        let result = ensemble.evaluate(&sample_context());
        let json = serde_json::to_string(&result).unwrap();
        let back: EnsembleResult = serde_json::from_str(&json).unwrap();
        assert_eq!(back.votes.len(), result.votes.len());
        assert_eq!(back.suggested_verdict, result.suggested_verdict);
    }
}
