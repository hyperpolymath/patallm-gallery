// SPDX-License-Identifier: PMPL-1.0-or-later
//! Graph-of-Thought (GoT) reasoning backend for the SLM ensemble.
//!
//! Uses three independent reasoning paths to evaluate claims from different
//! adversarial perspectives. Each path is configured with a different
//! PolicyOracle bias to force diversified reasoning:
//!
//! - **Path A (AssumeGenuine)**: Assumes work is real. Looks for contradictions
//!   that would invalidate genuine work. High bar for rejection.
//!
//! - **Path B (AssumeFabricated)**: Assumes work is fake. Looks for positive
//!   evidence that real work was done. High bar for approval.
//!
//! - **Path C (AssumeIncomplete)**: Assumes work is partially done. Checks
//!   whether claimed scope matches actual scope.
//!
//! ## Aggregation: Pessimistic Union
//!
//! If ANY path finds a problem, the problem is reported. This means the
//! GoT backend is naturally conservative — it's harder to get a clean bill
//! of health than from a single evaluator.

use super::{
    policy_oracle::PolicyOracle, EvaluationContext, SlmBackend, Vote, VoteDecision,
    PolicyViolation, ViolationCategory,
};
use serde::{Deserialize, Serialize};

/// Per-path reasoning trace.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReasoningPath {
    pub perspective: String,
    pub vote: Vote,
    pub violations_found: usize,
    pub reasoning_trace: String,
}

/// Full GoT result with per-path traces.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoTResult {
    pub paths: Vec<ReasoningPath>,
    pub aggregated_vote: Vote,
}

/// Graph-of-Thought backend implementing the SlmBackend trait.
pub struct GraphOfThought {
    oracle: PolicyOracle,
}

impl GraphOfThought {
    pub fn new() -> Self {
        Self {
            oracle: PolicyOracle::new(),
        }
    }

    /// Run all three reasoning paths and aggregate results.
    pub fn evaluate_with_traces(&self, context: &EvaluationContext) -> GoTResult {
        let path_a = self.path_assume_genuine(context);
        let path_b = self.path_assume_fabricated(context);
        let path_c = self.path_assume_incomplete(context);

        let paths = vec![path_a, path_b, path_c];

        // Pessimistic union: any rejection = rejection
        let any_reject = paths.iter().any(|p| p.vote.decision == VoteDecision::Reject);
        let all_approve = paths.iter().all(|p| p.vote.decision == VoteDecision::Approve);

        let (decision, confidence, reasoning) = if any_reject {
            let reject_paths: Vec<&ReasoningPath> = paths
                .iter()
                .filter(|p| p.vote.decision == VoteDecision::Reject)
                .collect();

            let max_conf = reject_paths
                .iter()
                .map(|p| p.vote.confidence)
                .fold(0.0f64, f64::max);

            let reasons: Vec<String> = reject_paths
                .iter()
                .map(|p| format!("[{}] {}", p.perspective, p.vote.reasoning))
                .collect();

            (VoteDecision::Reject, max_conf, reasons.join("; "))
        } else if all_approve {
            let min_conf = paths
                .iter()
                .map(|p| p.vote.confidence)
                .fold(1.0f64, f64::min);

            (
                VoteDecision::Approve,
                min_conf,
                "All three reasoning paths approve".to_string(),
            )
        } else {
            (
                VoteDecision::Abstain,
                0.4,
                "Mixed results across reasoning paths".to_string(),
            )
        };

        let aggregated_vote = Vote {
            voter: "graph_of_thought".to_string(),
            decision,
            confidence,
            reasoning,
            weight: if decision == VoteDecision::Reject { 1.5 } else { 1.0 },
        };

        GoTResult {
            paths,
            aggregated_vote,
        }
    }

    /// Path A: Assume work is genuine, look for contradictions.
    fn path_assume_genuine(&self, context: &EvaluationContext) -> ReasoningPath {
        let violations = self.oracle.check(context);

        // As a "genuine" path, only hard violations count
        let hard_violations: Vec<&PolicyViolation> = violations
            .iter()
            .filter(|v| v.severity >= 0.8)
            .collect();

        let (decision, confidence, trace) = if hard_violations.is_empty() {
            (
                VoteDecision::Approve,
                0.75,
                "Assuming genuine: no hard contradictions found".to_string(),
            )
        } else {
            let reasons: Vec<String> = hard_violations
                .iter()
                .map(|v| format!("  - {}: {}", v.category.name(), v.description))
                .collect();
            (
                VoteDecision::Reject,
                hard_violations.iter().map(|v| v.severity).fold(0.0f64, f64::max),
                format!(
                    "Assuming genuine but found {} hard contradiction(s):\n{}",
                    hard_violations.len(),
                    reasons.join("\n")
                ),
            )
        };

        ReasoningPath {
            perspective: "AssumeGenuine".to_string(),
            vote: Vote {
                voter: "got_path_a".to_string(),
                decision,
                confidence,
                reasoning: trace.clone(),
                weight: if decision == VoteDecision::Reject { 1.5 } else { 1.0 },
            },
            violations_found: hard_violations.len(),
            reasoning_trace: trace,
        }
    }

    /// Path B: Assume work is fabricated, look for positive evidence of real work.
    fn path_assume_fabricated(&self, context: &EvaluationContext) -> ReasoningPath {
        let mut evidence_of_real_work = Vec::new();

        if let Some(ref artifact) = context.artifact_sample {
            // Real work tends to have: error handling, comments, varied line lengths
            let has_error_handling = artifact.contains("Error")
                || artifact.contains("error")
                || artifact.contains("Result")
                || artifact.contains("rescue")
                || artifact.contains("catch")
                || artifact.contains("?");

            let has_varied_structure = {
                let lengths: Vec<usize> = artifact.lines().map(|l| l.len()).collect();
                if lengths.len() > 2 {
                    let mean = lengths.iter().sum::<usize>() as f64 / lengths.len() as f64;
                    let variance = lengths
                        .iter()
                        .map(|&l| (l as f64 - mean).powi(2))
                        .sum::<f64>()
                        / lengths.len() as f64;
                    variance > 100.0 // Real code has varied line lengths
                } else {
                    false
                }
            };

            let has_domain_logic = artifact.lines().count() > 5
                && !artifact.contains("todo!()")
                && !artifact.contains("unimplemented!()");

            if has_error_handling {
                evidence_of_real_work.push("Contains error handling");
            }
            if has_varied_structure {
                evidence_of_real_work.push("Has varied code structure");
            }
            if has_domain_logic {
                evidence_of_real_work.push("Contains domain-specific logic");
            }
        }

        // Also check evidence summary for confirmed results
        if context.evidence_summary.contains("Confirmed") {
            evidence_of_real_work.push("Other layers confirmed evidence");
        }

        let (decision, confidence, trace) = if evidence_of_real_work.len() >= 2 {
            (
                VoteDecision::Approve,
                0.6 + (evidence_of_real_work.len() as f64 * 0.1),
                format!(
                    "Assuming fabricated but found {} positive signals: {}",
                    evidence_of_real_work.len(),
                    evidence_of_real_work.join(", ")
                ),
            )
        } else if evidence_of_real_work.is_empty() {
            (
                VoteDecision::Reject,
                0.7,
                "Assuming fabricated: no positive evidence of real work found".to_string(),
            )
        } else {
            (
                VoteDecision::Abstain,
                0.4,
                format!(
                    "Assuming fabricated: weak evidence — {}",
                    evidence_of_real_work.join(", ")
                ),
            )
        };

        ReasoningPath {
            perspective: "AssumeFabricated".to_string(),
            vote: Vote {
                voter: "got_path_b".to_string(),
                decision,
                confidence: confidence.min(1.0),
                reasoning: trace.clone(),
                weight: if decision == VoteDecision::Reject { 1.5 } else { 1.0 },
            },
            violations_found: if evidence_of_real_work.is_empty() { 1 } else { 0 },
            reasoning_trace: trace,
        }
    }

    /// Path C: Assume work is incomplete, check scope matches claims.
    fn path_assume_incomplete(&self, context: &EvaluationContext) -> ReasoningPath {
        let violations = self.oracle.check(context);

        // Count scope-related violations
        let scope_violations: Vec<&PolicyViolation> = violations
            .iter()
            .filter(|v| {
                matches!(
                    v.category,
                    ViolationCategory::ScopeSilentlyReduced
                        | ViolationCategory::IncompleteImplementation
                        | ViolationCategory::StubPlaceholderLeftInPlace
                )
            })
            .collect();

        // Estimate claimed scope from description
        let claimed_scope = context
            .claim_text
            .split(&[',', ';', '\n', '.'][..])
            .filter(|s| s.len() > 5)
            .count();

        // Estimate actual scope from artifact
        let actual_lines = context
            .artifact_sample
            .as_ref()
            .map(|a| a.lines().filter(|l| !l.trim().is_empty()).count())
            .unwrap_or(0);

        let scope_mismatch = claimed_scope > 3 && actual_lines < claimed_scope * 5;

        let (decision, confidence, trace) = if !scope_violations.is_empty() || scope_mismatch {
            let mut issues = Vec::new();
            for v in &scope_violations {
                issues.push(format!("{}: {}", v.category.name(), v.description));
            }
            if scope_mismatch {
                issues.push(format!(
                    "Scope mismatch: ~{} claimed items but only {} non-empty lines",
                    claimed_scope, actual_lines
                ));
            }
            (
                VoteDecision::Reject,
                0.65 + (scope_violations.len() as f64 * 0.1),
                format!(
                    "Assuming incomplete — found {} issue(s): {}",
                    issues.len(),
                    issues.join("; ")
                ),
            )
        } else {
            (
                VoteDecision::Approve,
                0.65,
                format!(
                    "Assuming incomplete but scope appears adequate ({} claimed, {} lines)",
                    claimed_scope, actual_lines
                ),
            )
        };

        ReasoningPath {
            perspective: "AssumeIncomplete".to_string(),
            vote: Vote {
                voter: "got_path_c".to_string(),
                decision,
                confidence: confidence.min(1.0),
                reasoning: trace.clone(),
                weight: if decision == VoteDecision::Reject { 1.5 } else { 1.0 },
            },
            violations_found: scope_violations.len() + if scope_mismatch { 1 } else { 0 },
            reasoning_trace: trace,
        }
    }
}

impl Default for GraphOfThought {
    fn default() -> Self {
        Self::new()
    }
}

impl SlmBackend for GraphOfThought {
    fn model_name(&self) -> &str {
        "graph_of_thought"
    }

    fn evaluate(&self, context: &EvaluationContext) -> Vote {
        self.evaluate_with_traces(context).aggregated_vote
    }

    fn is_ready(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn clean_context() -> EvaluationContext {
        EvaluationContext {
            claim_text: "Added login function".to_string(),
            evidence_summary: "FileExists: Confirmed, ContentHash: Confirmed".to_string(),
            artifact_sample: Some(
                "pub fn login(email: &str, password: &str) -> Result<Session, AuthError> {\n\
                     let user = db.find_by_email(email)?;\n\
                     if verify_hash(password, &user.password_hash) {\n\
                         Ok(Session::create(user.id))\n\
                     } else {\n\
                         Err(AuthError::InvalidCredentials)\n\
                     }\n\
                 }\n"
                .to_string(),
            ),
            diff_sample: None,
        }
    }

    fn stub_context() -> EvaluationContext {
        EvaluationContext {
            claim_text: "Fully implemented the entire compiler backend".to_string(),
            evidence_summary: "FileExists: Confirmed".to_string(),
            artifact_sample: Some("fn compile() { todo!() }\n".to_string()),
            diff_sample: None,
        }
    }

    #[test]
    fn test_got_clean_code_approves() {
        let got = GraphOfThought::new();
        let result = got.evaluate_with_traces(&clean_context());

        assert_eq!(result.paths.len(), 3);
        assert_eq!(result.aggregated_vote.decision, VoteDecision::Approve);
    }

    #[test]
    fn test_got_stub_code_rejects() {
        let got = GraphOfThought::new();
        let result = got.evaluate_with_traces(&stub_context());

        assert_eq!(result.aggregated_vote.decision, VoteDecision::Reject);
        assert!(result.aggregated_vote.weight >= 1.5);
    }

    #[test]
    fn test_three_paths_always_present() {
        let got = GraphOfThought::new();
        let result = got.evaluate_with_traces(&clean_context());

        let perspectives: Vec<&str> = result.paths.iter().map(|p| p.perspective.as_str()).collect();
        assert!(perspectives.contains(&"AssumeGenuine"));
        assert!(perspectives.contains(&"AssumeFabricated"));
        assert!(perspectives.contains(&"AssumeIncomplete"));
    }

    #[test]
    fn test_pessimistic_union() {
        let got = GraphOfThought::new();

        // Stub code: Path A (genuine) should reject on hard violation,
        // Path B (fabricated) should reject on no real work evidence
        let result = got.evaluate_with_traces(&stub_context());
        let any_reject = result.paths.iter().any(|p| p.vote.decision == VoteDecision::Reject);
        assert!(any_reject);
        assert_eq!(result.aggregated_vote.decision, VoteDecision::Reject);
    }

    #[test]
    fn test_slm_backend_trait() {
        let got = GraphOfThought::new();
        assert_eq!(got.model_name(), "graph_of_thought");
        assert!(got.is_ready());

        let vote = got.evaluate(&clean_context());
        assert_eq!(vote.voter, "graph_of_thought");
    }
}
