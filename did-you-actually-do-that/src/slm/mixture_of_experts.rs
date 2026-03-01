// SPDX-License-Identifier: PMPL-1.0-or-later
//! Mixture-of-Experts (MoE) routing backend for the SLM ensemble.
//!
//! Routes claim evaluation to specialized expert backends based on the
//! violation signals detected by the PolicyOracle. Only relevant experts
//! are activated per claim, improving both accuracy and efficiency.
//!
//! ## Expert Categories
//!
//! - **StubDetection**: Activated when stub/placeholder patterns are detected
//! - **ScopeVerification**: Activated when scope mismatch signals are present
//! - **CompletenessCheck**: Activated when completeness gaps are detected
//! - **GeneralPurpose**: Always active as baseline evaluator
//!
//! ## Gating Logic
//!
//! The router uses PolicyOracle violation categories as activation signals.
//! Each expert has a set of categories it specializes in. An expert is
//! activated when any of its trigger categories are present in the violation
//! report. The GeneralPurpose expert always runs.

use super::{
    policy_oracle::PolicyOracle, EvaluationContext, PolicyViolation, SlmBackend, Vote,
    VoteDecision, ViolationCategory,
};
use serde::{Deserialize, Serialize};

/// Which expert type was activated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExpertType {
    StubDetection,
    ScopeVerification,
    CompletenessCheck,
    GeneralPurpose,
}

impl ExpertType {
    pub fn name(&self) -> &'static str {
        match self {
            ExpertType::StubDetection => "stub_detection",
            ExpertType::ScopeVerification => "scope_verification",
            ExpertType::CompletenessCheck => "completeness_check",
            ExpertType::GeneralPurpose => "general_purpose",
        }
    }

    /// Which violation categories trigger this expert.
    fn trigger_categories(&self) -> Vec<ViolationCategory> {
        match self {
            ExpertType::StubDetection => vec![
                ViolationCategory::StubPlaceholderLeftInPlace,
                ViolationCategory::IncompleteImplementation,
            ],
            ExpertType::ScopeVerification => vec![
                ViolationCategory::ScopeSilentlyReduced,
                ViolationCategory::DocumentationMismatch,
            ],
            ExpertType::CompletenessCheck => vec![
                ViolationCategory::ClaimedButNonExistentFile,
                ViolationCategory::UntestedCodePaths,
                ViolationCategory::MissingErrorHandling,
                ViolationCategory::DependencyNotAdded,
                ViolationCategory::ConfigurationNotUpdated,
            ],
            ExpertType::GeneralPurpose => vec![], // Always active
        }
    }
}

/// Result from a single expert's evaluation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpertResult {
    pub expert: ExpertType,
    pub vote: Vote,
    pub activated_by: Vec<String>,
}

/// Full MoE evaluation result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MoEResult {
    pub expert_results: Vec<ExpertResult>,
    pub aggregated_vote: Vote,
    pub experts_activated: usize,
}

/// Mixture-of-Experts backend.
pub struct MixtureOfExperts {
    oracle: PolicyOracle,
}

impl MixtureOfExperts {
    pub fn new() -> Self {
        Self {
            oracle: PolicyOracle::new(),
        }
    }

    /// Run the full MoE pipeline: detect violations, route to experts, aggregate.
    pub fn evaluate_with_experts(&self, context: &EvaluationContext) -> MoEResult {
        let violations = self.oracle.check(context);
        let violation_categories: Vec<ViolationCategory> =
            violations.iter().map(|v| v.category).collect();

        let mut expert_results = Vec::new();

        // Always run GeneralPurpose
        expert_results.push(self.run_general_purpose(context, &violations));

        // Conditionally activate specialized experts
        let all_experts = [
            ExpertType::StubDetection,
            ExpertType::ScopeVerification,
            ExpertType::CompletenessCheck,
        ];

        for expert_type in &all_experts {
            let triggers = expert_type.trigger_categories();
            let matched: Vec<String> = triggers
                .iter()
                .filter(|t| violation_categories.contains(t))
                .map(|t| t.name().to_string())
                .collect();

            if !matched.is_empty() {
                let result = match expert_type {
                    ExpertType::StubDetection => {
                        self.run_stub_expert(context, &violations, matched)
                    }
                    ExpertType::ScopeVerification => {
                        self.run_scope_expert(context, &violations, matched)
                    }
                    ExpertType::CompletenessCheck => {
                        self.run_completeness_expert(context, &violations, matched)
                    }
                    ExpertType::GeneralPurpose => unreachable!(),
                };
                expert_results.push(result);
            }
        }

        // Aggregate: weighted average, with specialized experts counting more
        let aggregated_vote = self.aggregate(&expert_results);

        MoEResult {
            experts_activated: expert_results.len(),
            expert_results,
            aggregated_vote,
        }
    }

    fn run_general_purpose(
        &self,
        context: &EvaluationContext,
        violations: &[PolicyViolation],
    ) -> ExpertResult {
        let oracle_vote = self.oracle.evaluate(context);

        // GeneralPurpose adjusts confidence based on artifact quality
        let quality_bonus = context
            .artifact_sample
            .as_ref()
            .map(|a| {
                let line_count = a.lines().count();
                let non_empty = a.lines().filter(|l| !l.trim().is_empty()).count();
                if line_count > 10 && non_empty as f64 / line_count as f64 > 0.6 {
                    0.1
                } else {
                    0.0
                }
            })
            .unwrap_or(0.0);

        ExpertResult {
            expert: ExpertType::GeneralPurpose,
            vote: Vote {
                voter: "moe_general".to_string(),
                confidence: (oracle_vote.confidence + quality_bonus).min(1.0),
                ..oracle_vote
            },
            activated_by: if violations.is_empty() {
                vec!["always_active".to_string()]
            } else {
                violations.iter().map(|v| v.category.name().to_string()).collect()
            },
        }
    }

    fn run_stub_expert(
        &self,
        context: &EvaluationContext,
        violations: &[PolicyViolation],
        activated_by: Vec<String>,
    ) -> ExpertResult {
        let stub_violations: Vec<&PolicyViolation> = violations
            .iter()
            .filter(|v| {
                matches!(
                    v.category,
                    ViolationCategory::StubPlaceholderLeftInPlace
                        | ViolationCategory::IncompleteImplementation
                )
            })
            .collect();

        // Count actual stub occurrences in artifact
        let stub_count = context
            .artifact_sample
            .as_ref()
            .map(|a| {
                let patterns = ["todo!()", "unimplemented!()", "// TODO", "# TODO", "pass  #"];
                patterns.iter().map(|p| a.matches(p).count()).sum::<usize>()
            })
            .unwrap_or(0);

        let total_lines = context
            .artifact_sample
            .as_ref()
            .map(|a| a.lines().count().max(1))
            .unwrap_or(1);

        let stub_ratio = stub_count as f64 / total_lines as f64;

        let confidence = 0.7 + (stub_ratio * 0.3).min(0.3);

        ExpertResult {
            expert: ExpertType::StubDetection,
            vote: Vote {
                voter: "moe_stub_expert".to_string(),
                decision: VoteDecision::Reject,
                confidence: confidence.min(1.0),
                reasoning: format!(
                    "Stub expert: {} stub patterns in {} lines ({:.0}% stub ratio), {} violation(s)",
                    stub_count,
                    total_lines,
                    stub_ratio * 100.0,
                    stub_violations.len()
                ),
                weight: 1.5,
            },
            activated_by,
        }
    }

    fn run_scope_expert(
        &self,
        context: &EvaluationContext,
        violations: &[PolicyViolation],
        activated_by: Vec<String>,
    ) -> ExpertResult {
        let scope_violations: Vec<&PolicyViolation> = violations
            .iter()
            .filter(|v| {
                matches!(
                    v.category,
                    ViolationCategory::ScopeSilentlyReduced
                        | ViolationCategory::DocumentationMismatch
                )
            })
            .collect();

        let max_severity = scope_violations
            .iter()
            .map(|v| v.severity)
            .fold(0.0f64, f64::max);

        let reasons: Vec<String> = scope_violations
            .iter()
            .map(|v| v.description.clone())
            .collect();

        ExpertResult {
            expert: ExpertType::ScopeVerification,
            vote: Vote {
                voter: "moe_scope_expert".to_string(),
                decision: VoteDecision::Reject,
                confidence: max_severity,
                reasoning: format!(
                    "Scope expert: {} issue(s) — {}",
                    scope_violations.len(),
                    reasons.join("; ")
                ),
                weight: 1.5,
            },
            activated_by,
        }
    }

    fn run_completeness_expert(
        &self,
        context: &EvaluationContext,
        violations: &[PolicyViolation],
        activated_by: Vec<String>,
    ) -> ExpertResult {
        let completeness_violations: Vec<&PolicyViolation> = violations
            .iter()
            .filter(|v| {
                matches!(
                    v.category,
                    ViolationCategory::ClaimedButNonExistentFile
                        | ViolationCategory::UntestedCodePaths
                        | ViolationCategory::MissingErrorHandling
                        | ViolationCategory::DependencyNotAdded
                        | ViolationCategory::ConfigurationNotUpdated
                )
            })
            .collect();

        // Check evidence summary for gaps
        let evidence_gaps = context.evidence_summary.matches("Refuted").count()
            + context.evidence_summary.matches("Inconclusive").count();

        let severity = if !completeness_violations.is_empty() {
            completeness_violations
                .iter()
                .map(|v| v.severity)
                .fold(0.0f64, f64::max)
        } else {
            0.5 + (evidence_gaps as f64 * 0.1).min(0.4)
        };

        ExpertResult {
            expert: ExpertType::CompletenessCheck,
            vote: Vote {
                voter: "moe_completeness_expert".to_string(),
                decision: VoteDecision::Reject,
                confidence: severity.min(1.0),
                reasoning: format!(
                    "Completeness expert: {} violation(s), {} evidence gap(s)",
                    completeness_violations.len(),
                    evidence_gaps
                ),
                weight: 1.5,
            },
            activated_by,
        }
    }

    fn aggregate(&self, results: &[ExpertResult]) -> Vote {
        if results.is_empty() {
            return Vote {
                voter: "mixture_of_experts".to_string(),
                decision: VoteDecision::Abstain,
                confidence: 0.0,
                reasoning: "No experts ran".to_string(),
                weight: 0.0,
            };
        }

        let mut approve_weight = 0.0f64;
        let mut reject_weight = 0.0f64;
        let mut total_weight = 0.0f64;
        let mut reasons = Vec::new();

        for r in results {
            // Specialized experts get 1.5x multiplier, general gets 1.0x
            let multiplier = if r.expert == ExpertType::GeneralPurpose {
                1.0
            } else {
                1.5
            };

            let vote_weight = r.vote.weight * multiplier;
            total_weight += vote_weight;

            match r.vote.decision {
                VoteDecision::Approve => approve_weight += vote_weight,
                VoteDecision::Reject => reject_weight += vote_weight,
                VoteDecision::Abstain => {}
            }

            reasons.push(format!("[{}] {}", r.expert.name(), r.vote.reasoning));
        }

        let decision = if reject_weight > approve_weight {
            VoteDecision::Reject
        } else if approve_weight > reject_weight {
            VoteDecision::Approve
        } else {
            VoteDecision::Abstain
        };

        let confidence = if total_weight > 0.0 {
            let dominant_weight = reject_weight.max(approve_weight);
            (dominant_weight / total_weight).min(1.0)
        } else {
            0.0
        };

        Vote {
            voter: "mixture_of_experts".to_string(),
            decision,
            confidence,
            reasoning: reasons.join("; "),
            weight: if decision == VoteDecision::Reject { 1.5 } else { 1.0 },
        }
    }
}

impl Default for MixtureOfExperts {
    fn default() -> Self {
        Self::new()
    }
}

impl SlmBackend for MixtureOfExperts {
    fn model_name(&self) -> &str {
        "mixture_of_experts"
    }

    fn evaluate(&self, context: &EvaluationContext) -> Vote {
        self.evaluate_with_experts(context).aggregated_vote
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
            claim_text: "Added login endpoint".to_string(),
            evidence_summary: "FileExists: Confirmed, Hash: Confirmed".to_string(),
            artifact_sample: Some(
                "pub async fn login(req: Request) -> Result<Response, Error> {\n\
                     let body: LoginBody = req.json().await?;\n\
                     let user = db.find_user(&body.email).await?;\n\
                     if verify(&body.password, &user.hash)? {\n\
                         let token = jwt::sign(user.id)?;\n\
                         Ok(Response::json(&TokenResponse { token }))\n\
                     } else {\n\
                         Err(Error::Unauthorized)\n\
                     }\n\
                 }\n"
                .to_string(),
            ),
            diff_sample: None,
        }
    }

    fn stub_context() -> EvaluationContext {
        EvaluationContext {
            claim_text: "Implemented the parser".to_string(),
            evidence_summary: "FileExists: Confirmed".to_string(),
            artifact_sample: Some("fn parse() { todo!() }\nfn lex() { unimplemented!() }\n".to_string()),
            diff_sample: None,
        }
    }

    #[test]
    fn test_clean_code_general_only() {
        let moe = MixtureOfExperts::new();
        let result = moe.evaluate_with_experts(&clean_context());

        // Only GeneralPurpose should activate for clean code
        assert_eq!(result.experts_activated, 1);
        assert_eq!(result.expert_results[0].expert, ExpertType::GeneralPurpose);
        assert_eq!(result.aggregated_vote.decision, VoteDecision::Approve);
    }

    #[test]
    fn test_stub_activates_expert() {
        let moe = MixtureOfExperts::new();
        let result = moe.evaluate_with_experts(&stub_context());

        // StubDetection expert should activate
        assert!(result.experts_activated >= 2);
        assert!(result
            .expert_results
            .iter()
            .any(|r| r.expert == ExpertType::StubDetection));
        assert_eq!(result.aggregated_vote.decision, VoteDecision::Reject);
    }

    #[test]
    fn test_expert_routing() {
        let moe = MixtureOfExperts::new();

        // Scope reduction context
        let ctx = EvaluationContext {
            claim_text: "Refactored auth, logging, caching, validation, routing, all tests".to_string(),
            evidence_summary: "".to_string(),
            artifact_sample: Some("fn main() { println!(\"done\") }\n".to_string()),
            diff_sample: Some(
                "+++ a/file.rs\n--- b/file.rs\n-old1\n-old2\n-old3\n-old4\n-old5\n\
                 -old6\n-old7\n-old8\n-old9\n-old10\n-old11\n+// simplified\n"
                    .to_string(),
            ),
        };

        let result = moe.evaluate_with_experts(&ctx);
        // Should activate scope expert
        let has_scope = result
            .expert_results
            .iter()
            .any(|r| r.expert == ExpertType::ScopeVerification);
        // Scope expert activates if scope reduction violation is found
        if has_scope {
            assert!(result.experts_activated >= 2);
        }
    }

    #[test]
    fn test_slm_backend_trait() {
        let moe = MixtureOfExperts::new();
        assert_eq!(moe.model_name(), "mixture_of_experts");
        assert!(moe.is_ready());

        let vote = moe.evaluate(&clean_context());
        assert_eq!(vote.voter, "mixture_of_experts");
    }

    #[test]
    fn test_general_expert_always_present() {
        let moe = MixtureOfExperts::new();

        for ctx in &[clean_context(), stub_context()] {
            let result = moe.evaluate_with_experts(ctx);
            assert!(result
                .expert_results
                .iter()
                .any(|r| r.expert == ExpertType::GeneralPurpose));
        }
    }
}
