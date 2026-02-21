// SPDX-License-Identifier: PMPL-1.0-or-later
//! Tier 1: Policy Oracle — deterministic rule checking.
//!
//! Fast, no ML inference needed. Checks for known violation patterns
//! against the 12 violation categories from cognitive-gating.
//! Rules are eventually driven by Nickel configuration files.

use super::{
    EvaluationContext, PolicyViolation, Vote, VoteDecision, ViolationCategory,
};

/// The Policy Oracle evaluates claims against deterministic rules.
///
/// This is Tier 1 of the three-tier cognitive-gating model.
/// It runs before any SLM inference and catches obvious violations quickly.
pub struct PolicyOracle {
    /// Patterns that indicate stub/placeholder code
    stub_patterns: Vec<&'static str>,
    /// Patterns that indicate incomplete work
    incomplete_patterns: Vec<&'static str>,
    /// Minimum artifact size (bytes) to not flag as suspiciously small
    min_artifact_size: usize,
}

impl Default for PolicyOracle {
    fn default() -> Self {
        Self::new()
    }
}

impl PolicyOracle {
    pub fn new() -> Self {
        Self {
            stub_patterns: vec![
                "todo!()",
                "unimplemented!()",
                "// TODO",
                "# TODO",
                "pass  # placeholder",
                "raise NotImplementedError",
                "throw new Error(\"not implemented\")",
                "fn stub()",
                "panic!(\"not yet\")",
                "todo()",
            ],
            incomplete_patterns: vec![
                "// This would be approximately",
                "// Estimated ~",
                "# approximately",
                "# estimated ~",
                "// placeholder for",
                "// stub for",
            ],
            min_artifact_size: 50,
        }
    }

    /// Evaluate a claim context and return any violations found.
    pub fn check(&self, context: &EvaluationContext) -> Vec<PolicyViolation> {
        let mut violations = Vec::new();

        // Check artifact for stub patterns
        if let Some(ref artifact) = context.artifact_sample {
            self.check_stubs(artifact, &mut violations);
            self.check_size_estimation(artifact, &mut violations);
            self.check_artifact_size(artifact, &mut violations);
        }

        // Check diff for scope reduction indicators
        if let Some(ref diff) = context.diff_sample {
            self.check_scope_reduction(diff, context, &mut violations);
        }

        // Check claim text for overconfident assertions
        self.check_false_confidence(&context.claim_text, &mut violations);

        violations
    }

    /// Produce a vote based on policy violations.
    pub fn evaluate(&self, context: &EvaluationContext) -> Vote {
        let violations = self.check(context);

        if violations.is_empty() {
            Vote {
                voter: "policy_oracle".to_string(),
                decision: VoteDecision::Approve,
                confidence: 0.7,
                reasoning: "No policy violations detected".to_string(),
                weight: 1.0,
            }
        } else {
            let max_severity = violations
                .iter()
                .map(|v| v.severity)
                .fold(0.0f64, f64::max);

            let descriptions: Vec<String> = violations
                .iter()
                .map(|v| format!("{}: {}", v.category.name(), v.description))
                .collect();

            Vote {
                voter: "policy_oracle".to_string(),
                decision: VoteDecision::Reject,
                confidence: max_severity.min(1.0),
                reasoning: descriptions.join("; "),
                // Rejections get 1.5x weight (cognitive-gating asymmetric weighting)
                weight: 1.5,
            }
        }
    }

    fn check_stubs(&self, artifact: &str, violations: &mut Vec<PolicyViolation>) {
        for pattern in &self.stub_patterns {
            if artifact.contains(pattern) {
                violations.push(PolicyViolation {
                    category: ViolationCategory::StubPlaceholderLeftInPlace,
                    description: format!("Found stub pattern: {}", pattern),
                    severity: 0.9,
                });
            }
        }
    }

    fn check_size_estimation(&self, artifact: &str, violations: &mut Vec<PolicyViolation>) {
        for pattern in &self.incomplete_patterns {
            if artifact.contains(pattern) {
                violations.push(PolicyViolation {
                    category: ViolationCategory::IncompleteImplementation,
                    description: format!("Found size estimation instead of code: {}", pattern),
                    severity: 0.95,
                });
            }
        }
    }

    fn check_artifact_size(&self, artifact: &str, violations: &mut Vec<PolicyViolation>) {
        if artifact.trim().len() < self.min_artifact_size {
            violations.push(PolicyViolation {
                category: ViolationCategory::IncompleteImplementation,
                description: format!(
                    "Artifact suspiciously small ({} bytes, minimum {})",
                    artifact.trim().len(),
                    self.min_artifact_size
                ),
                severity: 0.6,
            });
        }
    }

    fn check_scope_reduction(
        &self,
        diff: &str,
        context: &EvaluationContext,
        violations: &mut Vec<PolicyViolation>,
    ) {
        // Count additions vs deletions in diff
        let additions = diff.lines().filter(|l| l.starts_with('+')).count();
        let deletions = diff.lines().filter(|l| l.starts_with('-')).count();

        // If there are far more deletions than additions, scope may have been reduced
        if deletions > additions * 3 && deletions > 10 {
            violations.push(PolicyViolation {
                category: ViolationCategory::ScopeSilentlyReduced,
                description: format!(
                    "Diff shows {} deletions vs {} additions — possible scope reduction",
                    deletions, additions
                ),
                severity: 0.5,
            });
        }

        // Check if claim mentions more items than diff touches
        let claim_items = context
            .claim_text
            .split(&[',', ';', '\n'][..])
            .filter(|s| !s.trim().is_empty())
            .count();
        let diff_files = diff
            .lines()
            .filter(|l| l.starts_with("+++ ") || l.starts_with("--- "))
            .count()
            / 2;

        if claim_items > 3 && diff_files > 0 && claim_items > diff_files * 3 {
            violations.push(PolicyViolation {
                category: ViolationCategory::ScopeSilentlyReduced,
                description: format!(
                    "Claim mentions ~{} items but diff only touches {} files",
                    claim_items, diff_files
                ),
                severity: 0.7,
            });
        }
    }

    fn check_false_confidence(&self, claim: &str, violations: &mut Vec<PolicyViolation>) {
        let overconfident_phrases = [
            "100% complete",
            "fully implemented",
            "all tests pass",
            "everything works",
            "completely done",
            "nothing left to do",
        ];

        for phrase in &overconfident_phrases {
            if claim.to_lowercase().contains(phrase) {
                violations.push(PolicyViolation {
                    category: ViolationCategory::IncompleteImplementation,
                    description: format!(
                        "Overconfident assertion '{}' — requires evidence",
                        phrase
                    ),
                    severity: 0.4,
                });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_context(claim: &str, artifact: Option<&str>, diff: Option<&str>) -> EvaluationContext {
        EvaluationContext {
            claim_text: claim.to_string(),
            evidence_summary: String::new(),
            artifact_sample: artifact.map(|s| s.to_string()),
            diff_sample: diff.map(|s| s.to_string()),
        }
    }

    #[test]
    fn test_clean_artifact_passes() {
        let oracle = PolicyOracle::new();
        let ctx = make_context(
            "Added authentication module",
            Some("pub fn authenticate(user: &str, pass: &str) -> Result<Token, AuthError> {\n    let hash = hash_password(pass);\n    db.verify(user, &hash)\n}\n"),
            None,
        );
        let violations = oracle.check(&ctx);
        assert!(violations.is_empty());
    }

    #[test]
    fn test_stub_detected() {
        let oracle = PolicyOracle::new();
        let ctx = make_context(
            "Implemented parser",
            Some("pub fn parse(input: &str) -> AST {\n    todo!()\n}\n"),
            None,
        );
        let violations = oracle.check(&ctx);
        assert!(!violations.is_empty());
        assert!(violations
            .iter()
            .any(|v| v.category == ViolationCategory::StubPlaceholderLeftInPlace));
    }

    #[test]
    fn test_size_estimation_detected() {
        let oracle = PolicyOracle::new();
        let ctx = make_context(
            "Built the compiler backend",
            Some("// This would be approximately 500 lines of codegen\n// Estimated ~200 lines for register allocation\n"),
            None,
        );
        let violations = oracle.check(&ctx);
        assert!(violations
            .iter()
            .any(|v| v.category == ViolationCategory::IncompleteImplementation));
    }

    #[test]
    fn test_overconfident_claim() {
        let oracle = PolicyOracle::new();
        let ctx = make_context(
            "100% complete, everything works perfectly",
            Some("fn main() { println!(\"hello\"); } // placeholder for real implementation\n"),
            None,
        );
        let violations = oracle.check(&ctx);
        assert!(violations.len() >= 1);
    }

    #[test]
    fn test_scope_reduction_detected() {
        let oracle = PolicyOracle::new();
        let diff = "+++ a/file.rs\n--- b/file.rs\n-line1\n-line2\n-line3\n-line4\n-line5\n-line6\n-line7\n-line8\n-line9\n-line10\n-line11\n+// simplified\n";
        let ctx = make_context("Refactored authentication, logging, caching, validation, routing", None, Some(diff));
        let violations = oracle.check(&ctx);
        assert!(violations
            .iter()
            .any(|v| v.category == ViolationCategory::ScopeSilentlyReduced));
    }

    #[test]
    fn test_vote_approve_on_clean() {
        let oracle = PolicyOracle::new();
        let ctx = make_context(
            "Added user login",
            Some("pub async fn login(req: LoginRequest) -> Result<Session, Error> {\n    let user = db.find_user(&req.email).await?;\n    verify_password(&req.password, &user.hash)?;\n    Ok(Session::new(user.id))\n}\n"),
            None,
        );
        let vote = oracle.evaluate(&ctx);
        assert_eq!(vote.decision, VoteDecision::Approve);
        assert_eq!(vote.weight, 1.0);
    }

    #[test]
    fn test_vote_reject_on_violations() {
        let oracle = PolicyOracle::new();
        let ctx = make_context(
            "Fully implemented",
            Some("fn handle() { todo!() }\n"),
            None,
        );
        let vote = oracle.evaluate(&ctx);
        assert_eq!(vote.decision, VoteDecision::Reject);
        assert_eq!(vote.weight, 1.5); // Asymmetric weighting
    }
}
