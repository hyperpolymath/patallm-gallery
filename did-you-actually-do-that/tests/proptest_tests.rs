// SPDX-License-Identifier: MPL-2.0
//! Property-based tests using proptest

use did_you_actually_do_that::{Claim, EvidenceSpec, Verdict, Verifier};
use proptest::prelude::*;

// Strategy for generating arbitrary Verdict values
fn arb_verdict() -> impl Strategy<Value = Verdict> {
    prop_oneof![
        Just(Verdict::Confirmed),
        Just(Verdict::Refuted),
        Just(Verdict::Inconclusive),
        Just(Verdict::Unverifiable),
    ]
}

// Strategy for generating valid file paths (no null bytes, reasonable length)
fn arb_path() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9_/.-]{1,100}".prop_map(|s| format!("/tmp/test/{}", s))
}

// Strategy for generating EvidenceSpec
fn arb_evidence_spec() -> impl Strategy<Value = EvidenceSpec> {
    prop_oneof![
        arb_path().prop_map(|path| EvidenceSpec::FileExists { path }),
        (arb_path(), "[a-f0-9]{64}")
            .prop_map(|(path, sha256)| EvidenceSpec::FileWithHash { path, sha256 }),
        (arb_path(), ".*{1,50}")
            .prop_map(|(path, substring)| EvidenceSpec::FileContains { path, substring }),
        arb_path().prop_map(|path| EvidenceSpec::DirectoryExists { path }),
    ]
}

proptest! {
    /// Verdict serialization roundtrips correctly
    #[test]
    fn verdict_serde_roundtrip(verdict in arb_verdict()) {
        let json = serde_json::to_string(&verdict).unwrap();
        let parsed: Verdict = serde_json::from_str(&json).unwrap();
        prop_assert_eq!(verdict, parsed);
    }

    /// Only Confirmed verdicts are trustworthy
    #[test]
    fn only_confirmed_is_trustworthy(verdict in arb_verdict()) {
        let is_trustworthy = verdict.is_trustworthy();
        prop_assert_eq!(is_trustworthy, verdict == Verdict::Confirmed);
    }

    /// Claims always have non-empty IDs
    #[test]
    fn claims_have_ids(description in ".+") {
        let claim = Claim::new(description);
        prop_assert!(!claim.id.is_empty());
    }

    /// Claims preserve description
    #[test]
    fn claims_preserve_description(description in ".{1,200}") {
        let claim = Claim::new(description.clone());
        prop_assert_eq!(claim.description, description);
    }

    /// Claims preserve source
    #[test]
    fn claims_preserve_source(description in ".+", source in ".+") {
        let claim = Claim::new(description).with_source(source.clone());
        prop_assert_eq!(claim.source, Some(source));
    }

    /// Evidence can be added to claims
    #[test]
    fn claims_accumulate_evidence(
        description in ".+",
        evidence in prop::collection::vec(arb_evidence_spec(), 1..5)
    ) {
        let mut claim = Claim::new(description);
        for e in evidence.iter() {
            claim = claim.with_evidence(e.clone());
        }
        prop_assert_eq!(claim.evidence.len(), evidence.len());
    }

    /// EvidenceSpec serialization roundtrips correctly
    #[test]
    fn evidence_spec_serde_roundtrip(evidence in arb_evidence_spec()) {
        let json = serde_json::to_string(&evidence).unwrap();
        let parsed: EvidenceSpec = serde_json::from_str(&json).unwrap();
        // Compare JSON since EvidenceSpec doesn't derive PartialEq
        let json2 = serde_json::to_string(&parsed).unwrap();
        prop_assert_eq!(json, json2);
    }

    /// Claims with no evidence are unverifiable
    #[test]
    fn empty_claims_unverifiable(description in ".+") {
        let claim = Claim::new(description);
        let verifier = Verifier::new();
        let report = verifier.verify(&claim);
        prop_assert_eq!(report.overall_verdict, Verdict::Unverifiable);
    }

    /// Verification reports preserve claim
    #[test]
    fn reports_preserve_claim(description in ".{1,100}") {
        let claim = Claim::new(description.clone());
        let verifier = Verifier::new();
        let report = verifier.verify(&claim);
        prop_assert_eq!(report.claim.description, description);
    }
}

#[cfg(test)]
mod deterministic_tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn file_exists_confirmed_when_file_present() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "content").unwrap();

        let claim = Claim::new("File should exist").with_evidence(EvidenceSpec::FileExists {
            path: file_path.to_string_lossy().to_string(),
        });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Confirmed);
    }

    #[test]
    fn file_exists_refuted_when_file_missing() {
        let claim = Claim::new("File should exist").with_evidence(EvidenceSpec::FileExists {
            path: "/nonexistent/path/file.txt".to_string(),
        });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Refuted);
    }

    #[test]
    fn file_contains_confirmed_when_substring_present() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "hello world").unwrap();

        let claim = Claim::new("File contains hello").with_evidence(EvidenceSpec::FileContains {
            path: file_path.to_string_lossy().to_string(),
            substring: "hello".to_string(),
        });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Confirmed);
    }

    #[test]
    fn file_contains_refuted_when_substring_missing() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "hello world").unwrap();

        let claim = Claim::new("File contains goodbye").with_evidence(EvidenceSpec::FileContains {
            path: file_path.to_string_lossy().to_string(),
            substring: "goodbye".to_string(),
        });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Refuted);
    }

    #[test]
    fn file_hash_confirmed_when_hash_matches() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "test content").unwrap();

        // SHA-256 of "test content"
        let expected_hash = "6ae8a75555209fd6c44157c0aed8016e763ff435a19cf186f76863140143ff72";

        let claim = Claim::new("File has correct hash").with_evidence(EvidenceSpec::FileWithHash {
            path: file_path.to_string_lossy().to_string(),
            sha256: expected_hash.to_string(),
        });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Confirmed);
    }

    #[test]
    fn directory_exists_confirmed() {
        let dir = tempdir().unwrap();

        let claim = Claim::new("Directory exists").with_evidence(EvidenceSpec::DirectoryExists {
            path: dir.path().to_string_lossy().to_string(),
        });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Confirmed);
    }

    #[test]
    fn multiple_evidence_all_must_pass() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "hello").unwrap();

        let claim = Claim::new("Multiple checks")
            .with_evidence(EvidenceSpec::FileExists {
                path: file_path.to_string_lossy().to_string(),
            })
            .with_evidence(EvidenceSpec::FileContains {
                path: file_path.to_string_lossy().to_string(),
                substring: "hello".to_string(),
            })
            .with_evidence(EvidenceSpec::DirectoryExists {
                path: dir.path().to_string_lossy().to_string(),
            });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Confirmed);
        assert_eq!(report.evidence_results.len(), 3);
    }

    #[test]
    fn one_failure_refutes_all() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "hello").unwrap();

        let claim = Claim::new("One fails")
            .with_evidence(EvidenceSpec::FileExists {
                path: file_path.to_string_lossy().to_string(),
            })
            .with_evidence(EvidenceSpec::FileContains {
                path: file_path.to_string_lossy().to_string(),
                substring: "nonexistent".to_string(),
            });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Refuted);
    }

    #[test]
    fn command_succeeds_with_true() {
        let claim = Claim::new("True succeeds").with_evidence(EvidenceSpec::CommandSucceeds {
            command: "true".to_string(),
            args: vec![],
        });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Confirmed);
    }

    #[test]
    fn command_fails_with_false() {
        let claim = Claim::new("False fails").with_evidence(EvidenceSpec::CommandSucceeds {
            command: "false".to_string(),
            args: vec![],
        });

        let verifier = Verifier::new();
        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Refuted);
    }

    #[test]
    fn custom_checker_works() {
        use std::collections::HashMap;

        let mut verifier = Verifier::new();
        verifier.register_checker("always_pass", |_params| Ok(Verdict::Confirmed));

        let claim = Claim::new("Custom check").with_evidence(EvidenceSpec::Custom {
            name: "always_pass".to_string(),
            params: HashMap::new(),
        });

        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Confirmed);
    }

    #[test]
    fn unknown_custom_checker_is_unverifiable() {
        use std::collections::HashMap;

        let verifier = Verifier::new();

        let claim = Claim::new("Unknown checker").with_evidence(EvidenceSpec::Custom {
            name: "nonexistent".to_string(),
            params: HashMap::new(),
        });

        let report = verifier.verify(&claim);

        assert_eq!(report.overall_verdict, Verdict::Unverifiable);
    }
}
