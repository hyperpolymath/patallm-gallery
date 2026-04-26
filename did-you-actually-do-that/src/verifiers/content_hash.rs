// SPDX-License-Identifier: PMPL-1.0-or-later
//! Layer 2: Content Hash
//!
//! Verifies that file contents match expected SHA-256 hashes.

use crate::{EvidenceResult, EvidenceSpec, Verdict};
use sha2::{Digest, Sha256};

/// Check content hash evidence.
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    let (verdict, details) = match evidence {
        EvidenceSpec::FileWithHash { path, sha256 } => match std::fs::read(path) {
            Ok(contents) => {
                let mut hasher = Sha256::new();
                hasher.update(&contents);
                let actual_hash = hex::encode(hasher.finalize());
                if actual_hash == *sha256 {
                    (Verdict::Confirmed, Some("Hash matches".to_string()))
                } else {
                    (
                        Verdict::Refuted,
                        Some(format!(
                            "Hash mismatch: expected {}, got {}",
                            sha256, actual_hash
                        )),
                    )
                }
            }
            Err(e) => (Verdict::Refuted, Some(format!("Cannot read file: {}", e))),
        },

        _ => (
            Verdict::Unverifiable,
            Some("Not a content hash check".to_string()),
        ),
    };

    EvidenceResult {
        spec: evidence.clone(),
        verdict,
        details,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sha2::{Digest, Sha256};
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_hash_matches() {
        let dir = TempDir::new().unwrap();
        let file_path = dir.path().join("test.txt");
        let content = b"hello world";
        fs::write(&file_path, content).unwrap();

        let mut hasher = Sha256::new();
        hasher.update(content);
        let hash = hex::encode(hasher.finalize());

        let evidence = EvidenceSpec::FileWithHash {
            path: file_path.to_str().unwrap().to_string(),
            sha256: hash,
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_hash_mismatch() {
        let dir = TempDir::new().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "hello world").unwrap();

        let evidence = EvidenceSpec::FileWithHash {
            path: file_path.to_str().unwrap().to_string(),
            sha256: "0000000000000000000000000000000000000000000000000000000000000000"
                .to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
    }
}
