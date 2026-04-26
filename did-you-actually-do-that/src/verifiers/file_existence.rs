// SPDX-License-Identifier: PMPL-1.0-or-later
//! Layer 1: File Existence
//!
//! Checks that claimed files and directories actually exist on disk.

use crate::{EvidenceResult, EvidenceSpec, Verdict};
use std::path::Path;

/// Check file/directory existence evidence.
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    let (verdict, details) = match evidence {
        EvidenceSpec::FileExists { path } => {
            if Path::new(path).exists() {
                (Verdict::Confirmed, Some(format!("File exists: {}", path)))
            } else {
                (Verdict::Refuted, Some(format!("File not found: {}", path)))
            }
        }

        EvidenceSpec::DirectoryExists { path } => {
            let p = Path::new(path);
            if p.exists() && p.is_dir() {
                (
                    Verdict::Confirmed,
                    Some(format!("Directory exists: {}", path)),
                )
            } else {
                (
                    Verdict::Refuted,
                    Some(format!("Directory not found: {}", path)),
                )
            }
        }

        _ => (
            Verdict::Unverifiable,
            Some("Not a file existence check".to_string()),
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
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_file_exists_confirmed() {
        let dir = TempDir::new().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "hello").unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: file_path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_file_exists_refuted() {
        let evidence = EvidenceSpec::FileExists {
            path: "/nonexistent/file/path/abc123.txt".to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
    }

    #[test]
    fn test_directory_exists_confirmed() {
        let dir = TempDir::new().unwrap();
        let evidence = EvidenceSpec::DirectoryExists {
            path: dir.path().to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_directory_exists_refuted() {
        let evidence = EvidenceSpec::DirectoryExists {
            path: "/nonexistent/directory/abc123".to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
    }
}
