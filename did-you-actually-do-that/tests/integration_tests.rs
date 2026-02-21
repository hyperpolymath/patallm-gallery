// SPDX-License-Identifier: MPL-2.0
//! Integration tests for the CLI and library working together

use std::fs;
use std::process::Command;
use tempfile::tempdir;

#[test]
fn cli_verify_existing_file() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("exists.txt");
    fs::write(&file_path, "content").unwrap();

    let output = Command::new("cargo")
        .args(["run", "--", "verify", &file_path.to_string_lossy()])
        .output()
        .expect("Failed to run dyadt");

    assert!(
        output.status.success(),
        "Expected exit code 0 for existing file"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Confirmed"), "Expected Confirmed in output");
}

#[test]
fn cli_verify_missing_file() {
    let output = Command::new("cargo")
        .args(["run", "--", "verify", "/nonexistent/path/file.txt"])
        .output()
        .expect("Failed to run dyadt");

    assert_eq!(
        output.status.code(),
        Some(1),
        "Expected exit code 1 for missing file"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Refuted"), "Expected Refuted in output");
}

#[test]
fn cli_hash_file() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("hashme.txt");
    fs::write(&file_path, "test content").unwrap();

    let output = Command::new("cargo")
        .args(["run", "--", "hash", &file_path.to_string_lossy()])
        .output()
        .expect("Failed to run dyadt");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // SHA-256 of "test content"
    assert!(stdout.contains("6ae8a75555209fd6c44157c0aed8016e763ff435a19cf186f76863140143ff72"));
    assert!(stdout.contains("FileWithHash"));
}

#[test]
fn cli_check_claim_file() {
    let dir = tempdir().unwrap();

    // Create a file to verify
    let target_file = dir.path().join("target.txt");
    fs::write(&target_file, "hello world").unwrap();

    // Create a claim file
    let claim_file = dir.path().join("claim.json");
    let claim_json = format!(
        r#"{{
        "description": "Test claim",
        "evidence": [
            {{ "type": "FileExists", "spec": {{ "path": "{}" }} }},
            {{ "type": "FileContains", "spec": {{ "path": "{}", "substring": "hello" }} }}
        ],
        "source": "integration-test"
    }}"#,
        target_file.to_string_lossy().replace('\\', "\\\\"),
        target_file.to_string_lossy().replace('\\', "\\\\")
    );
    fs::write(&claim_file, claim_json).unwrap();

    let output = Command::new("cargo")
        .args(["run", "--", "check", &claim_file.to_string_lossy()])
        .output()
        .expect("Failed to run dyadt");

    assert!(output.status.success(), "Expected exit code 0");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Confirmed"));
    assert!(stdout.contains("integration-test"));
}

#[test]
fn cli_check_failing_claim() {
    let dir = tempdir().unwrap();

    // Create a claim that will fail
    let claim_file = dir.path().join("bad-claim.json");
    let claim_json = r#"{
        "description": "This will fail",
        "evidence": [
            { "type": "FileExists", "spec": { "path": "/definitely/not/real/file.txt" } }
        ]
    }"#;
    fs::write(&claim_file, claim_json).unwrap();

    let output = Command::new("cargo")
        .args(["run", "--", "check", &claim_file.to_string_lossy()])
        .output()
        .expect("Failed to run dyadt");

    assert_eq!(
        output.status.code(),
        Some(1),
        "Expected exit code 1 for refuted claim"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Refuted"));
}

#[test]
fn cli_report_multiple_claims() {
    let dir = tempdir().unwrap();

    // Create files
    let file1 = dir.path().join("file1.txt");
    let file2 = dir.path().join("file2.txt");
    fs::write(&file1, "content1").unwrap();
    fs::write(&file2, "content2").unwrap();

    // Create claims file with array
    let claims_file = dir.path().join("claims.json");
    let claims_json = format!(
        r#"[
        {{
            "description": "First claim",
            "evidence": [{{ "type": "FileExists", "spec": {{ "path": "{}" }} }}]
        }},
        {{
            "description": "Second claim",
            "evidence": [{{ "type": "FileExists", "spec": {{ "path": "{}" }} }}]
        }}
    ]"#,
        file1.to_string_lossy().replace('\\', "\\\\"),
        file2.to_string_lossy().replace('\\', "\\\\")
    );
    fs::write(&claims_file, claims_json).unwrap();

    let output = Command::new("cargo")
        .args(["run", "--", "report", &claims_file.to_string_lossy()])
        .output()
        .expect("Failed to run dyadt");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("First claim"));
    assert!(stdout.contains("Second claim"));
    assert!(stdout.contains("Overall: Confirmed"));
}

#[test]
fn cli_report_mixed_results() {
    let dir = tempdir().unwrap();

    let file1 = dir.path().join("exists.txt");
    fs::write(&file1, "content").unwrap();

    let claims_file = dir.path().join("mixed.json");
    let claims_json = format!(
        r#"[
        {{
            "description": "This passes",
            "evidence": [{{ "type": "FileExists", "spec": {{ "path": "{}" }} }}]
        }},
        {{
            "description": "This fails",
            "evidence": [{{ "type": "FileExists", "spec": {{ "path": "/no/such/file" }} }}]
        }}
    ]"#,
        file1.to_string_lossy().replace('\\', "\\\\")
    );
    fs::write(&claims_file, claims_json).unwrap();

    let output = Command::new("cargo")
        .args(["run", "--", "report", &claims_file.to_string_lossy()])
        .output()
        .expect("Failed to run dyadt");

    assert_eq!(
        output.status.code(),
        Some(1),
        "Expected exit code 1 when any claim refuted"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Overall: Refuted"));
}

#[test]
fn cli_help_shows_usage() {
    let output = Command::new("cargo")
        .args(["run", "--", "help"])
        .output()
        .expect("Failed to run dyadt");

    assert!(output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Did You Actually Do That?"));
    assert!(stderr.contains("USAGE"));
    assert!(stderr.contains("COMMANDS"));
}

#[test]
fn cli_invalid_json_returns_error() {
    let dir = tempdir().unwrap();
    let bad_file = dir.path().join("bad.json");
    fs::write(&bad_file, "not valid json {{{").unwrap();

    let output = Command::new("cargo")
        .args(["run", "--", "check", &bad_file.to_string_lossy()])
        .output()
        .expect("Failed to run dyadt");

    assert_eq!(
        output.status.code(),
        Some(3),
        "Expected exit code 3 for parse error"
    );
}

#[test]
fn cli_missing_file_returns_error() {
    let output = Command::new("cargo")
        .args(["run", "--", "check", "/no/such/claim/file.json"])
        .output()
        .expect("Failed to run dyadt");

    assert_eq!(
        output.status.code(),
        Some(3),
        "Expected exit code 3 for missing file"
    );
}

#[test]
fn self_verification_passes() {
    // The repo's own example-claim.json should pass when run from repo root
    let output = Command::new("cargo")
        .args(["run", "--", "check", "example-claim.json"])
        .output()
        .expect("Failed to run dyadt");

    assert!(output.status.success(), "Self-verification should pass");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Confirmed"));
}
