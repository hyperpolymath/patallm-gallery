// SPDX-License-Identifier: MPL-2.0
//! Benchmarks for verification operations

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use did_you_actually_do_that::{Claim, EvidenceSpec, Verifier};
use std::fs;
use tempfile::tempdir;

/// Benchmark single file existence check
fn bench_file_exists(c: &mut Criterion) {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("test.txt");
    fs::write(&file_path, "test content").unwrap();

    let verifier = Verifier::new();
    let claim = Claim::new("File exists").with_evidence(EvidenceSpec::FileExists {
        path: file_path.to_string_lossy().to_string(),
    });

    c.bench_function("file_exists", |b| {
        b.iter(|| verifier.verify(black_box(&claim)))
    });
}

/// Benchmark file hash verification
fn bench_file_hash(c: &mut Criterion) {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("test.txt");
    fs::write(&file_path, "test content").unwrap();

    // SHA-256 of "test content"
    let hash = "6ae8a75555209fd6c44157c0aed8016e763ff435a19cf186f76863140143ff72";

    let verifier = Verifier::new();
    let claim = Claim::new("File hash matches").with_evidence(EvidenceSpec::FileWithHash {
        path: file_path.to_string_lossy().to_string(),
        sha256: hash.to_string(),
    });

    c.bench_function("file_hash", |b| {
        b.iter(|| verifier.verify(black_box(&claim)))
    });
}

/// Benchmark file contains substring
fn bench_file_contains(c: &mut Criterion) {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("test.txt");
    fs::write(&file_path, "hello world this is a test").unwrap();

    let verifier = Verifier::new();
    let claim = Claim::new("File contains text").with_evidence(EvidenceSpec::FileContains {
        path: file_path.to_string_lossy().to_string(),
        substring: "world".to_string(),
    });

    c.bench_function("file_contains", |b| {
        b.iter(|| verifier.verify(black_box(&claim)))
    });
}

/// Benchmark regex matching
fn bench_file_regex(c: &mut Criterion) {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("test.txt");
    fs::write(&file_path, "hello world 12345").unwrap();

    let verifier = Verifier::new();
    let claim = Claim::new("File matches regex").with_evidence(EvidenceSpec::FileMatchesRegex {
        path: file_path.to_string_lossy().to_string(),
        pattern: r"\d{5}".to_string(),
    });

    c.bench_function("file_regex", |b| {
        b.iter(|| verifier.verify(black_box(&claim)))
    });
}

/// Benchmark JSON path extraction
fn bench_json_path(c: &mut Criterion) {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("test.json");
    fs::write(&file_path, r#"{"name": "test", "nested": {"value": 42}}"#).unwrap();

    let verifier = Verifier::new();
    let claim = Claim::new("JSON path matches").with_evidence(EvidenceSpec::FileJsonPath {
        path: file_path.to_string_lossy().to_string(),
        json_path: ".nested.value".to_string(),
        expected: serde_json::json!(42),
    });

    c.bench_function("json_path", |b| {
        b.iter(|| verifier.verify(black_box(&claim)))
    });
}

/// Benchmark directory existence check
fn bench_directory_exists(c: &mut Criterion) {
    let dir = tempdir().unwrap();

    let verifier = Verifier::new();
    let claim = Claim::new("Directory exists").with_evidence(EvidenceSpec::DirectoryExists {
        path: dir.path().to_string_lossy().to_string(),
    });

    c.bench_function("directory_exists", |b| {
        b.iter(|| verifier.verify(black_box(&claim)))
    });
}

/// Benchmark command execution
fn bench_command_succeeds(c: &mut Criterion) {
    let verifier = Verifier::new();
    let claim = Claim::new("Command succeeds").with_evidence(EvidenceSpec::CommandSucceeds {
        command: "true".to_string(),
        args: vec![],
    });

    c.bench_function("command_succeeds", |b| {
        b.iter(|| verifier.verify(black_box(&claim)))
    });
}

/// Benchmark multiple evidence verification
fn bench_multiple_evidence(c: &mut Criterion) {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("test.txt");
    fs::write(&file_path, "test content").unwrap();

    let verifier = Verifier::new();

    let mut group = c.benchmark_group("multiple_evidence");
    for count in [1, 2, 5, 10].iter() {
        let mut claim = Claim::new(format!("{} evidence pieces", count));
        for _ in 0..*count {
            claim = claim.with_evidence(EvidenceSpec::FileExists {
                path: file_path.to_string_lossy().to_string(),
            });
        }

        group.bench_with_input(BenchmarkId::from_parameter(count), count, |b, _| {
            b.iter(|| verifier.verify(black_box(&claim)))
        });
    }
    group.finish();
}

/// Benchmark claim creation
fn bench_claim_creation(c: &mut Criterion) {
    c.bench_function("claim_creation", |b| {
        b.iter(|| {
            Claim::new(black_box("Test claim"))
                .with_evidence(EvidenceSpec::FileExists {
                    path: "/tmp/test.txt".to_string(),
                })
                .with_source("benchmark")
        })
    });
}

/// Benchmark verifier with custom checker
fn bench_custom_checker(c: &mut Criterion) {
    let mut verifier = Verifier::new();
    verifier.register_checker("always_confirm", |_| {
        Ok(did_you_actually_do_that::Verdict::Confirmed)
    });

    let claim = Claim::new("Custom check").with_evidence(EvidenceSpec::Custom {
        name: "always_confirm".to_string(),
        params: std::collections::HashMap::new(),
    });

    c.bench_function("custom_checker", |b| {
        b.iter(|| verifier.verify(black_box(&claim)))
    });
}

criterion_group!(
    benches,
    bench_file_exists,
    bench_file_hash,
    bench_file_contains,
    bench_file_regex,
    bench_json_path,
    bench_directory_exists,
    bench_command_succeeds,
    bench_multiple_evidence,
    bench_claim_creation,
    bench_custom_checker,
);
criterion_main!(benches);
