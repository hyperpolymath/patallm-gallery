// SPDX-License-Identifier: PMPL-1.0-or-later
//! CLI for Did You Actually Do That?
//!
//! Usage:
//!   `dyadt check <claim.json>`     - Verify a claim from a JSON file
//!   `dyadt verify <path>`          - Quick check if a file/directory exists
//!   `dyadt report <claims.json>`   - Generate a verification report
//!   `dyadt serve`                  - Run as Erlang Port server (for Elixir brain)

use did_you_actually_do_that::hooks::{format_hook_output, parse_hook_input, HookHandler};
use did_you_actually_do_that::mcp_server::McpServer;
use did_you_actually_do_that::{Claim, EvidenceSpec, Verdict, VerificationReport, Verifier};
use sha2::{Digest, Sha256};
use std::env;
use std::fs;
use std::process::ExitCode;
#[cfg(feature = "watch")]
use std::io::Write;

/// Output format for reports
#[derive(Debug, Clone, Copy, PartialEq)]
enum OutputFormat {
    Human,
    Json,
    Sarif,
    JUnit,
}

fn print_help() {
    eprintln!(
        r#"
Did You Actually Do That? (dyadt) v0.1.0
A verification framework for validating claimed actions against reality.

USAGE:
    dyadt <COMMAND> [ARGS]

COMMANDS:
    check <claim.json>      Verify a claim from a JSON file
    verify <path>           Quick check if a file or directory exists
    hash <file>             Compute SHA-256 hash of a file (for evidence specs)
    report <claims.json>    Verify multiple claims and generate a report
    report --json <file>    Output report as JSON
    report --sarif <file>   Output report as SARIF (for code scanning)
    report --junit <file>   Output report as JUnit XML (for CI)
    watch <claims.json>     Watch and re-verify on file changes (requires 'watch' feature)
    mcp-server              Run as MCP server (for AI self-verification)
    serve                   Run as Erlang Port server (for Elixir brain)
    hook <json>             Process hook event from Claude Code
    help                    Show this help message

EXAMPLES:
    # Verify a specific claim
    dyadt check my-claim.json

    # Quick existence check
    dyadt verify /path/to/expected/file.txt

    # Get hash for evidence specification
    dyadt hash important-file.rs

    # Generate CI-friendly report
    dyadt report --junit claims.json > results.xml

EVIDENCE TYPES:
    FileExists, FileWithHash, FileContains, FileMatchesRegex, FileJsonPath,
    DirectoryExists, CommandSucceeds, GitClean, GitCommitExists, GitBranchExists,
    FileModifiedAfter, EnvVar, Custom

EXIT CODES:
    0 - All claims verified (Confirmed)
    1 - One or more claims refuted
    2 - Inconclusive or unverifiable
    3 - Error (invalid input, etc.)
"#
    );
}

fn verify_claim_file(path: &str) -> ExitCode {
    let contents = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading {}: {}", path, e);
            return ExitCode::from(3);
        }
    };

    let claim: Claim = match serde_json::from_str(&contents) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error parsing claim JSON: {}", e);
            return ExitCode::from(3);
        }
    };

    let verifier = Verifier::new();
    let report = verifier.verify(&claim);
    print_report(&report);

    verdict_to_exit_code(report.overall_verdict)
}

fn quick_verify(path: &str) -> ExitCode {
    let claim = Claim::new(format!("Path exists: {}", path))
        .with_evidence(EvidenceSpec::FileExists {
            path: path.to_string(),
        })
        .with_source("dyadt-cli");

    let verifier = Verifier::new();
    let report = verifier.verify(&claim);
    print_report(&report);

    verdict_to_exit_code(report.overall_verdict)
}

fn compute_hash(path: &str) -> ExitCode {
    match fs::read(path) {
        Ok(contents) => {
            let mut hasher = Sha256::new();
            hasher.update(&contents);
            let hash = hex::encode(hasher.finalize());
            println!("{}", hash);
            println!("\nEvidence spec:");
            println!(
                r#"{{ "type": "FileWithHash", "spec": {{ "path": "{}", "sha256": "{}" }} }}"#,
                path, hash
            );
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("Error reading {}: {}", path, e);
            ExitCode::from(3)
        }
    }
}

fn verify_multiple(path: &str) -> ExitCode {
    verify_multiple_with_format(path, OutputFormat::Human)
}

fn print_report(report: &VerificationReport) {
    println!("{}", report.summary());

    if let Some(ref source) = report.claim.source {
        println!("  Source: {}", source);
    }

    for result in &report.evidence_results {
        let icon = match result.verdict {
            Verdict::Confirmed => "  ✓",
            Verdict::Refuted => "  ✗",
            Verdict::Inconclusive => "  ?",
            Verdict::Unverifiable => "  ⊘",
        };

        let evidence_desc = match &result.spec {
            EvidenceSpec::FileExists { path } => format!("File exists: {}", path),
            EvidenceSpec::FileWithHash { path, .. } => format!("File hash: {}", path),
            EvidenceSpec::FileContains { path, substring } => {
                format!("File contains '{}': {}", substring, path)
            }
            EvidenceSpec::FileMatchesRegex { path, pattern } => {
                format!("File matches regex '{}': {}", pattern, path)
            }
            EvidenceSpec::FileJsonPath {
                path, json_path, ..
            } => {
                format!("JSON path '{}': {}", json_path, path)
            }
            EvidenceSpec::DirectoryExists { path } => format!("Directory exists: {}", path),
            EvidenceSpec::CommandSucceeds { command, .. } => {
                format!("Command succeeds: {}", command)
            }
            EvidenceSpec::GitClean { repo_path } => {
                format!("Git clean: {}", repo_path.as_deref().unwrap_or("."))
            }
            EvidenceSpec::GitCommitExists { commit, .. } => {
                format!("Git commit exists: {}", commit)
            }
            EvidenceSpec::GitBranchExists { branch, .. } => {
                format!("Git branch exists: {}", branch)
            }
            EvidenceSpec::FileModifiedAfter { path, after } => {
                format!("File modified after {}: {}", after, path)
            }
            EvidenceSpec::EnvVar { name, expected } => {
                format!("Env {}={}", name, expected)
            }
            EvidenceSpec::Custom { name, .. } => format!("Custom check: {}", name),
        };

        println!("{} {}", icon, evidence_desc);

        if let Some(ref details) = result.details {
            println!("      {}", details);
        }
    }
}

fn verdict_to_exit_code(verdict: Verdict) -> ExitCode {
    match verdict {
        Verdict::Confirmed => ExitCode::SUCCESS,
        Verdict::Refuted => ExitCode::from(1),
        Verdict::Inconclusive | Verdict::Unverifiable => ExitCode::from(2),
    }
}

/// Verify multiple claims with configurable output format
fn verify_multiple_with_format(path: &str, format: OutputFormat) -> ExitCode {
    let contents = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading {}: {}", path, e);
            return ExitCode::from(3);
        }
    };

    let claims: Vec<Claim> = match serde_json::from_str(&contents) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error parsing claims JSON: {}", e);
            return ExitCode::from(3);
        }
    };

    let verifier = Verifier::new();
    let reports: Vec<VerificationReport> = claims.iter().map(|c| verifier.verify(c)).collect();

    let worst_verdict = reports.iter().fold(Verdict::Confirmed, |acc, r| {
        match (acc, r.overall_verdict) {
            (_, Verdict::Refuted) | (Verdict::Refuted, _) => Verdict::Refuted,
            (_, Verdict::Inconclusive) | (Verdict::Inconclusive, _) => Verdict::Inconclusive,
            (_, Verdict::Unverifiable) | (Verdict::Unverifiable, _) => Verdict::Unverifiable,
            (Verdict::Confirmed, Verdict::Confirmed) => Verdict::Confirmed,
        }
    });

    match format {
        OutputFormat::Human => {
            println!("Verification Report");
            println!("===================\n");
            for report in &reports {
                print_report(report);
                println!();
            }
            println!("-------------------");
            println!("Overall: {:?}", worst_verdict);
        }
        OutputFormat::Json => {
            output_json(&reports);
        }
        OutputFormat::Sarif => {
            output_sarif(&reports);
        }
        OutputFormat::JUnit => {
            output_junit(&reports);
        }
    }

    verdict_to_exit_code(worst_verdict)
}

/// Output reports as JSON
fn output_json(reports: &[VerificationReport]) {
    let output = serde_json::json!({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "tool": {
            "name": "did-you-actually-do-that",
            "version": env!("CARGO_PKG_VERSION")
        },
        "reports": reports,
        "summary": {
            "total": reports.len(),
            "confirmed": reports.iter().filter(|r| r.overall_verdict == Verdict::Confirmed).count(),
            "refuted": reports.iter().filter(|r| r.overall_verdict == Verdict::Refuted).count(),
            "inconclusive": reports.iter().filter(|r| r.overall_verdict == Verdict::Inconclusive).count(),
            "unverifiable": reports.iter().filter(|r| r.overall_verdict == Verdict::Unverifiable).count()
        }
    });
    println!("{}", serde_json::to_string_pretty(&output).unwrap());
}

/// Output reports as SARIF (Static Analysis Results Interchange Format)
/// Useful for GitHub code scanning integration
fn output_sarif(reports: &[VerificationReport]) {
    let results: Vec<serde_json::Value> = reports
        .iter()
        .flat_map(|report| {
            report.evidence_results.iter().filter_map(|result| {
                // Only include refuted or inconclusive results as findings
                if result.verdict == Verdict::Confirmed {
                    return None;
                }

                let level = match result.verdict {
                    Verdict::Refuted => "error",
                    Verdict::Inconclusive => "warning",
                    Verdict::Unverifiable => "note",
                    Verdict::Confirmed => unreachable!(),
                };

                let rule_id = match &result.spec {
                    EvidenceSpec::FileExists { .. } => "file-exists",
                    EvidenceSpec::FileWithHash { .. } => "file-hash",
                    EvidenceSpec::FileContains { .. } => "file-contains",
                    EvidenceSpec::FileMatchesRegex { .. } => "file-regex",
                    EvidenceSpec::FileJsonPath { .. } => "file-json-path",
                    EvidenceSpec::DirectoryExists { .. } => "directory-exists",
                    EvidenceSpec::CommandSucceeds { .. } => "command-succeeds",
                    EvidenceSpec::GitClean { .. } => "git-clean",
                    EvidenceSpec::GitCommitExists { .. } => "git-commit-exists",
                    EvidenceSpec::GitBranchExists { .. } => "git-branch-exists",
                    EvidenceSpec::FileModifiedAfter { .. } => "file-modified-after",
                    EvidenceSpec::EnvVar { .. } => "env-var",
                    EvidenceSpec::Custom { .. } => "custom",
                };

                Some(serde_json::json!({
                    "ruleId": rule_id,
                    "level": level,
                    "message": {
                        "text": result.details.clone().unwrap_or_else(|| format!("{:?}", result.verdict))
                    },
                    "locations": [{
                        "physicalLocation": {
                            "artifactLocation": {
                                "uri": get_evidence_path(&result.spec).unwrap_or_default()
                            }
                        }
                    }],
                    "properties": {
                        "claim": report.claim.description.clone(),
                        "claimId": report.claim.id.clone()
                    }
                }))
            })
        })
        .collect();

    let sarif = serde_json::json!({
        "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
        "version": "2.1.0",
        "runs": [{
            "tool": {
                "driver": {
                    "name": "did-you-actually-do-that",
                    "version": env!("CARGO_PKG_VERSION"),
                    "informationUri": "https://gitlab.com/hyperpolymath/did-you-actually-do-that",
                    "rules": [
                        {"id": "file-exists", "shortDescription": {"text": "File existence check"}},
                        {"id": "file-hash", "shortDescription": {"text": "File hash verification"}},
                        {"id": "file-contains", "shortDescription": {"text": "File content check"}},
                        {"id": "file-regex", "shortDescription": {"text": "File regex match"}},
                        {"id": "file-json-path", "shortDescription": {"text": "JSON path verification"}},
                        {"id": "directory-exists", "shortDescription": {"text": "Directory existence check"}},
                        {"id": "command-succeeds", "shortDescription": {"text": "Command success verification"}},
                        {"id": "git-clean", "shortDescription": {"text": "Git working directory check"}},
                        {"id": "git-commit-exists", "shortDescription": {"text": "Git commit existence"}},
                        {"id": "git-branch-exists", "shortDescription": {"text": "Git branch existence"}},
                        {"id": "file-modified-after", "shortDescription": {"text": "File modification time check"}},
                        {"id": "env-var", "shortDescription": {"text": "Environment variable check"}},
                        {"id": "custom", "shortDescription": {"text": "Custom verification"}}
                    ]
                }
            },
            "results": results
        }]
    });

    println!("{}", serde_json::to_string_pretty(&sarif).unwrap());
}

/// Output reports as JUnit XML (for CI systems)
fn output_junit(reports: &[VerificationReport]) {
    let mut output = String::new();

    let total_tests: usize = reports.iter().map(|r| r.evidence_results.len()).sum();
    let failures: usize = reports
        .iter()
        .flat_map(|r| &r.evidence_results)
        .filter(|e| e.verdict == Verdict::Refuted)
        .count();
    let errors: usize = reports
        .iter()
        .flat_map(|r| &r.evidence_results)
        .filter(|e| e.verdict == Verdict::Unverifiable || e.verdict == Verdict::Inconclusive)
        .count();

    output.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    output.push_str(&format!(
        "<testsuites tests=\"{}\" failures=\"{}\" errors=\"{}\">\n",
        total_tests, failures, errors
    ));

    for report in reports {
        let suite_tests = report.evidence_results.len();
        let suite_failures = report
            .evidence_results
            .iter()
            .filter(|e| e.verdict == Verdict::Refuted)
            .count();
        let suite_errors = report
            .evidence_results
            .iter()
            .filter(|e| e.verdict == Verdict::Unverifiable || e.verdict == Verdict::Inconclusive)
            .count();

        output.push_str(&format!(
            "  <testsuite name=\"{}\" tests=\"{}\" failures=\"{}\" errors=\"{}\">\n",
            xml_escape(&report.claim.description),
            suite_tests,
            suite_failures,
            suite_errors
        ));

        for result in &report.evidence_results {
            let test_name = format_evidence_name(&result.spec);
            output.push_str(&format!(
                "    <testcase name=\"{}\" classname=\"{}\">\n",
                xml_escape(&test_name),
                xml_escape(&report.claim.id)
            ));

            match result.verdict {
                Verdict::Confirmed => {}
                Verdict::Refuted => {
                    output.push_str(&format!(
                        "      <failure message=\"{}\" type=\"Refuted\"/>\n",
                        xml_escape(result.details.as_deref().unwrap_or("Evidence refuted"))
                    ));
                }
                Verdict::Inconclusive | Verdict::Unverifiable => {
                    output.push_str(&format!(
                        "      <error message=\"{}\" type=\"{:?}\"/>\n",
                        xml_escape(result.details.as_deref().unwrap_or("Verification error")),
                        result.verdict
                    ));
                }
            }

            output.push_str("    </testcase>\n");
        }

        output.push_str("  </testsuite>\n");
    }

    output.push_str("</testsuites>\n");
    print!("{}", output);
}

/// Helper to escape XML special characters
fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// Helper to get path from evidence spec
fn get_evidence_path(spec: &EvidenceSpec) -> Option<String> {
    match spec {
        EvidenceSpec::FileExists { path }
        | EvidenceSpec::FileWithHash { path, .. }
        | EvidenceSpec::FileContains { path, .. }
        | EvidenceSpec::FileMatchesRegex { path, .. }
        | EvidenceSpec::FileJsonPath { path, .. }
        | EvidenceSpec::FileModifiedAfter { path, .. } => Some(path.clone()),
        EvidenceSpec::DirectoryExists { path } => Some(path.clone()),
        EvidenceSpec::GitClean { repo_path }
        | EvidenceSpec::GitCommitExists { repo_path, .. }
        | EvidenceSpec::GitBranchExists { repo_path, .. } => {
            repo_path.clone().or_else(|| Some(".".to_string()))
        }
        EvidenceSpec::CommandSucceeds { command, .. } => Some(command.clone()),
        EvidenceSpec::EnvVar { name, .. } => Some(format!("${}", name)),
        EvidenceSpec::Custom { name, .. } => Some(name.clone()),
    }
}

/// Helper to format evidence name for JUnit
fn format_evidence_name(spec: &EvidenceSpec) -> String {
    match spec {
        EvidenceSpec::FileExists { path } => format!("FileExists: {}", path),
        EvidenceSpec::FileWithHash { path, .. } => format!("FileWithHash: {}", path),
        EvidenceSpec::FileContains { path, substring } => {
            format!("FileContains '{}': {}", substring, path)
        }
        EvidenceSpec::FileMatchesRegex { path, pattern } => {
            format!("FileMatchesRegex '{}': {}", pattern, path)
        }
        EvidenceSpec::FileJsonPath {
            path, json_path, ..
        } => {
            format!("FileJsonPath '{}': {}", json_path, path)
        }
        EvidenceSpec::DirectoryExists { path } => format!("DirectoryExists: {}", path),
        EvidenceSpec::CommandSucceeds { command, .. } => format!("CommandSucceeds: {}", command),
        EvidenceSpec::GitClean { repo_path } => {
            format!("GitClean: {}", repo_path.as_deref().unwrap_or("."))
        }
        EvidenceSpec::GitCommitExists { commit, .. } => format!("GitCommitExists: {}", commit),
        EvidenceSpec::GitBranchExists { branch, .. } => format!("GitBranchExists: {}", branch),
        EvidenceSpec::FileModifiedAfter { path, after } => {
            format!("FileModifiedAfter {}: {}", after, path)
        }
        EvidenceSpec::EnvVar { name, expected } => format!("EnvVar {}={}", name, expected),
        EvidenceSpec::Custom { name, .. } => format!("Custom: {}", name),
    }
}

/// Watch mode for continuous verification
#[cfg(feature = "watch")]
fn watch_claims(path: &str) -> ExitCode {
    use did_you_actually_do_that::watch::watch_and_verify;

    let contents = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading {}: {}", path, e);
            return ExitCode::from(3);
        }
    };

    let claims: Vec<Claim> = match serde_json::from_str(&contents) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error parsing claims JSON: {}", e);
            return ExitCode::from(3);
        }
    };

    let on_report = |report: &VerificationReport| {
        print_report(report);
        let _ = std::io::stdout().flush();
    };

    match watch_and_verify(claims, on_report, 500) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Watch error: {}", e);
            ExitCode::from(3)
        }
    }
}

#[cfg(not(feature = "watch"))]
fn watch_claims(_path: &str) -> ExitCode {
    eprintln!("Watch mode requires the 'watch' feature. Rebuild with:");
    eprintln!("  cargo build --features watch");
    ExitCode::from(3)
}

/// Run the Erlang Port protocol server (for Elixir brain communication)
fn run_port_server() -> ExitCode {
    use did_you_actually_do_that::port_protocol;

    match port_protocol::serve() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Port server error: {}", e);
            ExitCode::from(3)
        }
    }
}

/// Run the MCP server
fn run_mcp_server() -> ExitCode {
    let server = McpServer::new();
    match server.run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("MCP server error: {}", e);
            ExitCode::from(3)
        }
    }
}

/// Process a hook event from Claude Code
fn run_hook(json_input: &str) -> ExitCode {
    let input = match parse_hook_input(json_input) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Error parsing hook input: {}", e);
            return ExitCode::from(3);
        }
    };

    let handler = HookHandler::new();
    let output = handler.handle(&input);

    // Output JSON result
    println!("{}", format_hook_output(&output));

    if output.success {
        ExitCode::SUCCESS
    } else {
        ExitCode::from(1)
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_help();
        return ExitCode::from(3);
    }

    match args[1].as_str() {
        "check" => {
            if args.len() < 3 {
                eprintln!("Usage: dyadt check <claim.json>");
                ExitCode::from(3)
            } else {
                verify_claim_file(&args[2])
            }
        }
        "verify" => {
            if args.len() < 3 {
                eprintln!("Usage: dyadt verify <path>");
                ExitCode::from(3)
            } else {
                quick_verify(&args[2])
            }
        }
        "hash" => {
            if args.len() < 3 {
                eprintln!("Usage: dyadt hash <file>");
                ExitCode::from(3)
            } else {
                compute_hash(&args[2])
            }
        }
        "report" => {
            if args.len() < 3 {
                eprintln!("Usage: dyadt report [--json|--sarif|--junit] <claims.json>");
                ExitCode::from(3)
            } else if args[2] == "--json" {
                if args.len() < 4 {
                    eprintln!("Usage: dyadt report --json <claims.json>");
                    ExitCode::from(3)
                } else {
                    verify_multiple_with_format(&args[3], OutputFormat::Json)
                }
            } else if args[2] == "--sarif" {
                if args.len() < 4 {
                    eprintln!("Usage: dyadt report --sarif <claims.json>");
                    ExitCode::from(3)
                } else {
                    verify_multiple_with_format(&args[3], OutputFormat::Sarif)
                }
            } else if args[2] == "--junit" {
                if args.len() < 4 {
                    eprintln!("Usage: dyadt report --junit <claims.json>");
                    ExitCode::from(3)
                } else {
                    verify_multiple_with_format(&args[3], OutputFormat::JUnit)
                }
            } else {
                verify_multiple(&args[2])
            }
        }
        "watch" => {
            if args.len() < 3 {
                eprintln!("Usage: dyadt watch <claims.json>");
                ExitCode::from(3)
            } else {
                watch_claims(&args[2])
            }
        }
        "mcp-server" => run_mcp_server(),
        "serve" => run_port_server(),
        "hook" => {
            if args.len() < 3 {
                eprintln!("Usage: dyadt hook <json>");
                ExitCode::from(3)
            } else {
                run_hook(&args[2])
            }
        }
        "help" | "--help" | "-h" => {
            print_help();
            ExitCode::SUCCESS
        }
        _ => {
            eprintln!("Unknown command: {}", args[1]);
            print_help();
            ExitCode::from(3)
        }
    }
}
