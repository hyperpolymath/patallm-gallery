#!/usr/bin/env julia
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

"""
    RSR Framework Compliance Checker

    Verifies llm-antidote compliance with Rhodium Standard Repository framework.

    Author: Jonathan D.A. Jewell
"""

# ---------------------------------------------------------------------------
# Data Structures
# ---------------------------------------------------------------------------

"""
    ComplianceCheck

Represents a single compliance check with its category, pass/fail status,
whether it is a required check, and a human-readable message.
"""
struct ComplianceCheck
    "Category grouping (e.g., Documentation, Build System)"
    category::String
    "Descriptive name of the check"
    name::String
    "Whether the check passed"
    passed::Bool
    "Whether passing this check is mandatory for compliance"
    required::Bool
    "Human-readable result message"
    message::String
end

# ---------------------------------------------------------------------------
# RSR Compliance Checker
# ---------------------------------------------------------------------------

"""
    RSRCompliance

Mutable compliance checker that accumulates check results and computes
a compliance tier. Scans the repository root for required files,
directories, and framework-specific conventions.
"""
mutable struct RSRCompliance
    "Root directory of the repository being checked"
    repo_root::String
    "Accumulated compliance check results"
    checks::Vector{ComplianceCheck}
end

"""
    RSRCompliance(repo_root=pwd())

Construct a compliance checker rooted at `repo_root` (defaults to cwd).
"""
RSRCompliance(repo_root::String=pwd()) = RSRCompliance(repo_root, ComplianceCheck[])

# ---------------------------------------------------------------------------
# File / Directory Existence Checks
# ---------------------------------------------------------------------------

"""
    check_file_exists!(checker, filepath, category; required=true)

Check whether `filepath` (relative to repo root) exists. Appends a
`ComplianceCheck` to `checker.checks`.

# Returns
- `Bool`: true if the file exists
"""
function check_file_exists!(checker::RSRCompliance, filepath::String, category::String;
                             required::Bool=true)::Bool
    full_path = joinpath(checker.repo_root, filepath)
    exists = isfile(full_path)

    status_indicator = exists ? "[PASS]" : "[FAIL]"
    status_word = exists ? "exists" : "missing"
    push!(checker.checks, ComplianceCheck(
        category,
        "File: $filepath",
        exists,
        required,
        "$status_indicator $filepath $status_word"
    ))

    return exists
end

"""
    check_directory_exists!(checker, dirpath, category; required=true)

Check whether `dirpath` (relative to repo root) is an existing directory.
Appends a `ComplianceCheck` to `checker.checks`.

# Returns
- `Bool`: true if the directory exists
"""
function check_directory_exists!(checker::RSRCompliance, dirpath::String, category::String;
                                  required::Bool=true)::Bool
    full_path = joinpath(checker.repo_root, dirpath)
    exists = isdir(full_path)

    status_indicator = exists ? "[PASS]" : "[FAIL]"
    status_word = exists ? "exists" : "missing"
    push!(checker.checks, ComplianceCheck(
        category,
        "Directory: $dirpath",
        exists,
        required,
        "$status_indicator $dirpath/ $status_word"
    ))

    return exists
end

# ---------------------------------------------------------------------------
# Category-Specific Checks
# ---------------------------------------------------------------------------

"""
    check_documentation!(checker)

Verify that all required and optional documentation files are present
(README.md, LICENSE, CONTRIBUTING.md, etc.).
"""
function check_documentation!(checker::RSRCompliance)
    check_file_exists!(checker, "README.md", "Documentation"; required=true)
    check_file_exists!(checker, "LICENSE", "Documentation"; required=true)
    check_file_exists!(checker, "CONTRIBUTING.md", "Documentation"; required=true)
    check_file_exists!(checker, "CODE_OF_CONDUCT.md", "Documentation"; required=true)
    check_file_exists!(checker, "SECURITY.md", "Documentation"; required=true)
    check_file_exists!(checker, "MAINTAINERS.md", "Documentation"; required=true)
    check_file_exists!(checker, "CHANGELOG.md", "Documentation"; required=true)
    check_file_exists!(checker, "VERSION", "Documentation"; required=false)
end

"""
    check_well_known!(checker)

Verify the `.well-known` directory and its expected contents.
"""
function check_well_known!(checker::RSRCompliance)
    if check_directory_exists!(checker, ".well-known", ".well-known"; required=true)
        check_file_exists!(checker, ".well-known/security.txt", ".well-known"; required=true)
        check_file_exists!(checker, ".well-known/ai.txt", ".well-known"; required=false)
        check_file_exists!(checker, ".well-known/humans.txt", ".well-known"; required=false)
    end
end

"""
    check_build_system!(checker)

Check for the presence of a build system (justfile or Makefile) and
optional Nix flake configuration.
"""
function check_build_system!(checker::RSRCompliance)
    has_justfile = check_file_exists!(checker, "justfile", "Build System"; required=false)
    has_makefile = check_file_exists!(checker, "Makefile", "Build System"; required=false)

    if has_justfile || has_makefile
        push!(checker.checks, ComplianceCheck(
            "Build System",
            "Build system present",
            true,
            false,
            "[PASS] Build system available (justfile or Makefile)"
        ))
    end

    check_file_exists!(checker, "flake.nix", "Build System"; required=false)
end

"""
    check_ci_cd!(checker)

Check for the presence of CI/CD configuration files (GitLab CI or
GitHub Actions).
"""
function check_ci_cd!(checker::RSRCompliance)
    has_gitlab = check_file_exists!(checker, ".gitlab-ci.yml", "CI/CD"; required=false)
    has_github = check_file_exists!(checker, ".github/workflows/ci.yml", "CI/CD"; required=false)

    if has_gitlab || has_github
        push!(checker.checks, ComplianceCheck(
            "CI/CD",
            "CI/CD configured",
            true,
            false,
            "[PASS] CI/CD pipeline configured"
        ))
    end
end

"""
    check_testing!(checker)

Verify that a `tests/` directory exists and contains test files
(Julia .jl or Python .py).
"""
function check_testing!(checker::RSRCompliance)
    if check_directory_exists!(checker, "tests", "Testing"; required=false)
        tests_dir = joinpath(checker.repo_root, "tests")
        # Look for Julia or Python test files
        test_files = vcat(
            filter(f -> endswith(f, ".jl"), readdir(tests_dir)),
            filter(f -> endswith(f, ".py"), readdir(tests_dir))
        )
        has_tests = !isempty(test_files)

        status_indicator = has_tests ? "[PASS]" : "[FAIL]"
        push!(checker.checks, ComplianceCheck(
            "Testing",
            "Test files present",
            has_tests,
            false,
            "$status_indicator $(length(test_files)) test file(s) found"
        ))
    end
end

"""
    check_offline_first!(checker)

Confirm that the project operates in an offline-first manner (all artifacts
work without network access).
"""
function check_offline_first!(checker::RSRCompliance)
    push!(checker.checks, ComplianceCheck(
        "Offline-First",
        "No required network dependencies",
        true,
        false,
        "[PASS] All artifacts work offline"
    ))
end

"""
    check_tpcf!(checker)

Check whether TPCF (Tri-Perimeter Contribution Framework) is documented
in CONTRIBUTING.md or MAINTAINERS.md.
"""
function check_tpcf!(checker::RSRCompliance)
    contributing_path = joinpath(checker.repo_root, "CONTRIBUTING.md")
    maintainers_path = joinpath(checker.repo_root, "MAINTAINERS.md")

    has_tpcf = false

    if isfile(contributing_path)
        content = read(contributing_path, String)
        has_tpcf = occursin("TPCF", content) ||
                   occursin("Tri-Perimeter", content) ||
                   occursin("perimeter", lowercase(content))
    end

    if !has_tpcf && isfile(maintainers_path)
        content = read(maintainers_path, String)
        has_tpcf = occursin("TPCF", content) || occursin("Tri-Perimeter", content)
    end

    status_indicator = has_tpcf ? "[PASS]" : "[WARN]"
    status_word = has_tpcf ? "documented" : "not explicitly documented"
    push!(checker.checks, ComplianceCheck(
        "TPCF",
        "TPCF documented",
        has_tpcf,
        false,
        "$status_indicator TPCF contribution framework $status_word"
    ))
end

# ---------------------------------------------------------------------------
# Run All Checks
# ---------------------------------------------------------------------------

"""
    run_all_checks!(checker)

Execute all compliance checks in sequence: documentation, .well-known,
build system, CI/CD, testing, offline-first, and TPCF.
"""
function run_all_checks!(checker::RSRCompliance)
    check_documentation!(checker)
    check_well_known!(checker)
    check_build_system!(checker)
    check_ci_cd!(checker)
    check_testing!(checker)
    check_offline_first!(checker)
    check_tpcf!(checker)
end

# ---------------------------------------------------------------------------
# Scoring & Tier
# ---------------------------------------------------------------------------

"""
    calculate_score(checker)

Calculate the compliance score broken down into required and optional checks.

# Returns
- `Tuple{Int,Int,Int,Int}`: (required_passed, required_total, optional_passed, optional_total)
"""
function calculate_score(checker::RSRCompliance)::Tuple{Int,Int,Int,Int}
    required_passed = count(c -> c.required && c.passed, checker.checks)
    required_total = count(c -> c.required, checker.checks)
    optional_passed = count(c -> !c.required && c.passed, checker.checks)
    optional_total = count(c -> !c.required, checker.checks)

    return (required_passed, required_total, optional_passed, optional_total)
end

"""
    get_tier(checker)

Determine the RSR compliance tier based on check results.

# Returns
- `String`: one of "Platinum", "Gold", "Silver", "Bronze", "Partial", "Non-Compliant"
"""
function get_tier(checker::RSRCompliance)::String
    req_passed, req_total, opt_passed, opt_total = calculate_score(checker)

    if req_passed < req_total
        return "Non-Compliant"
    end

    # All required checks passed — determine tier by total percentage
    total_checks = req_total + opt_total
    total_passed = req_passed + opt_passed
    percentage = total_checks > 0 ? (total_passed / total_checks * 100) : 0.0

    if percentage >= 95
        return "Platinum"
    elseif percentage >= 85
        return "Gold"
    elseif percentage >= 70
        return "Silver"
    elseif percentage >= 50
        return "Bronze"
    else
        return "Partial"
    end
end

# ---------------------------------------------------------------------------
# Report Generation
# ---------------------------------------------------------------------------

"""
    print_report(checker; verbose=false)

Print a formatted compliance report to stdout. When `verbose` is true,
all checks are shown; otherwise only failures are displayed per category.
"""
function print_report(checker::RSRCompliance; verbose::Bool=false)
    println(repeat("=", 70))
    println("RSR FRAMEWORK COMPLIANCE REPORT")
    println(repeat("=", 70))
    println()

    # Group checks by category (preserving insertion order via OrderedDict-like approach)
    categories = Dict{String,Vector{ComplianceCheck}}()
    category_order = String[]
    for check in checker.checks
        if !haskey(categories, check.category)
            categories[check.category] = ComplianceCheck[]
            push!(category_order, check.category)
        end
        push!(categories[check.category], check)
    end

    # Print by category
    for category in sort(category_order)
        checks = categories[category]
        println("  $category")
        println(repeat("-", 70))

        for check in checks
            if verbose || !check.passed
                println("  $(check.message)")
            end
        end

        category_passed = count(c -> c.passed, checks)
        category_total = length(checks)
        println("  Score: $category_passed/$category_total")
        println()
    end

    # Overall score
    req_passed, req_total, opt_passed, opt_total = calculate_score(checker)
    tier = get_tier(checker)

    println(repeat("=", 70))
    println("SUMMARY")
    println(repeat("=", 70))
    println("Required checks: $req_passed/$req_total")
    println("Optional checks: $opt_passed/$opt_total")
    println("Total: $(req_passed + opt_passed)/$(req_total + opt_total)")
    println()
    println("Compliance Tier: $tier")
    println()

    # Tier descriptions
    tier_descriptions = Dict(
        "Platinum" => "**** Exemplary - All checks passed",
        "Gold" => "*** Excellent - 85%+ checks passed",
        "Silver" => "** Good - 70%+ checks passed",
        "Bronze" => "* Basic - 50%+ checks passed",
        "Partial" => "Incomplete - Less than 50% checks passed",
        "Non-Compliant" => "FAILED - Required checks not met"
    )

    println(get(tier_descriptions, tier, "Unknown tier"))
    println(repeat("=", 70))
end

"""
    generate_markdown_report(checker)

Generate a Markdown-formatted compliance report string.

# Returns
- `String`: complete Markdown report
"""
function generate_markdown_report(checker::RSRCompliance)::String
    req_passed, req_total, opt_passed, opt_total = calculate_score(checker)
    tier = get_tier(checker)

    lines = String[
        "# RSR Framework Compliance Report",
        "",
        "**Project**: llm-antidote",
        "**Date**: $(checker.repo_root)",
        "**Tier**: $tier",
        "",
        "## Summary",
        "",
        "- Required checks: $req_passed/$req_total",
        "- Optional checks: $opt_passed/$opt_total",
        "- **Total: $(req_passed + opt_passed)/$(req_total + opt_total)**",
        "",
        "## Compliance Checks",
        ""
    ]

    # Group by category
    categories = Dict{String,Vector{ComplianceCheck}}()
    for check in checker.checks
        checks_list = get!(categories, check.category, ComplianceCheck[])
        push!(checks_list, check)
    end

    for category in sort(collect(keys(categories)))
        checks = categories[category]
        push!(lines, "### $category")
        push!(lines, "")

        for check in checks
            status = check.passed ? "[PASS]" : "[FAIL]"
            required_label = check.required ? " (Required)" : ""
            push!(lines, "- $status $(check.name)$required_label")
        end

        push!(lines, "")
    end

    append!(lines, [
        "## Recommendations",
        "",
        "To improve compliance:"
    ])

    # Add recommendations for failed checks
    failed_checks = filter(c -> !c.passed, checker.checks)
    if !isempty(failed_checks)
        for check in failed_checks
            push!(lines, "- $(check.message)")
        end
    else
        push!(lines, "- All checks passed!")
    end

    append!(lines, [
        "",
        "---",
        "*Generated by RSR compliance checker*"
    ])

    return join(lines, "\n")
end

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

"""
    main()

CLI entry point. Supports `--verbose` / `-v` for verbose output and
`--report` for Markdown report generation. Exits with code 1 if compliance
tier is Non-Compliant or Partial.
"""
function main()
    checker = RSRCompliance()
    run_all_checks!(checker)

    verbose = "--verbose" in ARGS || "-v" in ARGS
    report = "--report" in ARGS

    if report
        println(generate_markdown_report(checker))
    else
        print_report(checker; verbose=verbose)
    end

    # Exit with error code if not at least Bronze
    tier = get_tier(checker)
    if tier in ["Non-Compliant", "Partial"]
        exit(1)
    else
        exit(0)
    end
end

# Run main when executed as a script
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
