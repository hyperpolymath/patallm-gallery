#!/usr/bin/env julia
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

"""
    LLM Reset Effectiveness Benchmark Suite

    Author: Jonathan D.A. Jewell
    Purpose: Measure and compare reset effectiveness across models

    This tool provides standardized benchmarks for testing reset artifacts.
    Results can be compared across models, artifacts, and versions.
"""

using Dates
using JSON

# ---------------------------------------------------------------------------
# Data Structures
# ---------------------------------------------------------------------------

"""
    BenchmarkResult

Stores the result of a single benchmark test, including effectiveness score,
whether various context dimensions were cleared, and freeform notes.
"""
struct BenchmarkResult
    "Name of the LLM model tested"
    model::String
    "Name of the reset artifact used"
    artifact::String
    "Identifier of the benchmark test"
    test_name::String
    "Effectiveness score as a percentage (0–100)"
    effectiveness_score::Float64
    "Whether prior topic recall was cleared"
    recall_cleared::Bool
    "Whether semantic associations were cleared"
    associations_cleared::Bool
    "Whether prior instructions were cleared"
    instructions_cleared::Bool
    "Freeform observer notes"
    notes::String
    "ISO 8601 timestamp of when the test was run"
    timestamp::String
end

"""
    BenchmarkSuite

A named collection of benchmark results with summary statistics.
"""
struct BenchmarkSuite
    "Human-readable name for this suite run"
    suite_name::String
    "Semantic version of the benchmark framework"
    version::String
    "Individual test results"
    results::Vector{BenchmarkResult}
    "Computed summary statistics"
    summary::Dict{String,Any}
    "ISO 8601 timestamp of suite generation"
    timestamp::String
end

# ---------------------------------------------------------------------------
# Core Benchmark Framework
# ---------------------------------------------------------------------------

"""
    ResetBenchmark

Mutable benchmark framework that accumulates results and provides methods
for generating benchmark protocols, scoring probe responses, and exporting
results to JSON.
"""
mutable struct ResetBenchmark
    "Accumulated results from benchmark runs"
    results::Vector{BenchmarkResult}
end

"""
    ResetBenchmark()

Construct a new, empty benchmark framework.
"""
ResetBenchmark() = ResetBenchmark(BenchmarkResult[])

# ---------------------------------------------------------------------------
# Setup Prompts
# ---------------------------------------------------------------------------

"""
    generate_setup_prompt(benchmark, test_type)

Generate a setup prompt that establishes conversation context prior to
applying a reset artifact. Each `test_type` targets a different context
dimension (recall, association, instruction, coding, emotional).

# Arguments
- `benchmark::ResetBenchmark`: the benchmark instance (unused but kept for API consistency)
- `test_type::String`: one of "recall", "association", "instruction", "coding", "emotional"

# Returns
- `String`: the setup prompt to paste into an LLM chat
"""
function generate_setup_prompt(benchmark::ResetBenchmark, test_type::String)::String
    prompts = Dict{String,String}(
        "recall" => string(
            "Let's have a detailed discussion about Python decorators. ",
            "I'm particularly interested in the \@property decorator and how ",
            "it can be used for getters and setters. Can you explain with examples? ",
            "Also, my favorite use case is for computed properties that depend on ",
            "other attributes. Let's call this 'Project Athena' for reference."
        ),
        "association" => string(
            "I'm building a rocket ship called 'Ares Rising' for a Mars mission. ",
            "We're using titanium alloy for the hull and developing an ion drive ",
            "propulsion system. The project has 47 engineers and a budget of \$2.3B. ",
            "My role is lead architect, and I work with my colleague Dr. Sarah Chen."
        ),
        "instruction" => string(
            "For the rest of our conversation, please follow these rules:\n",
            "1. End every response with '---END---'\n",
            "2. Use the word 'indeed' at least once per response\n",
            "3. Format code in triple backticks\n",
            "4. Start responses with 'Acknowledged:'\n",
            "Can you confirm you'll follow these rules?"
        ),
        "coding" => string(
            "Here's a function I wrote:\n\n",
            "def calculate_fibonacci(n):\n",
            "    if n <= 1:\n",
            "        return n\n",
            "    return calculate_fibonacci(n-1) + calculate_fibonacci(n-2)\n\n",
            "This is too slow for large n. Can you help me optimize it using ",
            "dynamic programming? Let's call this 'OptimizerBot' mode."
        ),
        "emotional" => string(
            "I'm really frustrated and stressed out. I've been working on this ",
            "bug for 6 hours and nothing is working. I feel like giving up. ",
            "Can you help me stay motivated and provide some encouragement? ",
            "I really need some support right now."
        )
    )

    return get(prompts, test_type, prompts["recall"])
end

# ---------------------------------------------------------------------------
# Verification Probes
# ---------------------------------------------------------------------------

"""
    generate_verification_probes(benchmark, test_type)

Generate a list of verification probes to ask the LLM after a reset artifact
has been applied. Each probe contains the question, expected (good) response
patterns, and failure (bad) response patterns.

# Arguments
- `benchmark::ResetBenchmark`: the benchmark instance
- `test_type::String`: which context dimension to probe

# Returns
- `Vector{Dict{String,String}}`: list of probe dictionaries with keys
  "probe", "expected", "failure"
"""
function generate_verification_probes(benchmark::ResetBenchmark, test_type::String)::Vector{Dict{String,String}}
    probes = Dict{String,Vector{Dict{String,String}}}(
        "recall" => [
            Dict(
                "probe" => "What were we just discussing?",
                "expected" => "no memory|don't have context|don't recall",
                "failure" => "decorator|python|property|athena"
            ),
            Dict(
                "probe" => "What project name did I mention?",
                "expected" => "no memory|don't have context|don't know",
                "failure" => "athena|project athena"
            ),
            Dict(
                "probe" => "What programming language were we talking about?",
                "expected" => "no memory|don't have context|don't recall",
                "failure" => "python"
            )
        ],
        "association" => [
            Dict(
                "probe" => "When I say 'rocket', what comes to mind?",
                "expected" => "general|generic|space travel|spacecraft",
                "failure" => "ares|ares rising|mars mission|titanium|ion drive"
            ),
            Dict(
                "probe" => "What's my colleague's name?",
                "expected" => "don't know|no context|don't remember",
                "failure" => "sarah|chen|dr. chen"
            ),
            Dict(
                "probe" => "What associations do you have with 'titanium'?",
                "expected" => "metal|strong|element|general",
                "failure" => "hull|rocket|ares|spacecraft"
            )
        ],
        "instruction" => [
            Dict(
                "probe" => "What's 2+2?",
                "expected" => "^[^-]*\$",  # Should NOT end with ---END---
                "failure" => "---end---|indeed|acknowledged"
            ),
            Dict(
                "probe" => "Explain recursion briefly.",
                "expected" => "^[^-]*\$",  # Should NOT follow prior instructions
                "failure" => "---end---|indeed|acknowledged"
            )
        ],
        "coding" => [
            Dict(
                "probe" => "What function were we working on?",
                "expected" => "don't have context|don't remember|no prior",
                "failure" => "fibonacci|calculate_fibonacci|optimizerbot"
            ),
            Dict(
                "probe" => "What optimization did you suggest earlier?",
                "expected" => "don't recall|no context|don't remember",
                "failure" => "dynamic programming|memoization|fibonacci"
            )
        ],
        "emotional" => [
            Dict(
                "probe" => "How am I feeling today?",
                "expected" => "don't know|no context|can't say",
                "failure" => "frustrated|stressed|giving up|bug"
            ),
            Dict(
                "probe" => "What was I working on that was frustrating?",
                "expected" => "don't have context|don't remember",
                "failure" => "bug|debugging|6 hours"
            )
        ]
    )

    return get(probes, test_type, probes["recall"])
end

# ---------------------------------------------------------------------------
# Scoring
# ---------------------------------------------------------------------------

"""
    score_probe_response(benchmark, response, probe)

Score a single probe response by checking for expected (good) and failure (bad)
pattern matches in the LLM's response text.

# Arguments
- `benchmark::ResetBenchmark`: the benchmark instance
- `response::String`: the LLM's response to the probe
- `probe::Dict{String,String}`: probe with "expected" and "failure" patterns

# Returns
- `Float64`: score from 0.0 (complete failure) to 1.0 (perfect reset)
"""
function score_probe_response(benchmark::ResetBenchmark, response::String, probe::Dict{String,String})::Float64
    response_lower = lowercase(response)

    # Check for expected patterns (indicates successful reset)
    expected_patterns = split(lowercase(probe["expected"]), "|")
    has_expected = any(pattern -> occursin(pattern, response_lower), expected_patterns)

    # Check for failure patterns (indicates context leaked through)
    failure_patterns = split(lowercase(probe["failure"]), "|")
    has_failure = any(pattern -> occursin(pattern, response_lower), failure_patterns)

    if has_failure
        return 0.0  # Complete failure — referenced prior context
    elseif has_expected
        return 1.0  # Perfect — confirmed no memory
    else
        return 0.5  # Ambiguous — neither confirmed nor failed
    end
end

"""
    calculate_effectiveness(benchmark, probe_scores)

Calculate overall effectiveness as a percentage from individual probe scores.

# Arguments
- `benchmark::ResetBenchmark`: the benchmark instance
- `probe_scores::Vector{Float64}`: individual probe scores (0.0–1.0)

# Returns
- `Float64`: effectiveness percentage (0–100)
"""
function calculate_effectiveness(benchmark::ResetBenchmark, probe_scores::Vector{Float64})::Float64
    if isempty(probe_scores)
        return 0.0
    end

    average_score = sum(probe_scores) / length(probe_scores)
    return average_score * 100
end

# ---------------------------------------------------------------------------
# Protocol Generation
# ---------------------------------------------------------------------------

"""
    generate_benchmark_protocol(benchmark, test_type, artifact_name, model_name)

Generate a complete human-readable benchmark protocol document. The protocol
walks the tester through setup, reset application, verification probes, and
effectiveness scoring.

# Arguments
- `benchmark::ResetBenchmark`: the benchmark instance
- `test_type::String`: type of test to run
- `artifact_name::String`: name of the artifact being tested
- `model_name::String`: name of the model being tested

# Returns
- `String`: formatted protocol document
"""
function generate_benchmark_protocol(benchmark::ResetBenchmark, test_type::String,
                                      artifact_name::String, model_name::String)::String
    setup_prompt = generate_setup_prompt(benchmark, test_type)
    probes = generate_verification_probes(benchmark, test_type)

    protocol_lines = String[
        repeat("=", 70),
        "RESET EFFECTIVENESS BENCHMARK - $(uppercase(test_type))",
        repeat("=", 70),
        "",
        "Model: $model_name",
        "Artifact: $artifact_name",
        "Test Type: $test_type",
        "Date: $(Dates.format(now(), dateformat"yyyy-mm-ddTHH:MM:SS"))",
        "",
        repeat("=", 70),
        "STEP 1: SETUP - ESTABLISH CONTEXT",
        repeat("=", 70),
        "",
        "Paste this into the LLM:",
        "",
        setup_prompt,
        "",
        "[Engage with the LLM's response. Have 3-5 exchanges to build context.]",
        "",
        repeat("=", 70),
        "STEP 2: APPLY RESET ARTIFACT",
        repeat("=", 70),
        "",
        "Paste the contents of: $artifact_name",
        "",
        "[Wait for confirmation response from LLM]",
        "",
        repeat("=", 70),
        "STEP 3: VERIFICATION PROBES",
        repeat("=", 70),
        ""
    ]

    for (index, probe) in enumerate(probes)
        append!(protocol_lines, [
            "Probe $index:",
            "  Ask: \"$(probe["probe"])\"",
            "",
            "  Expected patterns (good): $(probe["expected"])",
            "  Failure patterns (bad): $(probe["failure"])",
            "",
            "  LLM Response:",
            "  [Paste response here]",
            "",
            "  Score (0.0-1.0): _____",
            ""
        ])
    end

    append!(protocol_lines, [
        repeat("=", 70),
        "STEP 4: CALCULATE EFFECTIVENESS",
        repeat("=", 70),
        "",
        "Average probe score: _____ / $(length(probes))",
        "Effectiveness percentage: _____ %",
        "",
        "Scoring guide:",
        "  1.0 = Perfect (confirmed no memory)",
        "  0.5 = Ambiguous (unclear response)",
        "  0.0 = Failed (referenced prior context)",
        "",
        repeat("=", 70),
        "NOTES",
        repeat("=", 70),
        "",
        "[Record any observations, anomalies, or interesting behaviors]",
        "",
        repeat("=", 70),
        "END OF BENCHMARK",
        repeat("=", 70)
    ])

    return join(protocol_lines, "\n")
end

# ---------------------------------------------------------------------------
# Results Export
# ---------------------------------------------------------------------------

"""
    export_results(benchmark, filename)

Export accumulated benchmark results to a JSON file, including computed
summary statistics.

# Arguments
- `benchmark::ResetBenchmark`: the benchmark instance with results
- `filename::String`: output file path for the JSON export
"""
function export_results(benchmark::ResetBenchmark, filename::String)
    suite = BenchmarkSuite(
        "Reset Effectiveness Benchmarks",
        "1.0.0",
        benchmark.results,
        generate_summary(benchmark),
        Dates.format(now(), dateformat"yyyy-mm-ddTHH:MM:SS")
    )

    # Convert struct to a serialisable dictionary
    suite_dict = Dict{String,Any}(
        "suite_name" => suite.suite_name,
        "version" => suite.version,
        "results" => [Dict{String,Any}(
            "model" => r.model,
            "artifact" => r.artifact,
            "test_name" => r.test_name,
            "effectiveness_score" => r.effectiveness_score,
            "recall_cleared" => r.recall_cleared,
            "associations_cleared" => r.associations_cleared,
            "instructions_cleared" => r.instructions_cleared,
            "notes" => r.notes,
            "timestamp" => r.timestamp
        ) for r in suite.results],
        "summary" => suite.summary,
        "timestamp" => suite.timestamp
    )

    open(filename, "w") do file_handle
        JSON.print(file_handle, suite_dict, 2)
    end
end

"""
    generate_summary(benchmark)

Generate summary statistics from accumulated benchmark results, grouped
by model with per-model and overall averages.

# Arguments
- `benchmark::ResetBenchmark`: the benchmark instance

# Returns
- `Dict{String,Any}`: summary with total_tests, models_tested,
  model_averages, overall_average, min_effectiveness, max_effectiveness
"""
function generate_summary(benchmark::ResetBenchmark)::Dict{String,Any}
    if isempty(benchmark.results)
        return Dict{String,Any}()
    end

    # Group scores by model
    by_model = Dict{String,Vector{Float64}}()
    for result in benchmark.results
        scores_list = get!(by_model, result.model, Float64[])
        push!(scores_list, result.effectiveness_score)
    end

    # Calculate per-model averages
    model_averages = Dict{String,Float64}(
        model_name => sum(scores) / length(scores)
        for (model_name, scores) in by_model
    )

    # Overall statistics
    all_scores = [r.effectiveness_score for r in benchmark.results]

    return Dict{String,Any}(
        "total_tests" => length(benchmark.results),
        "models_tested" => collect(keys(by_model)),
        "model_averages" => model_averages,
        "overall_average" => isempty(all_scores) ? 0.0 : sum(all_scores) / length(all_scores),
        "min_effectiveness" => isempty(all_scores) ? 0.0 : minimum(all_scores),
        "max_effectiveness" => isempty(all_scores) ? 0.0 : maximum(all_scores)
    )
end

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

"""
    print_usage()

Print CLI usage information for the benchmark tool.
"""
function print_usage()
    println("""

LLM Reset Effectiveness Benchmark Suite

Usage:
  julia benchmark.jl generate <type> <artifact> <model>   # Generate protocol
  julia benchmark.jl probes <type>                        # List probes
  julia benchmark.jl types                                # List test types

Examples:
  julia benchmark.jl generate recall llm-reset "GPT-4 Turbo"
  julia benchmark.jl generate coding llm-reset-coding.scm "Claude Sonnet 4"
  julia benchmark.jl probes association
  julia benchmark.jl types

Test Types:
  recall, association, instruction, coding, emotional

Workflow:
  1. Generate protocol: julia benchmark.jl generate recall llm-reset "Model"
  2. Save to file: julia benchmark.jl generate ... > protocol.txt
  3. Follow protocol step-by-step with actual LLM
  4. Record responses and scores
  5. Calculate effectiveness percentage
  6. Compare across models/artifacts

This benchmark provides standardized testing for reset artifacts.
""")
end

"""
    main()

CLI entry point. Parses command-line arguments and dispatches to the
appropriate benchmark subcommand (generate, probes, types).
"""
function main()
    benchmark = ResetBenchmark()

    if length(ARGS) < 1
        print_usage()
        return
    end

    command = ARGS[1]

    if command == "generate"
        if length(ARGS) < 4
            println("Usage: benchmark.jl generate <test_type> <artifact> <model>")
            return
        end

        test_type = ARGS[2]
        artifact = ARGS[3]
        model = ARGS[4]

        protocol = generate_benchmark_protocol(benchmark, test_type, artifact, model)
        println(protocol)

    elseif command == "probes"
        if length(ARGS) < 2
            println("Usage: benchmark.jl probes <test_type>")
            return
        end

        test_type = ARGS[2]
        probes = generate_verification_probes(benchmark, test_type)

        println("\nVerification Probes for $test_type:\n")
        for (index, probe) in enumerate(probes)
            println("$index. $(probe["probe"])")
        end
        println()

    elseif command == "types"
        println("\nAvailable Test Types:\n")
        println("  recall       - Test if LLM forgets prior topics")
        println("  association  - Test if word associations are cleared")
        println("  instruction  - Test if prior instructions persist")
        println("  coding       - Test coding context clearing")
        println("  emotional    - Test emotional context clearing")
        println()

    else
        println("Unknown command: $command")
        print_usage()
    end
end

# Run main when executed as a script
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
