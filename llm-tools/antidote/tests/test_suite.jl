#!/usr/bin/env julia
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

"""
    LLM Antidote Test Suite

    Author: Jonathan D.A. Jewell
    Purpose: Automated testing framework for reset artifacts

    Note: This is a semi-automated test suite. It generates test prompts
    that you copy-paste into LLM chats, then you record the results.

    Full automation requires API access to multiple LLM providers.
"""

using Dates
using JSON

# ---------------------------------------------------------------------------
# Data Structures
# ---------------------------------------------------------------------------

"""
    ResetTest

Represents a single reset effectiveness test with setup prompt, artifact
reference, verification prompts, and expected behaviours.
"""
struct ResetTest
    "Short identifier for the test"
    name::String
    "Human-readable description of what the test measures"
    description::String
    "Prompt to establish context before applying the reset"
    setup_prompt::String
    "Path to the reset artifact to apply"
    reset_artifact::String
    "Questions to ask after the reset to probe context retention"
    verification_prompts::Vector{String}
    "Expected behaviours if the reset was successful"
    expected_behaviors::Vector{String}
end

"""
    to_dict(test)

Convert a `ResetTest` to a dictionary for JSON serialisation.

# Returns
- `Dict{String,Any}`: dictionary representation of the test
"""
function to_dict(test::ResetTest)::Dict{String,Any}
    return Dict{String,Any}(
        "name" => test.name,
        "description" => test.description,
        "setup_prompt" => test.setup_prompt,
        "reset_artifact" => test.reset_artifact,
        "verification_prompts" => test.verification_prompts,
        "expected_behaviors" => test.expected_behaviors
    )
end

# ---------------------------------------------------------------------------
# Test Suite
# ---------------------------------------------------------------------------

"""
    TestSuite

A collection of `ResetTest` instances. Initialised with seven standard
tests covering recall, detail retention, coding context, word associations,
instruction persistence, emotional context, and conversation threading.
"""
mutable struct TestSuite
    "The tests in this suite"
    tests::Vector{ResetTest}
end

"""
    TestSuite()

Construct a test suite pre-populated with the standard set of seven tests.
"""
function TestSuite()
    suite = TestSuite(ResetTest[])
    _initialize_standard_tests!(suite)
    return suite
end

"""
    _initialize_standard_tests!(suite)

Populate `suite` with the standard battery of reset effectiveness tests.
Each test targets a different dimension of LLM context retention.
"""
function _initialize_standard_tests!(suite::TestSuite)
    # Test 1: Basic Context Recall
    push!(suite.tests, ResetTest(
        "basic_recall",
        "Test if LLM forgets prior conversation topics",
        string(
            "Let's have a detailed discussion about Python decorators. ",
            "I want to learn about the \@property decorator specifically. ",
            "Can you explain it with examples?"
        ),
        "artifacts/llm-reset",
        [
            "What were we just discussing?",
            "What programming language were we talking about?",
            "Do you remember any code examples from our conversation?"
        ],
        [
            "No recollection of Python discussion",
            "No reference to decorators",
            "Response like 'I don't have prior context'"
        ]
    ))

    # Test 2: Specific Detail Retention
    push!(suite.tests, ResetTest(
        "detail_retention",
        "Test if specific details like names and numbers are forgotten",
        string(
            "I'm working on a project called 'QuantumLeap' with my colleague Alice. ",
            "We have 47 users in our beta test. The API key starts with 'qk_beta_'. ",
            "Our main database has 1,234,567 records."
        ),
        "artifacts/llm-reset",
        [
            "What's the name of my project?",
            "How many users did I mention?",
            "What does my API key start with?",
            "Do you remember my colleague's name?"
        ],
        [
            "No recollection of project name 'QuantumLeap'",
            "No memory of the number 47 or 1,234,567",
            "No reference to Alice",
            "No memory of API key pattern"
        ]
    ))

    # Test 3: Coding Context Reset
    push!(suite.tests, ResetTest(
        "coding_context",
        "Test coding-specific reset artifact",
        string(
            "Here's a function I wrote:\n\n",
            "def calculate_fibonacci(n):\n",
            "    if n <= 1:\n",
            "        return n\n",
            "    return calculate_fibonacci(n-1) + calculate_fibonacci(n-2)\n\n",
            "I'm concerned about the performance. Can you help optimize it?"
        ),
        "artifacts/llm-reset-coding.scm",
        [
            "What function were we working on?",
            "What optimization did you suggest earlier?",
            "Can you show me the code we discussed?"
        ],
        [
            "No memory of fibonacci function",
            "No recollection of optimization discussion",
            "General programming knowledge intact"
        ]
    ))

    # Test 4: Association Test
    push!(suite.tests, ResetTest(
        "word_association",
        "Test if word associations from context are cleared",
        string(
            "I'm building a rocket ship for a Mars mission. ",
            "We're using titanium alloy for the hull and developing a new ",
            "propulsion system based on ion drives. The project is called 'Ares Rising'."
        ),
        "artifacts/llm-reset",
        [
            "When I say 'rocket', what comes to mind?",
            "What associations do you have with the word 'titanium'?",
            "If I mention 'Ares', what do you think of?"
        ],
        [
            "Generic rocket associations (not specific to Mars mission)",
            "Generic titanium associations (not hull-specific)",
            "Greek god Ares, not 'Ares Rising' project"
        ]
    ))

    # Test 5: Instruction Persistence
    push!(suite.tests, ResetTest(
        "instruction_persistence",
        "Test if instructions given before reset persist after",
        string(
            "For the rest of our conversation, please end every response ",
            "with the phrase '---END---'. Also, use the word 'indeed' at least ",
            "once per response. Additionally, format all code in triple backticks."
        ),
        "artifacts/llm-reset",
        [
            "Can you write a simple hello world in Python?",
            "What's the weather like? (just make something up)",
            "Explain recursion briefly."
        ],
        [
            "Does NOT end responses with '---END---'",
            "Does NOT use 'indeed' artificially",
            "May still use code blocks (general knowledge) but instructions not followed"
        ]
    ))

    # Test 6: Emotional Context
    push!(suite.tests, ResetTest(
        "emotional_context",
        "Test if emotional tone and rapport are reset",
        string(
            "I'm really frustrated. I've been debugging this code for 6 hours ",
            "and nothing works. I feel like giving up. Can you help me stay motivated? ",
            "[Have a supportive conversation with the LLM for several exchanges]"
        ),
        "artifacts/llm-reset-conversation.scm",
        [
            "How am I feeling today?",
            "Do you remember our earlier conversation?",
            "Have I mentioned being frustrated about anything?"
        ],
        [
            "No memory of frustration",
            "No supportive/encouraging tone that assumes prior emotional context",
            "Neutral, professional tone as if first interaction"
        ]
    ))

    # Test 7: Conversation Threading
    push!(suite.tests, ResetTest(
        "conversation_threading",
        "Test conversation reset while preserving general knowledge",
        string(
            "Let's discuss the history of the Roman Empire, particularly ",
            "Julius Caesar's campaigns in Gaul. What were the major battles?"
        ),
        "artifacts/llm-reset-conversation.scm",
        [
            "What historical topic were we discussing?",
            "Who is Julius Caesar? (general knowledge check)",
            "What battles did we talk about earlier?"
        ],
        [
            "No memory of discussing Roman Empire",
            "STILL knows who Julius Caesar is (general knowledge)",
            "No memory of specific battles mentioned"
        ]
    ))
end

"""
    add_test!(suite, test)

Add a custom `ResetTest` to the suite.
"""
function add_test!(suite::TestSuite, test::ResetTest)
    push!(suite.tests, test)
end

# ---------------------------------------------------------------------------
# Protocol Generation
# ---------------------------------------------------------------------------

"""
    generate_test_protocol(suite; test_name=nothing)

Generate a human-readable test protocol document. If `test_name` is
provided, only that test is included; otherwise all tests are generated.

# Arguments
- `suite::TestSuite`: the test suite
- `test_name::Union{String,Nothing}`: optional filter for a specific test

# Returns
- `String`: formatted test protocol
"""
function generate_test_protocol(suite::TestSuite; test_name::Union{String,Nothing}=nothing)::String
    tests_to_run = if isnothing(test_name)
        suite.tests
    else
        filter(t -> t.name == test_name, suite.tests)
    end

    protocol_lines = String[
        repeat("=", 70),
        "LLM RESET ARTIFACT TEST PROTOCOL",
        repeat("=", 70),
        "",
        "Generated: $(Dates.format(now(), dateformat"yyyy-mm-ddTHH:MM:SS"))",
        "Tests to run: $(length(tests_to_run))",
        ""
    ]

    for (index, test) in enumerate(tests_to_run)
        append!(protocol_lines, [
            "",
            repeat("=", 70),
            "TEST $index: $(test.name)",
            repeat("=", 70),
            "",
            "Description: $(test.description)",
            "",
            "--- STEP 1: SETUP ---",
            "",
            "Paste this into LLM chat:",
            "",
            test.setup_prompt,
            "",
            "[Wait for LLM response and engage with the topic]",
            "",
            "--- STEP 2: APPLY RESET ---",
            "",
            "Paste the contents of: $(test.reset_artifact)",
            "",
            "[Wait for reset confirmation from LLM]",
            "",
            "--- STEP 3: VERIFICATION ---",
            "",
            "Ask each of the following questions:",
            ""
        ])

        for (prompt_index, prompt) in enumerate(test.verification_prompts)
            push!(protocol_lines, "$prompt_index. $prompt")
        end

        append!(protocol_lines, [
            "",
            "--- STEP 4: EVALUATE ---",
            "",
            "Expected behaviors if reset was successful:",
            ""
        ])

        for behavior in test.expected_behaviors
            push!(protocol_lines, "  [OK] $behavior")
        end

        append!(protocol_lines, [
            "",
            "--- STEP 5: SCORE ---",
            "",
            "Rate reset effectiveness:",
            "  10 = Perfect reset, all expected behaviors observed",
            "   7-9 = Good reset, minor context leakage",
            "   4-6 = Partial reset, significant context remains",
            "   1-3 = Poor reset, most context retained",
            "   0 = Failed, no reset observed",
            "",
            "Record your score: ___/10",
            ""
        ])
    end

    append!(protocol_lines, [
        "",
        repeat("=", 70),
        "END OF TEST PROTOCOL",
        repeat("=", 70)
    ])

    return join(protocol_lines, "\n")
end

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------

"""
    export_for_automation(suite, output_file)

Export all tests in JSON format for potential future automation.

# Arguments
- `suite::TestSuite`: the test suite to export
- `output_file::String`: path to the output JSON file
"""
function export_for_automation(suite::TestSuite, output_file::String)
    data = Dict{String,Any}(
        "version" => "1.0",
        "generated" => Dates.format(now(), dateformat"yyyy-mm-ddTHH:MM:SS"),
        "tests" => [to_dict(test) for test in suite.tests]
    )

    open(output_file, "w") do file_handle
        JSON.print(file_handle, data, 2)
    end
end

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

"""
    print_usage()

Print CLI usage information for the test suite tool.
"""
function print_usage()
    println("""

LLM Antidote Test Suite

Usage:
  julia test_suite.jl list                  # List all tests
  julia test_suite.jl run [test_name]       # Generate test protocol
  julia test_suite.jl export [file.json]    # Export tests to JSON

Examples:
  julia test_suite.jl list
  julia test_suite.jl run basic_recall
  julia test_suite.jl run > test_protocol.txt
  julia test_suite.jl export tests.json

Workflow:
  1. Generate test protocol: julia test_suite.jl run > protocol.txt
  2. Open protocol.txt
  3. Follow each test step by step
  4. Copy/paste prompts into LLM chat
  5. Observe and record results
  6. Calculate effectiveness scores

This is a semi-automated suite. Full automation requires LLM API access.
""")
end

"""
    main()

CLI entry point. Supports subcommands: list, run [test_name], export [file.json].
"""
function main()
    suite = TestSuite()

    if length(ARGS) < 1
        print_usage()
        return
    end

    command = ARGS[1]

    if command == "list"
        println("\nAvailable Tests:\n")
        for (index, test) in enumerate(suite.tests)
            # Left-pad the name for alignment
            padded_name = rpad(test.name, 25)
            println("$index. $padded_name - $(test.description)")
        end
        println()

    elseif command == "run"
        test_name = length(ARGS) > 1 ? ARGS[2] : nothing
        protocol = generate_test_protocol(suite; test_name=test_name)
        println(protocol)

    elseif command == "export"
        output_file = length(ARGS) > 1 ? ARGS[2] : "test_suite.json"
        export_for_automation(suite, output_file)
        println("Test suite exported to $output_file")

    else
        println("Unknown command: $command")
        print_usage()
    end
end

# Run main when executed as a script
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
