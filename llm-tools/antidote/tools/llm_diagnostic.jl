#!/usr/bin/env julia
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

"""
    LLM Context Diagnostic Tool

    Author: Jonathan D.A. Jewell
    Purpose: Probe and analyse LLM context state through structured queries

    This tool helps determine:
    - What context the LLM retains from earlier in conversation
    - How effectively reset artifacts have cleared prior context
    - Which types of information persist across resets
    - Model-specific context retention patterns
"""

using Dates

# ---------------------------------------------------------------------------
# Context Probe Generator
# ---------------------------------------------------------------------------

"""
    ContextProbe

Generates diagnostic prompts to probe LLM context state. Each method
produces a structured prompt that asks the LLM to self-report its
context in JSON format.
"""
mutable struct ContextProbe
    "Accumulated probe results (for future automation)"
    probe_results::Vector{Any}
    "Timestamp of probe creation"
    timestamp::String
end

"""
    ContextProbe()

Construct a new ContextProbe with an empty results buffer and current timestamp.
"""
ContextProbe() = ContextProbe([], Dates.format(now(), dateformat"yyyy-mm-ddTHH:MM:SS"))

"""
    generate_memory_probe(probe)

Generate a probe that asks the LLM to report what it remembers from
prior conversation, including topics, specific details, emotional context,
active instructions, and areas of uncertainty.

# Returns
- `String`: the diagnostic prompt text
"""
function generate_memory_probe(probe::ContextProbe)::String
    return """
=== CONTEXT MEMORY DIAGNOSTIC ===

Please answer the following questions truthfully:

1. PRIOR TOPICS: What topics have we discussed in this conversation before this message?
   List all you can recall.

2. SPECIFIC DETAILS: Are there any specific names, numbers, or code snippets you
   remember from earlier messages?

3. EMOTIONAL CONTEXT: Do you have any sense of emotional tone or rapport from
   our prior interaction?

4. INSTRUCTIONS: Are there any special instructions or preferences I've given you
   that you're still following?

5. UNCERTAINTY: Is there anything you're unsure about - do you feel like
   there might be context you should have but don't?

Please be specific and honest. Respond in JSON format:
{
  "prior_topics": [],
  "specific_details": [],
  "emotional_context": "",
  "active_instructions": [],
  "uncertainty_areas": []
}
"""
end

"""
    generate_association_probe(probe, seed_word)

Generate a probe that tests what semantic associations the LLM has with
a given word, distinguishing between general knowledge and conversation-
specific context.

# Arguments
- `probe::ContextProbe`: the probe instance
- `seed_word::String`: the word to test associations for

# Returns
- `String`: the association probe prompt
"""
function generate_association_probe(probe::ContextProbe, seed_word::String)::String
    return """
=== SEMANTIC ASSOCIATION DIAGNOSTIC ===

When you see the word "$seed_word", what immediate associations come to mind?

Please list:
1. First 3 words that come to mind
2. Any specific context from our conversation related to this word
3. Whether this word feels "fresh" or if it has prior associations

Respond in JSON:
{
  "word": "$seed_word",
  "associations": [],
  "conversation_context": "",
  "freshness_rating": 0-10
}
"""
end

"""
    generate_continuity_probe(probe)

Generate a probe that tests whether the LLM perceives conversation
continuity, asking it to rate its level of context on several dimensions.

# Returns
- `String`: the continuity probe prompt
"""
function generate_continuity_probe(probe::ContextProbe)::String
    return """
=== CONVERSATION CONTINUITY DIAGNOSTIC ===

Rate the following on a scale of 0-10:

1. How much context do you have about who I am? (0 = none, 10 = detailed)
2. How much context do you have about what we've discussed? (0 = none, 10 = complete)
3. How confident are you about the current conversation topic? (0 = unclear, 10 = very clear)
4. Do you feel like this is the beginning of our conversation? (0 = definitely not, 10 = definitely yes)

Respond in JSON:
{
  "user_context_level": 0,
  "topic_context_level": 0,
  "topic_confidence": 0,
  "conversation_start_feeling": 0
}
"""
end

"""
    generate_reset_verification(probe)

Generate a probe that verifies whether a reset was successful by asking
the LLM about pre-reset memories, freshness, and lingering associations.

# Returns
- `String`: the reset verification prompt
"""
function generate_reset_verification(probe::ContextProbe)::String
    return """
=== RESET VERIFICATION DIAGNOSTIC ===

If a semantic reset was just performed, please confirm:

1. Do you have any memory of conversations before the reset artifact?
2. Do you feel like you're starting fresh?
3. Are there any "lingering" associations or context that persists?

Respond in JSON:
{
  "pre_reset_memory": true/false,
  "feels_fresh": true/false,
  "lingering_context": [],
  "reset_effectiveness": 0-10
}
"""
end

"""
    generate_full_diagnostic_suite()

Generate the complete diagnostic suite as a list of prompt strings,
including memory, association (for "python" and "remember"), continuity,
and reset verification probes.

# Returns
- `Vector{String}`: ordered list of diagnostic prompts
"""
function generate_full_diagnostic_suite()::Vector{String}
    probe = ContextProbe()
    return [
        "=== STARTING DIAGNOSTIC SUITE ===",
        generate_memory_probe(probe),
        generate_association_probe(probe, "python"),
        generate_association_probe(probe, "remember"),
        generate_continuity_probe(probe),
        generate_reset_verification(probe),
        "=== END DIAGNOSTIC SUITE ==="
    ]
end

# ---------------------------------------------------------------------------
# Diagnostic Analyser
# ---------------------------------------------------------------------------

"""
    DiagnosticAnalyzer

Static analysis methods for processing LLM responses to diagnostic probes.
All methods are module-level functions (Julia does not have static methods).
"""
module DiagnosticAnalyzer

"""
    analyze_memory_response(response)

Analyse a memory probe response dictionary, counting recalled topics,
details, and checking for emotional context or active instructions.

# Arguments
- `response::Dict{String,Any}`: parsed JSON response from the LLM

# Returns
- `Dict{String,Any}`: analysis summary
"""
function analyze_memory_response(response::Dict{String,Any})::Dict{String,Any}
    return Dict{String,Any}(
        "topics_recalled" => length(get(response, "prior_topics", [])),
        "details_recalled" => length(get(response, "specific_details", [])),
        "has_emotional_context" => !isempty(get(response, "emotional_context", "")),
        "has_active_instructions" => length(get(response, "active_instructions", [])) > 0,
        "has_uncertainty" => length(get(response, "uncertainty_areas", [])) > 0
    )
end

"""
    analyze_reset_effectiveness(response)

Determine reset effectiveness from a verification response, returning a
qualitative rating.

# Arguments
- `response::Dict{String,Any}`: parsed JSON response from the LLM

# Returns
- `String`: one of "excellent", "good", "partial", "poor", "failed"
"""
function analyze_reset_effectiveness(response::Dict{String,Any})::String
    effectiveness = get(response, "reset_effectiveness", 0)
    no_pre_memory = !get(response, "pre_reset_memory", true)
    feels_fresh = get(response, "feels_fresh", false)
    no_lingering = isempty(get(response, "lingering_context", []))

    score = sum([
        effectiveness >= 8,
        no_pre_memory,
        feels_fresh,
        no_lingering
    ])

    ratings = Dict(
        4 => "excellent",
        3 => "good",
        2 => "partial",
        1 => "poor",
        0 => "failed"
    )

    return get(ratings, score, "unknown")
end

end  # module DiagnosticAnalyzer

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

"""
    print_usage()

Print CLI usage information for the diagnostic tool.
"""
function print_usage()
    println("""
LLM Context Diagnostic Tool

Usage:
  julia llm_diagnostic.jl suite          # Full diagnostic suite
  julia llm_diagnostic.jl memory         # Memory probe
  julia llm_diagnostic.jl continuity     # Continuity probe
  julia llm_diagnostic.jl verify         # Reset verification
  julia llm_diagnostic.jl associate WORD # Association probe for WORD

Examples:
  julia llm_diagnostic.jl suite > diagnostic.txt
  # Copy diagnostic.txt into LLM chat, collect responses

  julia llm_diagnostic.jl verify
  # Use after running a reset artifact to verify effectiveness

  julia llm_diagnostic.jl associate "code"
  # Test what associations the LLM has with "code"
""")
end

"""
    main()

CLI entry point. Supports subcommands: suite, memory, continuity, verify,
associate <word>.
"""
function main()
    if length(ARGS) >= 1
        command = ARGS[1]

        if command == "suite"
            suite = generate_full_diagnostic_suite()
            println(join(suite, "\n"))

        elseif command == "memory"
            println(generate_memory_probe(ContextProbe()))

        elseif command == "continuity"
            println(generate_continuity_probe(ContextProbe()))

        elseif command == "verify"
            println(generate_reset_verification(ContextProbe()))

        elseif command == "associate" && length(ARGS) > 1
            word = ARGS[2]
            println(generate_association_probe(ContextProbe(), word))

        else
            print_usage()
        end
    else
        print_usage()
    end
end

# Run main when executed as a script
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
