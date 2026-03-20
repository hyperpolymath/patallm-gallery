#!/usr/bin/env julia
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

"""
    LLM Antidote CLI Tool

    Author: Jonathan D.A. Jewell
    Purpose: Command-line interface for managing reset artifacts and diagnostics

    Usage:
      llm_cli.jl reset [universal|coding|conversation]
      llm_cli.jl preserve
      llm_cli.jl verify
      llm_cli.jl diagnose [memory|continuity|association]
      llm_cli.jl list
      llm_cli.jl info <artifact>
"""

# ---------------------------------------------------------------------------
# Artifact Manager
# ---------------------------------------------------------------------------

"""
    ArtifactManager

Manages LLM reset and preservation artifacts by scanning the repository
directory structure for known artifact files and providing access to their
contents.
"""
struct ArtifactManager
    "Root directory of the llm-antidote repository"
    repo_root::String
    "Path to the artifacts/ subdirectory"
    artifacts_dir::String
    "Path to the tools/ subdirectory"
    tools_dir::String
end

"""
    ArtifactManager(; repo_root=nothing)

Construct an ArtifactManager, optionally specifying the repo root. If not
provided, walks up from cwd looking for the `llm-reset` file to find the
repository root.
"""
function ArtifactManager(; repo_root::Union{String,Nothing}=nothing)
    if isnothing(repo_root)
        # Walk up from cwd to find repo root containing llm-reset
        current = pwd()
        while true
            if isfile(joinpath(current, "llm-reset"))
                repo_root = current
                break
            end
            parent = dirname(current)
            if parent == current
                # Reached filesystem root without finding it
                repo_root = pwd()
                break
            end
            current = parent
        end
    end

    return ArtifactManager(
        repo_root,
        joinpath(repo_root, "artifacts"),
        joinpath(repo_root, "tools")
    )
end

"""
    list_artifacts(manager)

List all available artifacts with their metadata (name, file path, format,
and description).

# Returns
- `Vector{Dict{String,String}}`: list of artifact metadata dictionaries
"""
function list_artifacts(manager::ArtifactManager)::Vector{Dict{String,String}}
    artifacts = Dict{String,String}[]

    # Check root directory for the universal reset artifact
    if isfile(joinpath(manager.repo_root, "llm-reset"))
        push!(artifacts, Dict(
            "name" => "universal",
            "file" => "llm-reset",
            "format" => "scheme",
            "description" => "Universal semantic reset (original)"
        ))
    end

    # Check artifacts directory for variant artifacts
    if isdir(manager.artifacts_dir)
        artifact_map = Dict{String,Tuple{String,String,String}}(
            "llm-reset.py" => ("universal-py", "python", "Universal reset (Python)"),
            "llm-reset.js" => ("universal-js", "javascript", "Universal reset (JavaScript)"),
            "llm-reset.json" => ("universal-json", "json", "Universal reset (JSON)"),
            "llm-reset-coding.scm" => ("coding", "scheme", "Coding context reset"),
            "llm-reset-conversation.scm" => ("conversation", "scheme", "Conversation thread reset"),
            "llm-preserve.scm" => ("preserve", "scheme", "Context preservation")
        )

        for (filename, (artifact_name, format, description)) in artifact_map
            if isfile(joinpath(manager.artifacts_dir, filename))
                push!(artifacts, Dict(
                    "name" => artifact_name,
                    "file" => "artifacts/$filename",
                    "format" => format,
                    "description" => description
                ))
            end
        end
    end

    return artifacts
end

"""
    get_artifact(manager, name)

Retrieve the content of a named artifact.

# Arguments
- `manager::ArtifactManager`: the artifact manager
- `name::String`: artifact name (e.g., "universal", "coding", "preserve")

# Returns
- `Union{String,Nothing}`: artifact content, or nothing if not found
"""
function get_artifact(manager::ArtifactManager, name::String)::Union{String,Nothing}
    artifacts = list_artifacts(manager)
    for artifact in artifacts
        if artifact["name"] == name
            file_path = joinpath(manager.repo_root, artifact["file"])
            if isfile(file_path)
                return read(file_path, String)
            end
        end
    end
    return nothing
end

"""
    get_diagnostic(manager, diagnostic_type)

Generate a diagnostic prompt of the given type by delegating to the
ContextProbe from llm_diagnostic.jl.

# Arguments
- `manager::ArtifactManager`: the artifact manager
- `diagnostic_type::String`: one of "memory", "continuity", "verify", "association"

# Returns
- `Union{String,Nothing}`: the diagnostic prompt, or nothing if type is unknown
"""
function get_diagnostic(manager::ArtifactManager, diagnostic_type::String)::Union{String,Nothing}
    # Import diagnostic functions from the sibling module
    diagnostic_path = joinpath(manager.tools_dir, "llm_diagnostic.jl")
    if isfile(diagnostic_path)
        include(diagnostic_path)
    end

    probe = ContextProbe()

    if diagnostic_type == "memory"
        return generate_memory_probe(probe)
    elseif diagnostic_type == "continuity"
        return generate_continuity_probe(probe)
    elseif diagnostic_type == "verify"
        return generate_reset_verification(probe)
    elseif diagnostic_type == "association"
        return generate_association_probe(probe, "context")
    else
        return nothing
    end
end

# ---------------------------------------------------------------------------
# Output Formatting
# ---------------------------------------------------------------------------

"""
    print_artifact(content)

Print artifact content wrapped in separator lines with a copy instruction.
"""
function print_artifact(content::String)
    println()
    println(repeat("=", 70))
    println(content)
    println(repeat("=", 70))
    println()
    println("Copy the above artifact and paste it into your LLM chat.")
    println()
end

"""
    print_artifact_list(artifacts)

Print a formatted table of available artifacts.
"""
function print_artifact_list(artifacts::Vector{Dict{String,String}})
    println("\nAvailable Artifacts:\n")
    for (index, artifact) in enumerate(artifacts)
        padded_name = rpad(artifact["name"], 20)
        padded_format = rpad("($(artifact["format"]))", 12)
        println("$index. $padded_name $padded_format - $(artifact["description"])")
    end
    println()
end

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

"""
    print_usage()

Print CLI usage information.
"""
function print_usage()
    println("""

LLM Antidote CLI Tool

Usage:
  llm_cli.jl reset [TYPE]       Display reset artifact
  llm_cli.jl preserve           Display context preservation artifact
  llm_cli.jl verify             Display reset verification tool
  llm_cli.jl diagnose [TYPE]    Display diagnostic prompt
  llm_cli.jl list               List all artifacts
  llm_cli.jl info <artifact>    Show artifact information

Reset Types:
  universal       Universal semantic reset (default)
  universal-py    Universal reset (Python)
  universal-js    Universal reset (JavaScript)
  universal-json  Universal reset (JSON)
  coding          Coding context reset
  conversation    Conversation thread reset

Diagnostic Types:
  memory          Test what LLM remembers
  continuity      Test conversation continuity
  verify          Verify reset effectiveness
  association     Test word associations

Examples:
  llm_cli.jl reset                    # Show universal reset
  llm_cli.jl reset coding             # Show coding context reset
  llm_cli.jl preserve                 # Show preservation artifact
  llm_cli.jl diagnose memory          # Show memory diagnostic
  llm_cli.jl list                     # List all artifacts

Workflow:
  1. Use 'reset' to clear LLM context
  2. Use 'verify' to check if reset worked
  3. Use 'preserve' to mark important context
  4. Use 'diagnose' to probe context state
""")
end

"""
    main()

CLI entry point. Supports subcommands: list, reset [type], preserve, verify,
diagnose [type], info <name>.
"""
function main()
    manager = ArtifactManager()

    if length(ARGS) < 1
        print_usage()
        return
    end

    command = ARGS[1]

    if command == "list"
        print_artifact_list(list_artifacts(manager))

    elseif command == "reset"
        artifact_type = length(ARGS) > 1 ? ARGS[2] : "universal"
        content = get_artifact(manager, artifact_type)
        if !isnothing(content)
            print_artifact(content)
        else
            println("Artifact '$artifact_type' not found.")
            println("\nUse 'llm_cli.jl list' to see available artifacts.")
        end

    elseif command == "preserve"
        content = get_artifact(manager, "preserve")
        if !isnothing(content)
            print_artifact(content)
        else
            println("Preservation artifact not found.")
        end

    elseif command == "verify"
        verify_path = joinpath(manager.tools_dir, "llm-verify.scm")
        if isfile(verify_path)
            print_artifact(read(verify_path, String))
        else
            println("Verification tool not found.")
        end

    elseif command == "diagnose"
        diagnostic_type = length(ARGS) > 1 ? ARGS[2] : "memory"
        content = get_diagnostic(manager, diagnostic_type)
        if !isnothing(content)
            print_artifact(content)
        else
            println("Diagnostic '$diagnostic_type' not found.")
            println("\nAvailable: memory, continuity, verify, association")
        end

    elseif command == "info"
        if length(ARGS) < 2
            println("Please specify an artifact name.")
            println("Use 'llm_cli.jl list' to see available artifacts.")
        else
            artifact_name = ARGS[2]
            artifacts = list_artifacts(manager)
            found = false
            for artifact in artifacts
                if artifact["name"] == artifact_name
                    println("\nArtifact: $(artifact["name"])")
                    println("   File: $(artifact["file"])")
                    println("   Format: $(artifact["format"])")
                    println("   Description: $(artifact["description"])\n")
                    found = true
                    break
                end
            end
            if !found
                println("Artifact '$artifact_name' not found.")
            end
        end

    else
        println("Unknown command: $command")
        print_usage()
    end
end

# Run main when executed as a script
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
