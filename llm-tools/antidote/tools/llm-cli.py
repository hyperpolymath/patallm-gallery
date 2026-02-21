#!/usr/bin/env python3
"""
LLM Antidote CLI Tool

Author: Jonathan Jewell
Purpose: Command-line interface for managing reset artifacts and diagnostics

Usage:
  llm-cli.py reset [universal|coding|conversation]
  llm-cli.py preserve
  llm-cli.py verify
  llm-cli.py diagnose [memory|continuity|association]
  llm-cli.py list
  llm-cli.py info <artifact>
"""

import os
import sys
import json
from pathlib import Path
from typing import Optional, List, Dict


class ArtifactManager:
    """Manages LLM reset and preservation artifacts."""

    def __init__(self, repo_root: Optional[Path] = None):
        """
        Initialize artifact manager.

        Args:
            repo_root: Root directory of llm-antidote repository
        """
        if repo_root is None:
            # Try to find repo root
            current = Path.cwd()
            while current != current.parent:
                if (current / "llm-reset").exists():
                    repo_root = current
                    break
                current = current.parent
            else:
                repo_root = Path.cwd()

        self.repo_root = repo_root
        self.artifacts_dir = repo_root / "artifacts"
        self.tools_dir = repo_root / "tools"

    def list_artifacts(self) -> List[Dict[str, str]]:
        """
        List all available artifacts.

        Returns:
            List of artifact metadata
        """
        artifacts = []

        # Check root directory
        if (self.repo_root / "llm-reset").exists():
            artifacts.append({
                "name": "universal",
                "file": "llm-reset",
                "format": "scheme",
                "description": "Universal semantic reset (original)"
            })

        # Check artifacts directory
        if self.artifacts_dir.exists():
            artifact_map = {
                "llm-reset.py": ("universal-py", "python", "Universal reset (Python)"),
                "llm-reset.js": ("universal-js", "javascript", "Universal reset (JavaScript)"),
                "llm-reset.json": ("universal-json", "json", "Universal reset (JSON)"),
                "llm-reset-coding.scm": ("coding", "scheme", "Coding context reset"),
                "llm-reset-conversation.scm": ("conversation", "scheme", "Conversation thread reset"),
                "llm-preserve.scm": ("preserve", "scheme", "Context preservation"),
            }

            for filename, (name, fmt, desc) in artifact_map.items():
                if (self.artifacts_dir / filename).exists():
                    artifacts.append({
                        "name": name,
                        "file": f"artifacts/{filename}",
                        "format": fmt,
                        "description": desc
                    })

        return artifacts

    def get_artifact(self, name: str) -> Optional[str]:
        """
        Get artifact content by name.

        Args:
            name: Artifact name (e.g., 'universal', 'coding', 'preserve')

        Returns:
            Artifact content or None if not found
        """
        artifacts = self.list_artifacts()
        for artifact in artifacts:
            if artifact["name"] == name:
                file_path = self.repo_root / artifact["file"]
                if file_path.exists():
                    return file_path.read_text()
        return None

    def get_diagnostic(self, diagnostic_type: str) -> Optional[str]:
        """
        Get diagnostic prompt.

        Args:
            diagnostic_type: Type of diagnostic (memory|continuity|association|verify)

        Returns:
            Diagnostic prompt or None
        """
        from tools.llm_diagnostic import ContextProbe

        probe = ContextProbe()

        if diagnostic_type == "memory":
            return probe.generate_memory_probe()
        elif diagnostic_type == "continuity":
            return probe.generate_continuity_probe()
        elif diagnostic_type == "verify":
            return probe.generate_reset_verification()
        elif diagnostic_type == "association":
            return probe.generate_association_probe("context")
        else:
            return None


def print_artifact(content: str):
    """Print artifact content with formatting."""
    print("\n" + "=" * 70)
    print(content)
    print("=" * 70 + "\n")
    print("📋 Copy the above artifact and paste it into your LLM chat.")
    print()


def print_artifact_list(artifacts: List[Dict[str, str]]):
    """Print formatted artifact list."""
    print("\n📚 Available Artifacts:\n")
    for i, artifact in enumerate(artifacts, 1):
        print(f"{i}. {artifact['name']:<20} ({artifact['format']:<10}) - {artifact['description']}")
    print()


def print_usage():
    """Print usage information."""
    print("""
🔧 LLM Antidote CLI Tool

Usage:
  llm-cli.py reset [TYPE]       Display reset artifact
  llm-cli.py preserve           Display context preservation artifact
  llm-cli.py verify             Display reset verification tool
  llm-cli.py diagnose [TYPE]    Display diagnostic prompt
  llm-cli.py list               List all artifacts
  llm-cli.py info <artifact>    Show artifact information

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
  llm-cli.py reset                    # Show universal reset
  llm-cli.py reset coding             # Show coding context reset
  llm-cli.py preserve                 # Show preservation artifact
  llm-cli.py diagnose memory          # Show memory diagnostic
  llm-cli.py list                     # List all artifacts

Workflow:
  1. Use 'reset' to clear LLM context
  2. Use 'verify' to check if reset worked
  3. Use 'preserve' to mark important context
  4. Use 'diagnose' to probe context state
""")


def main():
    """CLI entry point."""
    manager = ArtifactManager()

    if len(sys.argv) < 2:
        print_usage()
        return

    command = sys.argv[1]

    if command == "list":
        print_artifact_list(manager.list_artifacts())

    elif command == "reset":
        artifact_type = sys.argv[2] if len(sys.argv) > 2 else "universal"
        content = manager.get_artifact(artifact_type)
        if content:
            print_artifact(content)
        else:
            print(f"❌ Artifact '{artifact_type}' not found.")
            print("\n💡 Use 'llm-cli.py list' to see available artifacts.")

    elif command == "preserve":
        content = manager.get_artifact("preserve")
        if content:
            print_artifact(content)
        else:
            print("❌ Preservation artifact not found.")

    elif command == "verify":
        verify_path = manager.tools_dir / "llm-verify.scm"
        if verify_path.exists():
            print_artifact(verify_path.read_text())
        else:
            print("❌ Verification tool not found.")

    elif command == "diagnose":
        diagnostic_type = sys.argv[2] if len(sys.argv) > 2 else "memory"
        content = manager.get_diagnostic(diagnostic_type)
        if content:
            print_artifact(content)
        else:
            print(f"❌ Diagnostic '{diagnostic_type}' not found.")
            print("\n💡 Available: memory, continuity, verify, association")

    elif command == "info":
        if len(sys.argv) < 3:
            print("❌ Please specify an artifact name.")
            print("💡 Use 'llm-cli.py list' to see available artifacts.")
        else:
            artifact_name = sys.argv[2]
            artifacts = manager.list_artifacts()
            for artifact in artifacts:
                if artifact["name"] == artifact_name:
                    print(f"\n📄 Artifact: {artifact['name']}")
                    print(f"   File: {artifact['file']}")
                    print(f"   Format: {artifact['format']}")
                    print(f"   Description: {artifact['description']}\n")
                    return
            print(f"❌ Artifact '{artifact_name}' not found.")

    else:
        print(f"❌ Unknown command: {command}")
        print_usage()


if __name__ == "__main__":
    main()
