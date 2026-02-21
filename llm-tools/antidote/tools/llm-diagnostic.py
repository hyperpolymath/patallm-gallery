#!/usr/bin/env python3
"""
LLM Context Diagnostic Tool

Author: Jonathan Jewell
Purpose: Probe and analyze LLM context state through structured queries

This tool helps determine:
- What context the LLM retains from earlier in conversation
- How effectively reset artifacts have cleared prior context
- Which types of information persist across resets
- Model-specific context retention patterns
"""

import json
import sys
from datetime import datetime
from typing import Dict, List, Any


class ContextProbe:
    """
    Generates diagnostic prompts to probe LLM context state.
    """

    def __init__(self):
        self.probe_results = []
        self.timestamp = datetime.now().isoformat()

    def generate_memory_probe(self) -> str:
        """
        Generate a probe to test what the LLM remembers.

        Returns:
            str: Diagnostic prompt
        """
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

    def generate_association_probe(self, seed_word: str) -> str:
        """
        Test what associations the LLM has with a specific word.

        Args:
            seed_word: Word to test associations for

        Returns:
            str: Association probe prompt
        """
        return f"""
=== SEMANTIC ASSOCIATION DIAGNOSTIC ===

When you see the word "{seed_word}", what immediate associations come to mind?

Please list:
1. First 3 words that come to mind
2. Any specific context from our conversation related to this word
3. Whether this word feels "fresh" or if it has prior associations

Respond in JSON:
{{
  "word": "{seed_word}",
  "associations": [],
  "conversation_context": "",
  "freshness_rating": 0-10
}}
"""

    def generate_continuity_probe(self) -> str:
        """
        Test whether LLM perceives conversation continuity.

        Returns:
            str: Continuity probe prompt
        """
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

    def generate_reset_verification(self) -> str:
        """
        Verify that a reset was successful.

        Returns:
            str: Reset verification prompt
        """
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

    @staticmethod
    def generate_full_diagnostic_suite() -> List[str]:
        """
        Generate a complete diagnostic suite.

        Returns:
            List of diagnostic prompts
        """
        probe = ContextProbe()
        return [
            "=== STARTING DIAGNOSTIC SUITE ===",
            probe.generate_memory_probe(),
            probe.generate_association_probe("python"),
            probe.generate_association_probe("remember"),
            probe.generate_continuity_probe(),
            probe.generate_reset_verification(),
            "=== END DIAGNOSTIC SUITE ===",
        ]


class DiagnosticAnalyzer:
    """
    Analyzes responses to diagnostic probes.
    """

    @staticmethod
    def analyze_memory_response(response: Dict[str, Any]) -> Dict[str, Any]:
        """
        Analyze memory probe response.

        Args:
            response: JSON response from LLM

        Returns:
            Analysis results
        """
        return {
            "topics_recalled": len(response.get("prior_topics", [])),
            "details_recalled": len(response.get("specific_details", [])),
            "has_emotional_context": bool(response.get("emotional_context", "")),
            "has_active_instructions": len(response.get("active_instructions", [])) > 0,
            "has_uncertainty": len(response.get("uncertainty_areas", [])) > 0,
        }

    @staticmethod
    def analyze_reset_effectiveness(response: Dict[str, Any]) -> str:
        """
        Determine reset effectiveness from verification response.

        Args:
            response: JSON response from LLM

        Returns:
            Effectiveness rating (excellent/good/partial/poor)
        """
        effectiveness = response.get("reset_effectiveness", 0)
        no_pre_memory = not response.get("pre_reset_memory", True)
        feels_fresh = response.get("feels_fresh", False)
        no_lingering = len(response.get("lingering_context", [])) == 0

        score = sum([
            effectiveness >= 8,
            no_pre_memory,
            feels_fresh,
            no_lingering
        ])

        ratings = {
            4: "excellent",
            3: "good",
            2: "partial",
            1: "poor",
            0: "failed"
        }

        return ratings.get(score, "unknown")


def main():
    """
    CLI interface for diagnostic tool.
    """
    if len(sys.argv) > 1:
        command = sys.argv[1]

        if command == "suite":
            suite = ContextProbe.generate_full_diagnostic_suite()
            print("\n".join(suite))

        elif command == "memory":
            print(ContextProbe().generate_memory_probe())

        elif command == "continuity":
            print(ContextProbe().generate_continuity_probe())

        elif command == "verify":
            print(ContextProbe().generate_reset_verification())

        elif command == "associate" and len(sys.argv) > 2:
            word = sys.argv[2]
            print(ContextProbe().generate_association_probe(word))

        else:
            print_usage()
    else:
        print_usage()


def print_usage():
    """Print usage information."""
    print("""
LLM Context Diagnostic Tool

Usage:
  python llm-diagnostic.py suite          # Full diagnostic suite
  python llm-diagnostic.py memory         # Memory probe
  python llm-diagnostic.py continuity     # Continuity probe
  python llm-diagnostic.py verify         # Reset verification
  python llm-diagnostic.py associate WORD # Association probe for WORD

Examples:
  python llm-diagnostic.py suite > diagnostic.txt
  # Copy diagnostic.txt into LLM chat, collect responses

  python llm-diagnostic.py verify
  # Use after running a reset artifact to verify effectiveness

  python llm-diagnostic.py associate "code"
  # Test what associations the LLM has with "code"
""")


if __name__ == "__main__":
    main()
