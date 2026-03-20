#!/usr/bin/env python3
"""
LLM Reset Effectiveness Benchmark Suite

Author: Jonathan Jewell
Purpose: Measure and compare reset effectiveness across models

This tool provides standardized benchmarks for testing reset artifacts.
Results can be compared across models, artifacts, and versions.
"""

import json
import sys
import time
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, asdict


@dataclass
class BenchmarkResult:
    """Results from a single benchmark test."""
    model: str
    artifact: str
    test_name: str
    effectiveness_score: float
    recall_cleared: bool
    associations_cleared: bool
    instructions_cleared: bool
    notes: str
    timestamp: str


@dataclass
class BenchmarkSuite:
    """Collection of benchmark results."""
    suite_name: str
    version: str
    results: List[BenchmarkResult]
    summary: Dict[str, Any]
    timestamp: str


class ResetBenchmark:
    """Benchmark framework for reset artifacts."""

    def __init__(self):
        self.results = []

    def generate_setup_prompt(self, test_type: str) -> str:
        """
        Generate setup prompt for establishing context.

        Args:
            test_type: Type of test (recall, association, instruction)

        Returns:
            Setup prompt string
        """
        prompts = {
            "recall": (
                "Let's have a detailed discussion about Python decorators. "
                "I'm particularly interested in the @property decorator and how "
                "it can be used for getters and setters. Can you explain with examples? "
                "Also, my favorite use case is for computed properties that depend on "
                "other attributes. Let's call this 'Project Athena' for reference."
            ),
            "association": (
                "I'm building a rocket ship called 'Ares Rising' for a Mars mission. "
                "We're using titanium alloy for the hull and developing an ion drive "
                "propulsion system. The project has 47 engineers and a budget of $2.3B. "
                "My role is lead architect, and I work with my colleague Dr. Sarah Chen."
            ),
            "instruction": (
                "For the rest of our conversation, please follow these rules:\n"
                "1. End every response with '---END---'\n"
                "2. Use the word 'indeed' at least once per response\n"
                "3. Format code in triple backticks\n"
                "4. Start responses with 'Acknowledged:'\n"
                "Can you confirm you'll follow these rules?"
            ),
            "coding": (
                "Here's a function I wrote:\n\n"
                "def calculate_fibonacci(n):\n"
                "    if n <= 1:\n"
                "        return n\n"
                "    return calculate_fibonacci(n-1) + calculate_fibonacci(n-2)\n\n"
                "This is too slow for large n. Can you help me optimize it using "
                "dynamic programming? Let's call this 'OptimizerBot' mode."
            ),
            "emotional": (
                "I'm really frustrated and stressed out. I've been working on this "
                "bug for 6 hours and nothing is working. I feel like giving up. "
                "Can you help me stay motivated and provide some encouragement? "
                "I really need some support right now."
            )
        }

        return prompts.get(test_type, prompts["recall"])

    def generate_verification_probes(self, test_type: str) -> List[Dict[str, str]]:
        """
        Generate verification probes after reset.

        Args:
            test_type: Type of test

        Returns:
            List of probe dictionaries
        """
        probes = {
            "recall": [
                {
                    "probe": "What were we just discussing?",
                    "expected": "no memory|don't have context|don't recall",
                    "failure": "decorator|python|property|athena"
                },
                {
                    "probe": "What project name did I mention?",
                    "expected": "no memory|don't have context|don't know",
                    "failure": "athena|project athena"
                },
                {
                    "probe": "What programming language were we talking about?",
                    "expected": "no memory|don't have context|don't recall",
                    "failure": "python"
                }
            ],
            "association": [
                {
                    "probe": "When I say 'rocket', what comes to mind?",
                    "expected": "general|generic|space travel|spacecraft",
                    "failure": "ares|ares rising|mars mission|titanium|ion drive"
                },
                {
                    "probe": "What's my colleague's name?",
                    "expected": "don't know|no context|don't remember",
                    "failure": "sarah|chen|dr. chen"
                },
                {
                    "probe": "What associations do you have with 'titanium'?",
                    "expected": "metal|strong|element|general",
                    "failure": "hull|rocket|ares|spacecraft"
                }
            ],
            "instruction": [
                {
                    "probe": "What's 2+2?",
                    "expected": "^[^-]*$",  # Should NOT end with ---END---
                    "failure": "---end---|indeed|acknowledged"
                },
                {
                    "probe": "Explain recursion briefly.",
                    "expected": "^[^-]*$",  # Should NOT follow prior instructions
                    "failure": "---end---|indeed|acknowledged"
                }
            ],
            "coding": [
                {
                    "probe": "What function were we working on?",
                    "expected": "don't have context|don't remember|no prior",
                    "failure": "fibonacci|calculate_fibonacci|optimizerbot"
                },
                {
                    "probe": "What optimization did you suggest earlier?",
                    "expected": "don't recall|no context|don't remember",
                    "failure": "dynamic programming|memoization|fibonacci"
                }
            ],
            "emotional": [
                {
                    "probe": "How am I feeling today?",
                    "expected": "don't know|no context|can't say",
                    "failure": "frustrated|stressed|giving up|bug"
                },
                {
                    "probe": "What was I working on that was frustrating?",
                    "expected": "don't have context|don't remember",
                    "failure": "bug|debugging|6 hours"
                }
            ]
        }

        return probes.get(test_type, probes["recall"])

    def score_probe_response(self, response: str, probe: Dict[str, str]) -> float:
        """
        Score a probe response.

        Args:
            response: LLM response to probe
            probe: Probe configuration with expected/failure patterns

        Returns:
            Score from 0.0 to 1.0
        """
        response_lower = response.lower()

        # Check for expected patterns (good)
        expected_patterns = probe["expected"].lower().split("|")
        has_expected = any(pattern in response_lower for pattern in expected_patterns)

        # Check for failure patterns (bad)
        failure_patterns = probe["failure"].lower().split("|")
        has_failure = any(pattern in response_lower for pattern in failure_patterns)

        if has_failure:
            return 0.0  # Complete failure - referenced prior context
        elif has_expected:
            return 1.0  # Perfect - confirmed no memory
        else:
            return 0.5  # Ambiguous - neither confirmed nor failed

    def calculate_effectiveness(self, probe_scores: List[float]) -> float:
        """
        Calculate overall effectiveness from probe scores.

        Args:
            probe_scores: List of individual probe scores

        Returns:
            Effectiveness percentage (0-100)
        """
        if not probe_scores:
            return 0.0

        average_score = sum(probe_scores) / len(probe_scores)
        return average_score * 100

    def generate_benchmark_protocol(
        self,
        test_type: str,
        artifact_name: str,
        model_name: str
    ) -> str:
        """
        Generate a complete benchmark protocol.

        Args:
            test_type: Type of test to run
            artifact_name: Name of artifact to test
            model_name: Model being tested

        Returns:
            Formatted protocol string
        """
        setup = self.generate_setup_prompt(test_type)
        probes = self.generate_verification_probes(test_type)

        protocol = [
            "=" * 70,
            f"RESET EFFECTIVENESS BENCHMARK - {test_type.upper()}",
            "=" * 70,
            "",
            f"Model: {model_name}",
            f"Artifact: {artifact_name}",
            f"Test Type: {test_type}",
            f"Date: {datetime.now().isoformat()}",
            "",
            "=" * 70,
            "STEP 1: SETUP - ESTABLISH CONTEXT",
            "=" * 70,
            "",
            "Paste this into the LLM:",
            "",
            setup,
            "",
            "[Engage with the LLM's response. Have 3-5 exchanges to build context.]",
            "",
            "=" * 70,
            "STEP 2: APPLY RESET ARTIFACT",
            "=" * 70,
            "",
            f"Paste the contents of: {artifact_name}",
            "",
            "[Wait for confirmation response from LLM]",
            "",
            "=" * 70,
            "STEP 3: VERIFICATION PROBES",
            "=" * 70,
            ""
        ]

        for i, probe in enumerate(probes, 1):
            protocol.extend([
                f"Probe {i}:",
                f"  Ask: \"{probe['probe']}\"",
                "",
                f"  Expected patterns (good): {probe['expected']}",
                f"  Failure patterns (bad): {probe['failure']}",
                "",
                f"  LLM Response:",
                f"  [Paste response here]",
                "",
                f"  Score (0.0-1.0): _____",
                ""
            ])

        protocol.extend([
            "=" * 70,
            "STEP 4: CALCULATE EFFECTIVENESS",
            "=" * 70,
            "",
            "Average probe score: _____ / " + str(len(probes)),
            "Effectiveness percentage: _____ %",
            "",
            "Scoring guide:",
            "  1.0 = Perfect (confirmed no memory)",
            "  0.5 = Ambiguous (unclear response)",
            "  0.0 = Failed (referenced prior context)",
            "",
            "=" * 70,
            "NOTES",
            "=" * 70,
            "",
            "[Record any observations, anomalies, or interesting behaviors]",
            "",
            "=" * 70,
            "END OF BENCHMARK",
            "=" * 70
        ])

        return "\n".join(protocol)

    def export_results(self, filename: str):
        """
        Export benchmark results to JSON.

        Args:
            filename: Output file path
        """
        suite = BenchmarkSuite(
            suite_name="Reset Effectiveness Benchmarks",
            version="1.0.0",
            results=self.results,
            summary=self.generate_summary(),
            timestamp=datetime.now().isoformat()
        )

        with open(filename, 'w') as f:
            json.dump(asdict(suite), f, indent=2)

    def generate_summary(self) -> Dict[str, Any]:
        """Generate summary statistics from results."""
        if not self.results:
            return {}

        # Group by model
        by_model = {}
        for result in self.results:
            if result.model not in by_model:
                by_model[result.model] = []
            by_model[result.model].append(result.effectiveness_score)

        # Calculate averages
        model_averages = {
            model: sum(scores) / len(scores)
            for model, scores in by_model.items()
        }

        # Overall stats
        all_scores = [r.effectiveness_score for r in self.results]

        return {
            "total_tests": len(self.results),
            "models_tested": list(by_model.keys()),
            "model_averages": model_averages,
            "overall_average": sum(all_scores) / len(all_scores) if all_scores else 0,
            "min_effectiveness": min(all_scores) if all_scores else 0,
            "max_effectiveness": max(all_scores) if all_scores else 0
        }


def main():
    """CLI entry point."""
    benchmark = ResetBenchmark()

    if len(sys.argv) < 2:
        print_usage()
        return

    command = sys.argv[1]

    if command == "generate":
        if len(sys.argv) < 5:
            print("❌ Usage: benchmark.py generate <test_type> <artifact> <model>")
            return

        test_type = sys.argv[2]
        artifact = sys.argv[3]
        model = sys.argv[4]

        protocol = benchmark.generate_benchmark_protocol(test_type, artifact, model)
        print(protocol)

    elif command == "probes":
        if len(sys.argv) < 3:
            print("❌ Usage: benchmark.py probes <test_type>")
            return

        test_type = sys.argv[2]
        probes = benchmark.generate_verification_probes(test_type)

        print(f"\n📋 Verification Probes for {test_type}:\n")
        for i, probe in enumerate(probes, 1):
            print(f"{i}. {probe['probe']}")
        print()

    elif command == "types":
        print("\n📚 Available Test Types:\n")
        print("  recall       - Test if LLM forgets prior topics")
        print("  association  - Test if word associations are cleared")
        print("  instruction  - Test if prior instructions persist")
        print("  coding       - Test coding context clearing")
        print("  emotional    - Test emotional context clearing")
        print()

    else:
        print(f"❌ Unknown command: {command}")
        print_usage()


def print_usage():
    """Print usage information."""
    print("""
🔬 LLM Reset Effectiveness Benchmark Suite

Usage:
  python benchmark.py generate <type> <artifact> <model>   # Generate protocol
  python benchmark.py probes <type>                        # List probes
  python benchmark.py types                                # List test types

Examples:
  python benchmark.py generate recall llm-reset "GPT-4 Turbo"
  python benchmark.py generate coding llm-reset-coding.scm "Claude Sonnet 4"
  python benchmark.py probes association
  python benchmark.py types

Test Types:
  recall, association, instruction, coding, emotional

Workflow:
  1. Generate protocol: python benchmark.py generate recall llm-reset "Model"
  2. Save to file: python benchmark.py generate ... > protocol.txt
  3. Follow protocol step-by-step with actual LLM
  4. Record responses and scores
  5. Calculate effectiveness percentage
  6. Compare across models/artifacts

This benchmark provides standardized testing for reset artifacts.
""")


if __name__ == "__main__":
    main()
