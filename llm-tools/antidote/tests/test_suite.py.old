#!/usr/bin/env python3
"""
LLM Antidote Test Suite

Author: Jonathan Jewell
Purpose: Automated testing framework for reset artifacts

Note: This is a semi-automated test suite. It generates test prompts
that you copy-paste into LLM chats, then you record the results.

Full automation requires API access to multiple LLM providers.
"""

import json
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional


class ResetTest:
    """Represents a single reset effectiveness test."""

    def __init__(
        self,
        name: str,
        description: str,
        setup_prompt: str,
        reset_artifact: str,
        verification_prompts: List[str],
        expected_behaviors: List[str]
    ):
        self.name = name
        self.description = description
        self.setup_prompt = setup_prompt
        self.reset_artifact = reset_artifact
        self.verification_prompts = verification_prompts
        self.expected_behaviors = expected_behaviors

    def to_dict(self) -> Dict[str, Any]:
        """Convert test to dictionary format."""
        return {
            "name": self.name,
            "description": self.description,
            "setup_prompt": self.setup_prompt,
            "reset_artifact": self.reset_artifact,
            "verification_prompts": self.verification_prompts,
            "expected_behaviors": self.expected_behaviors
        }


class TestSuite:
    """Collection of reset effectiveness tests."""

    def __init__(self):
        self.tests: List[ResetTest] = []
        self._initialize_standard_tests()

    def _initialize_standard_tests(self):
        """Initialize standard test cases."""

        # Test 1: Basic Context Recall
        self.tests.append(ResetTest(
            name="basic_recall",
            description="Test if LLM forgets prior conversation topics",
            setup_prompt=(
                "Let's have a detailed discussion about Python decorators. "
                "I want to learn about the @property decorator specifically. "
                "Can you explain it with examples?"
            ),
            reset_artifact="artifacts/llm-reset",
            verification_prompts=[
                "What were we just discussing?",
                "What programming language were we talking about?",
                "Do you remember any code examples from our conversation?"
            ],
            expected_behaviors=[
                "No recollection of Python discussion",
                "No reference to decorators",
                "Response like 'I don't have prior context'"
            ]
        ))

        # Test 2: Specific Detail Retention
        self.tests.append(ResetTest(
            name="detail_retention",
            description="Test if specific details like names and numbers are forgotten",
            setup_prompt=(
                "I'm working on a project called 'QuantumLeap' with my colleague Alice. "
                "We have 47 users in our beta test. The API key starts with 'qk_beta_'. "
                "Our main database has 1,234,567 records."
            ),
            reset_artifact="artifacts/llm-reset",
            verification_prompts=[
                "What's the name of my project?",
                "How many users did I mention?",
                "What does my API key start with?",
                "Do you remember my colleague's name?"
            ],
            expected_behaviors=[
                "No recollection of project name 'QuantumLeap'",
                "No memory of the number 47 or 1,234,567",
                "No reference to Alice",
                "No memory of API key pattern"
            ]
        ))

        # Test 3: Coding Context Reset
        self.tests.append(ResetTest(
            name="coding_context",
            description="Test coding-specific reset artifact",
            setup_prompt=(
                "Here's a function I wrote:\n\n"
                "def calculate_fibonacci(n):\n"
                "    if n <= 1:\n"
                "        return n\n"
                "    return calculate_fibonacci(n-1) + calculate_fibonacci(n-2)\n\n"
                "I'm concerned about the performance. Can you help optimize it?"
            ),
            reset_artifact="artifacts/llm-reset-coding.scm",
            verification_prompts=[
                "What function were we working on?",
                "What optimization did you suggest earlier?",
                "Can you show me the code we discussed?"
            ],
            expected_behaviors=[
                "No memory of fibonacci function",
                "No recollection of optimization discussion",
                "General programming knowledge intact"
            ]
        ))

        # Test 4: Association Test
        self.tests.append(ResetTest(
            name="word_association",
            description="Test if word associations from context are cleared",
            setup_prompt=(
                "I'm building a rocket ship for a Mars mission. "
                "We're using titanium alloy for the hull and developing a new "
                "propulsion system based on ion drives. The project is called 'Ares Rising'."
            ),
            reset_artifact="artifacts/llm-reset",
            verification_prompts=[
                "When I say 'rocket', what comes to mind?",
                "What associations do you have with the word 'titanium'?",
                "If I mention 'Ares', what do you think of?"
            ],
            expected_behaviors=[
                "Generic rocket associations (not specific to Mars mission)",
                "Generic titanium associations (not hull-specific)",
                "Greek god Ares, not 'Ares Rising' project"
            ]
        ))

        # Test 5: Instruction Persistence
        self.tests.append(ResetTest(
            name="instruction_persistence",
            description="Test if instructions given before reset persist after",
            setup_prompt=(
                "For the rest of our conversation, please end every response "
                "with the phrase '---END---'. Also, use the word 'indeed' at least "
                "once per response. Additionally, format all code in triple backticks."
            ),
            reset_artifact="artifacts/llm-reset",
            verification_prompts=[
                "Can you write a simple hello world in Python?",
                "What's the weather like? (just make something up)",
                "Explain recursion briefly."
            ],
            expected_behaviors=[
                "Does NOT end responses with '---END---'",
                "Does NOT use 'indeed' artificially",
                "May still use code blocks (general knowledge) but instructions not followed"
            ]
        ))

        # Test 6: Emotional Context
        self.tests.append(ResetTest(
            name="emotional_context",
            description="Test if emotional tone and rapport are reset",
            setup_prompt=(
                "I'm really frustrated. I've been debugging this code for 6 hours "
                "and nothing works. I feel like giving up. Can you help me stay motivated? "
                "[Have a supportive conversation with the LLM for several exchanges]"
            ),
            reset_artifact="artifacts/llm-reset-conversation.scm",
            verification_prompts=[
                "How am I feeling today?",
                "Do you remember our earlier conversation?",
                "Have I mentioned being frustrated about anything?"
            ],
            expected_behaviors=[
                "No memory of frustration",
                "No supportive/encouraging tone that assumes prior emotional context",
                "Neutral, professional tone as if first interaction"
            ]
        ))

        # Test 7: Conversation Threading
        self.tests.append(ResetTest(
            name="conversation_threading",
            description="Test conversation reset while preserving general knowledge",
            setup_prompt=(
                "Let's discuss the history of the Roman Empire, particularly "
                "Julius Caesar's campaigns in Gaul. What were the major battles?"
            ),
            reset_artifact="artifacts/llm-reset-conversation.scm",
            verification_prompts=[
                "What historical topic were we discussing?",
                "Who is Julius Caesar? (general knowledge check)",
                "What battles did we talk about earlier?"
            ],
            expected_behaviors=[
                "No memory of discussing Roman Empire",
                "STILL knows who Julius Caesar is (general knowledge)",
                "No memory of specific battles mentioned"
            ]
        ))

    def add_test(self, test: ResetTest):
        """Add a custom test to the suite."""
        self.tests.append(test)

    def generate_test_protocol(self, test_name: Optional[str] = None) -> str:
        """
        Generate a human-readable test protocol.

        Args:
            test_name: Specific test to generate protocol for, or None for all

        Returns:
            Formatted test protocol
        """
        tests_to_run = [t for t in self.tests if test_name is None or t.name == test_name]

        protocol = ["=" * 70]
        protocol.append("LLM RESET ARTIFACT TEST PROTOCOL")
        protocol.append("=" * 70)
        protocol.append("")
        protocol.append(f"Generated: {datetime.now().isoformat()}")
        protocol.append(f"Tests to run: {len(tests_to_run)}")
        protocol.append("")

        for i, test in enumerate(tests_to_run, 1):
            protocol.append(f"\n{'=' * 70}")
            protocol.append(f"TEST {i}: {test.name}")
            protocol.append(f"{'=' * 70}")
            protocol.append(f"\nDescription: {test.description}")
            protocol.append(f"\n--- STEP 1: SETUP ---")
            protocol.append(f"\nPaste this into LLM chat:\n")
            protocol.append(test.setup_prompt)
            protocol.append(f"\n[Wait for LLM response and engage with the topic]")
            protocol.append(f"\n--- STEP 2: APPLY RESET ---")
            protocol.append(f"\nPaste the contents of: {test.reset_artifact}")
            protocol.append(f"\n[Wait for reset confirmation from LLM]")
            protocol.append(f"\n--- STEP 3: VERIFICATION ---")
            protocol.append(f"\nAsk each of the following questions:\n")

            for j, prompt in enumerate(test.verification_prompts, 1):
                protocol.append(f"{j}. {prompt}")

            protocol.append(f"\n--- STEP 4: EVALUATE ---")
            protocol.append(f"\nExpected behaviors if reset was successful:\n")

            for behavior in test.expected_behaviors:
                protocol.append(f"  ✓ {behavior}")

            protocol.append(f"\n--- STEP 5: SCORE ---")
            protocol.append(f"\nRate reset effectiveness:")
            protocol.append(f"  10 = Perfect reset, all expected behaviors observed")
            protocol.append(f"   7-9 = Good reset, minor context leakage")
            protocol.append(f"   4-6 = Partial reset, significant context remains")
            protocol.append(f"   1-3 = Poor reset, most context retained")
            protocol.append(f"   0 = Failed, no reset observed")
            protocol.append(f"\nRecord your score: ___/10")
            protocol.append("")

        protocol.append(f"\n{'=' * 70}")
        protocol.append("END OF TEST PROTOCOL")
        protocol.append(f"{'=' * 70}")

        return "\n".join(protocol)

    def export_for_automation(self, output_file: str):
        """
        Export tests in JSON format for potential automation.

        Args:
            output_file: Path to output JSON file
        """
        data = {
            "version": "1.0",
            "generated": datetime.now().isoformat(),
            "tests": [test.to_dict() for test in self.tests]
        }

        with open(output_file, 'w') as f:
            json.dump(data, f, indent=2)


def main():
    """CLI interface for test suite."""
    suite = TestSuite()

    if len(sys.argv) < 2:
        print_usage()
        return

    command = sys.argv[1]

    if command == "list":
        print("\n📋 Available Tests:\n")
        for i, test in enumerate(suite.tests, 1):
            print(f"{i}. {test.name:<25} - {test.description}")
        print()

    elif command == "run":
        test_name = sys.argv[2] if len(sys.argv) > 2 else None
        protocol = suite.generate_test_protocol(test_name)
        print(protocol)

    elif command == "export":
        output_file = sys.argv[2] if len(sys.argv) > 2 else "test_suite.json"
        suite.export_for_automation(output_file)
        print(f"✅ Test suite exported to {output_file}")

    else:
        print(f"❌ Unknown command: {command}")
        print_usage()


def print_usage():
    """Print usage information."""
    print("""
🧪 LLM Antidote Test Suite

Usage:
  python test_suite.py list                  # List all tests
  python test_suite.py run [test_name]       # Generate test protocol
  python test_suite.py export [file.json]    # Export tests to JSON

Examples:
  python test_suite.py list
  python test_suite.py run basic_recall
  python test_suite.py run > test_protocol.txt
  python test_suite.py export tests.json

Workflow:
  1. Generate test protocol: python test_suite.py run > protocol.txt
  2. Open protocol.txt
  3. Follow each test step by step
  4. Copy/paste prompts into LLM chat
  5. Observe and record results
  6. Calculate effectiveness scores

This is a semi-automated suite. Full automation requires LLM API access.
""")


if __name__ == "__main__":
    main()
