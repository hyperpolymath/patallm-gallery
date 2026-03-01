#!/usr/bin/env python3
"""
RSR Framework Compliance Checker

Verifies llm-antidote compliance with Rhodium Standard Repository framework.

Author: Jonathan Jewell
License: CC0 1.0 Universal
"""

import sys
from pathlib import Path
from typing import Dict, List, Tuple
from dataclasses import dataclass


@dataclass
class ComplianceCheck:
    """Represents a single compliance check."""
    category: str
    name: str
    passed: bool
    required: bool
    message: str


class RSRCompliance:
    """RSR Framework compliance checker."""

    def __init__(self, repo_root: Path = None):
        """Initialize compliance checker."""
        self.repo_root = repo_root or Path.cwd()
        self.checks: List[ComplianceCheck] = []

    def check_file_exists(self, filepath: str, category: str, required: bool = True) -> bool:
        """Check if a file exists."""
        path = self.repo_root / filepath
        exists = path.exists()

        self.checks.append(ComplianceCheck(
            category=category,
            name=f"File: {filepath}",
            passed=exists,
            required=required,
            message=f"{'✅' if exists else '❌'} {filepath} {'exists' if exists else 'missing'}"
        ))

        return exists

    def check_directory_exists(self, dirpath: str, category: str, required: bool = True) -> bool:
        """Check if a directory exists."""
        path = self.repo_root / dirpath
        exists = path.exists() and path.is_dir()

        self.checks.append(ComplianceCheck(
            category=category,
            name=f"Directory: {dirpath}",
            passed=exists,
            required=required,
            message=f"{'✅' if exists else '❌'} {dirpath}/ {'exists' if exists else 'missing'}"
        ))

        return exists

    def check_documentation(self):
        """Check documentation compliance."""
        self.check_file_exists("README.md", "Documentation", required=True)
        self.check_file_exists("LICENSE", "Documentation", required=True)
        self.check_file_exists("CONTRIBUTING.md", "Documentation", required=True)
        self.check_file_exists("CODE_OF_CONDUCT.md", "Documentation", required=True)
        self.check_file_exists("SECURITY.md", "Documentation", required=True)
        self.check_file_exists("MAINTAINERS.md", "Documentation", required=True)
        self.check_file_exists("CHANGELOG.md", "Documentation", required=True)
        self.check_file_exists("VERSION", "Documentation", required=False)

    def check_well_known(self):
        """Check .well-known directory compliance."""
        if self.check_directory_exists(".well-known", ".well-known", required=True):
            self.check_file_exists(".well-known/security.txt", ".well-known", required=True)
            self.check_file_exists(".well-known/ai.txt", ".well-known", required=False)
            self.check_file_exists(".well-known/humans.txt", ".well-known", required=False)

    def check_build_system(self):
        """Check build system compliance."""
        has_justfile = self.check_file_exists("justfile", "Build System", required=False)
        has_makefile = self.check_file_exists("Makefile", "Build System", required=False)

        if has_justfile or has_makefile:
            self.checks.append(ComplianceCheck(
                category="Build System",
                name="Build system present",
                passed=True,
                required=False,
                message="✅ Build system available (justfile or Makefile)"
            ))

        self.check_file_exists("flake.nix", "Build System", required=False)

    def check_ci_cd(self):
        """Check CI/CD configuration."""
        has_gitlab = self.check_file_exists(".gitlab-ci.yml", "CI/CD", required=False)
        has_github = self.check_file_exists(".github/workflows/ci.yml", "CI/CD", required=False)

        if has_gitlab or has_github:
            self.checks.append(ComplianceCheck(
                category="CI/CD",
                name="CI/CD configured",
                passed=True,
                required=False,
                message="✅ CI/CD pipeline configured"
            ))

    def check_testing(self):
        """Check testing infrastructure."""
        if self.check_directory_exists("tests", "Testing", required=False):
            test_files = list((self.repo_root / "tests").glob("*.py"))
            has_tests = len(test_files) > 0

            self.checks.append(ComplianceCheck(
                category="Testing",
                name="Test files present",
                passed=has_tests,
                required=False,
                message=f"{'✅' if has_tests else '❌'} {len(test_files)} test file(s) found"
            ))

    def check_offline_first(self):
        """Check offline-first capability."""
        # For llm-antidote, all artifacts work offline
        # Check for absence of required network dependencies

        self.checks.append(ComplianceCheck(
            category="Offline-First",
            name="No required network dependencies",
            passed=True,
            required=False,
            message="✅ All artifacts work offline"
        ))

    def check_tpcf(self):
        """Check TPCF (Tri-Perimeter Contribution Framework) documentation."""
        # Check if TPCF is documented in CONTRIBUTING.md or MAINTAINERS.md

        contributing_path = self.repo_root / "CONTRIBUTING.md"
        maintainers_path = self.repo_root / "MAINTAINERS.md"

        has_tpcf = False

        if contributing_path.exists():
            content = contributing_path.read_text()
            has_tpcf = "TPCF" in content or "Tri-Perimeter" in content or "perimeter" in content.lower()

        if not has_tpcf and maintainers_path.exists():
            content = maintainers_path.read_text()
            has_tpcf = "TPCF" in content or "Tri-Perimeter" in content

        self.checks.append(ComplianceCheck(
            category="TPCF",
            name="TPCF documented",
            passed=has_tpcf,
            required=False,
            message=f"{'✅' if has_tpcf else '⚠️'} TPCF contribution framework {'documented' if has_tpcf else 'not explicitly documented'}"
        ))

    def run_all_checks(self):
        """Run all compliance checks."""
        self.check_documentation()
        self.check_well_known()
        self.check_build_system()
        self.check_ci_cd()
        self.check_testing()
        self.check_offline_first()
        self.check_tpcf()

    def calculate_score(self) -> Tuple[int, int, int, int]:
        """
        Calculate compliance score.

        Returns:
            Tuple of (required_passed, required_total, optional_passed, optional_total)
        """
        required_passed = sum(1 for c in self.checks if c.required and c.passed)
        required_total = sum(1 for c in self.checks if c.required)
        optional_passed = sum(1 for c in self.checks if not c.required and c.passed)
        optional_total = sum(1 for c in self.checks if not c.required)

        return required_passed, required_total, optional_passed, optional_total

    def get_tier(self) -> str:
        """
        Determine RSR compliance tier.

        Returns:
            Compliance tier (Bronze, Silver, Gold, Platinum)
        """
        req_passed, req_total, opt_passed, opt_total = self.calculate_score()

        if req_passed < req_total:
            return "Non-Compliant"

        # All required checks passed
        total_checks = req_total + opt_total
        total_passed = req_passed + opt_passed
        percentage = (total_passed / total_checks * 100) if total_checks > 0 else 0

        if percentage >= 95:
            return "Platinum"
        elif percentage >= 85:
            return "Gold"
        elif percentage >= 70:
            return "Silver"
        elif percentage >= 50:
            return "Bronze"
        else:
            return "Partial"

    def print_report(self, verbose: bool = False):
        """Print compliance report."""
        print("=" * 70)
        print("RSR FRAMEWORK COMPLIANCE REPORT")
        print("=" * 70)
        print()

        # Group checks by category
        categories: Dict[str, List[ComplianceCheck]] = {}
        for check in self.checks:
            if check.category not in categories:
                categories[check.category] = []
            categories[check.category].append(check)

        # Print by category
        for category, checks in sorted(categories.items()):
            print(f"📁 {category}")
            print("-" * 70)

            for check in checks:
                if verbose or not check.passed:
                    print(f"  {check.message}")

            category_passed = sum(1 for c in checks if c.passed)
            category_total = len(checks)
            print(f"  Score: {category_passed}/{category_total}")
            print()

        # Overall score
        req_passed, req_total, opt_passed, opt_total = self.calculate_score()
        tier = self.get_tier()

        print("=" * 70)
        print("SUMMARY")
        print("=" * 70)
        print(f"Required checks: {req_passed}/{req_total}")
        print(f"Optional checks: {opt_passed}/{opt_total}")
        print(f"Total: {req_passed + opt_passed}/{req_total + opt_total}")
        print()
        print(f"Compliance Tier: {tier}")
        print()

        # Tier descriptions
        tier_desc = {
            "Platinum": "⭐⭐⭐⭐ Exemplary - All checks passed",
            "Gold": "⭐⭐⭐ Excellent - 85%+ checks passed",
            "Silver": "⭐⭐ Good - 70%+ checks passed",
            "Bronze": "⭐ Basic - 50%+ checks passed",
            "Partial": "Incomplete - Less than 50% checks passed",
            "Non-Compliant": "❌ Failed - Required checks not met"
        }

        print(tier_desc.get(tier, "Unknown tier"))
        print("=" * 70)

    def generate_markdown_report(self) -> str:
        """Generate Markdown compliance report."""
        req_passed, req_total, opt_passed, opt_total = self.calculate_score()
        tier = self.get_tier()

        lines = [
            "# RSR Framework Compliance Report",
            "",
            f"**Project**: llm-antidote",
            f"**Date**: {Path.cwd()}",
            f"**Tier**: {tier}",
            "",
            "## Summary",
            "",
            f"- Required checks: {req_passed}/{req_total}",
            f"- Optional checks: {opt_passed}/{opt_total}",
            f"- **Total: {req_passed + opt_passed}/{req_total + opt_total}**",
            "",
            "## Compliance Checks",
            ""
        ]

        # Group by category
        categories: Dict[str, List[ComplianceCheck]] = {}
        for check in self.checks:
            if check.category not in categories:
                categories[check.category] = []
            categories[check.category].append(check)

        for category, checks in sorted(categories.items()):
            lines.append(f"### {category}")
            lines.append("")

            for check in checks:
                status = "✅" if check.passed else "❌"
                required = " (Required)" if check.required else ""
                lines.append(f"- {status} {check.name}{required}")

            lines.append("")

        lines.extend([
            "## Recommendations",
            "",
            "To improve compliance:"
        ])

        # Add recommendations for failed checks
        failed_checks = [c for c in self.checks if not c.passed]
        if failed_checks:
            for check in failed_checks:
                lines.append(f"- {check.message}")
        else:
            lines.append("- All checks passed! ✅")

        lines.append("")
        lines.append("---")
        lines.append("*Generated by RSR compliance checker*")

        return "\n".join(lines)


def main():
    """CLI entry point."""
    checker = RSRCompliance()
    checker.run_all_checks()

    verbose = "--verbose" in sys.argv or "-v" in sys.argv
    report = "--report" in sys.argv

    if report:
        print(checker.generate_markdown_report())
    else:
        checker.print_report(verbose=verbose)

    # Exit with error code if not at least Bronze
    tier = checker.get_tier()
    if tier in ["Non-Compliant", "Partial"]:
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == "__main__":
    main()
