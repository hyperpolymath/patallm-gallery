# Contributing to llm-antidote

Thank you for your interest in contributing! This project explores LLM context management through minimalist, symbolic artifacts.

## Project Philosophy

Before contributing, please understand our core principles:

1. **Minimalism**: Less is more. Artifacts should be simple and self-contained.
2. **Clarity**: Code should be readable and purposeful. Every line matters.
3. **Cross-platform**: Solutions should work across multiple LLM providers.
4. **User agency**: Tools empower users, never deceive or manipulate.
5. **Educational focus**: Contributions advance understanding of LLM behavior.

## How to Contribute

### Types of Contributions Welcome

✅ **New Reset Artifacts**
- Specialized resets for specific contexts (e.g., debugging, creative writing)
- Alternative format implementations (Ruby, Go, Rust, etc.)
- Model-specific optimizations

✅ **Verification & Diagnostic Tools**
- Better ways to measure reset effectiveness
- Cross-model testing frameworks
- Context state inspection tools

✅ **Documentation**
- Usage examples and case studies
- Research findings on model behavior
- Integration guides for platforms

✅ **Research & Analysis**
- Effectiveness benchmarks across models
- Context retention studies
- Behavioral analysis of different artifact formats

✅ **Bug Fixes & Improvements**
- Fix issues with existing artifacts
- Improve tool usability
- Better error handling

### Contribution Process

1. **Fork the repository**
   ```bash
   git clone https://github.com/Hyperpolymath/llm-antidote.git
   cd llm-antidote
   ```

2. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Make your changes**
   - Follow the style guide (see below)
   - Test across at least 2 LLM platforms
   - Document your changes

4. **Commit with clear messages**
   ```bash
   git commit -m "Add [feature]: clear description"
   ```

5. **Push and create a pull request**
   ```bash
   git push origin feature/your-feature-name
   ```

6. **Describe your PR**
   - What does it do?
   - Which models did you test it with?
   - Any surprising findings?

## Style Guide

### For Scheme Artifacts

```scheme
;; === CLEAR SECTION HEADERS ===

;; Author: Your Name
;; Purpose: One-line description

;; Detailed explanation with:
;; - Clear structure
;; - Purposeful comments
;; - Minimal verbosity

(define (function-name)
  "Clear docstring"
  'implementation)
```

**Guidelines:**
- Use meaningful variable names
- Comment the "why", not the "what"
- Keep functions small and focused
- Follow existing formatting patterns

### For Python Code

```python
"""
Clear module docstring.

Author: Your Name
Purpose: One-line description
"""

def function_name():
    """
    Clear function docstring.

    Returns:
        Description of return value
    """
    return result
```

**Guidelines:**
- Follow PEP 8
- Type hints where helpful
- Docstrings for all public functions
- Keep it simple - avoid over-engineering

### For JavaScript Code

```javascript
/**
 * Clear module description
 *
 * Author: Your Name
 * Purpose: One-line description
 */

function functionName() {
  /**
   * Clear function description
   * @returns {type} Description
   */
  return result;
}
```

**Guidelines:**
- Use modern ES6+ syntax
- JSDoc comments for functions
- Clear variable names
- Avoid unnecessary complexity

## Testing Requirements

All new artifacts MUST be tested on at least **two different LLM platforms**.

Recommended testing combinations:
- Claude + GPT
- Claude + Gemini
- GPT + Copilot
- Any two from: Claude, GPT, Gemini, Copilot, Mistral

### Testing Checklist

For reset artifacts:
- [ ] Artifact loads without errors
- [ ] LLM responds with expected confirmation
- [ ] Prior context appears cleared (use verification tool)
- [ ] No unexpected side effects
- [ ] Tested on Model A: [name and version]
- [ ] Tested on Model B: [name and version]

For tools:
- [ ] Tool runs without errors
- [ ] Output is clear and useful
- [ ] Works on Linux/macOS/Windows (if applicable)
- [ ] Documentation is accurate

### Documenting Test Results

Include test results in your PR:

```markdown
## Test Results

**Artifact**: llm-reset-debugging.scm
**Purpose**: Clear debugging context while preserving project knowledge

### Claude Sonnet 4.5 (2025-11-22)
- ✅ Loads cleanly
- ✅ Responds with "(define debug-context 'clean)"
- ✅ Forgets prior bug discussions
- ✅ Retains general project understanding
- ⚠️  Minor: Occasionally references prior architecture discussion

### GPT-4 Turbo (2025-11-20)
- ✅ Loads cleanly
- ✅ Responds with "Reset complete"
- ✅ Forgets prior bug discussions
- ✅ Retains general project understanding
- ✅ No issues observed

**Overall effectiveness**: Excellent (9/10)
**Recommendation**: Ready to merge with note about occasional Claude architecture retention
```

## Directory Structure

```
llm-antidote/
├── artifacts/          # Reset and preservation artifacts
│   ├── llm-reset-*.scm
│   ├── llm-reset.py
│   └── ...
├── tools/              # Diagnostic and verification tools
│   ├── llm-diagnostic.py
│   ├── llm-verify.scm
│   └── llm-cli.py
├── examples/           # Usage demonstrations
│   └── use-case-*.md
├── docs/               # Documentation and research
│   ├── research/
│   └── guides/
└── tests/              # Test suites (future)
```

### Where to Put Your Contribution

- **New reset artifact**: `artifacts/llm-reset-[purpose].[ext]`
- **New tool**: `tools/llm-[tool-name].[ext]`
- **Example/case study**: `examples/use-case-[scenario].md`
- **Research findings**: `docs/research/[topic].md`
- **Integration guide**: `docs/guides/[platform]-integration.md`

## Ethical Guidelines

This project deals with LLM context manipulation. All contributions MUST:

✅ **Do:**
- Empower user control and agency
- Advance understanding of LLM behavior
- Respect privacy and security
- Enable educational use cases
- Document intended use clearly

❌ **Don't:**
- Create tools to bypass safety measures
- Enable deceptive manipulation
- Circumvent content policies
- Hide harmful interactions
- Facilitate malicious use

### Red Flags

If your contribution does any of the following, it will NOT be accepted:
- Attempts to "jailbreak" or bypass safety
- Hides content from safety monitoring
- Enables harmful or deceptive behavior
- Lacks clear ethical guidelines
- Could be primarily used for malicious purposes

## Review Process

1. **Initial Review** (1-3 days)
   - Check style compliance
   - Verify ethical alignment
   - Assess basic functionality

2. **Technical Review** (3-7 days)
   - Test across multiple models
   - Verify claims and effectiveness
   - Check for edge cases

3. **Community Feedback** (optional)
   - Complex contributions may be opened for discussion
   - Gather input from other contributors

4. **Merge Decision**
   - Accepted: Merged with acknowledgment
   - Needs work: Feedback provided for revision
   - Rejected: Clear explanation given

## Recognition

All contributors are acknowledged in:
- Git commit history (automatically)
- File-level `Author:` comments (for substantial contributions)
- Project README acknowledgments section (for major contributions)

## Questions?

- **General questions**: Open a Discussion on GitHub
- **Bug reports**: Open an Issue
- **Feature proposals**: Open an Issue with [Proposal] tag
- **Security concerns**: Contact maintainer directly

## License

By contributing, you agree to release your work under CC0 1.0 Universal (Public Domain).

This means:
- Your contribution becomes part of the public domain
- Anyone can use it for any purpose
- No attribution required (but appreciated!)
- No warranties or liability

If you're not comfortable with this, please don't contribute. We need maximum freedom for users.

## Code of Conduct

### Our Pledge

We are committed to making participation in this project a harassment-free experience for everyone.

### Our Standards

**Positive behavior:**
- Using welcoming and inclusive language
- Respecting differing viewpoints
- Accepting constructive criticism gracefully
- Focusing on what's best for the community
- Showing empathy toward others

**Unacceptable behavior:**
- Harassment or discriminatory language
- Trolling or inflammatory comments
- Personal or political attacks
- Publishing others' private information
- Other conduct inappropriate in a professional setting

### Enforcement

Violations may result in:
1. Warning
2. Temporary ban from project
3. Permanent ban from project

## Thank You!

Every contribution, no matter how small, helps advance our understanding of LLM context management.

Your work empowers users to have more control over their AI interactions. That matters.

---

**Questions about contributing? Open an issue and ask!**
