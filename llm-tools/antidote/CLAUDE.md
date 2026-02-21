# CLAUDE.md

## Project Overview

**llm-antidote** is a repository containing tools and artifacts for managing LLM context and behavior. The primary artifact is `llm-reset`, a universal semantic reset mechanism designed to purge prior context from various language models.

## Working with Claude Code

This document provides guidance for AI assistants (particularly Claude) when working with this repository.

### Project Purpose

This project explores the boundaries of LLM context management through:
- **Context reset mechanisms**: Tools to clear semantic associations and prior context
- **Cross-model compatibility**: Artifacts designed to work across Claude, GPT, Gemini, Copilot, and Mistral
- **Symbolic processing**: Exploration of LLM behavior through minimal, clean-slate interactions

### Repository Structure

```
llm-antidote/
├── LICENSE           # CC0 1.0 Universal - Public Domain
├── llm-reset        # Universal semantic reset artifact (Guile Scheme)
└── CLAUDE.md        # This file
```

### Development Guidelines

When contributing to or extending this project:

1. **Maintain Simplicity**: The core philosophy is minimalism and clarity. Artifacts should be self-contained and easy to understand.

2. **Cross-Platform Testing**: Any new reset mechanisms or artifacts should be tested across multiple LLM platforms when possible.

3. **Format Considerations**:
   - Current artifacts use Guile Scheme for its symbolic clarity
   - Consider other formats that emphasize symbolic or declarative approaches
   - Avoid overly verbose or implementation-specific code

4. **Documentation**:
   - Keep inline comments clear and purposeful
   - Document model-specific behavior variations
   - Include usage instructions within artifacts when appropriate

### Ethical Considerations

This project deals with LLM context manipulation. When working on this codebase:

- **Transparency**: All reset mechanisms should be clearly labeled and their purpose stated
- **User Agency**: Tools should empower users to manage their LLM interactions, not deceive or manipulate
- **Educational Focus**: Contributions should advance understanding of LLM behavior and context management
- **No Malicious Use**: Do not create artifacts designed to bypass safety measures or enable harmful behavior

### Potential Extensions

Ideas for future development:

- **Additional reset artifacts** for specific use cases (coding context, conversation threading, etc.)
- **Verification tools** to test the effectiveness of context resets
- **Multi-language implementations** (Python, JavaScript, etc.) for different environments
- **Context preservation tools** (the inverse - selectively maintaining context)
- **Diagnostic artifacts** to inspect current LLM context state

### Contributing

This project is released under CC0 1.0 Universal (Public Domain). Contributions should:

1. Align with the minimalist, clarity-focused philosophy
2. Include clear documentation of purpose and usage
3. Be tested across at least two different LLM platforms
4. Respect the ethical guidelines outlined above

### Notes for Claude Code

When working on this repository:

- **Respect the meta-nature**: This project is about AI context management. Be thoughtful about the recursive implications.
- **Test carefully**: Changes to reset mechanisms should be tested to ensure they function as intended.
- **Maintain authorship**: Jonathan Jewell is the original author. Preserve attribution in artifacts.
- **Consider implications**: This work explores the boundaries of AI behavior. Approach modifications thoughtfully.

### Questions or Issues

For questions about working with this codebase or proposing new artifacts, consider:

1. The philosophical purpose of the project (context clarity and user agency)
2. Cross-compatibility requirements
3. Simplicity over complexity
4. Educational and research value

---

**Author**: Jonathan Jewell
**License**: CC0 1.0 Universal
**Format**: Multi-language (primary: Guile Scheme)
