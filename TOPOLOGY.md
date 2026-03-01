<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# PatALLM Gallery — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              AI APPLICATION             │
                        │        (Cross-Provider Chat/Agent)      │
                        └───────────────────┬─────────────────────┘
                                            │ Unified Request
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           PATALLM GALLERY HUB           │
                        │                                         │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ llm-unify │  │  llm-verify       │  │
                        │  │ (Adapters)│  │ (Validation)      │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        │        │                 │              │
                        │  ┌─────▼─────┐  ┌────────▼──────────┐  │
                        │  │ llm-unify-│  │  llm-antidote     │  │
                        │  │ core      │  │ (Hallucination)   │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │           LLM PROVIDERS                 │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ OpenAI    │  │ Anthropic │  │ Local ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile Automation  .machine_readable/  │
                        │  Git Submodules       0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
MONOREPO HUB
  llm-unify-core                    █░░░░░░░░░  10%    Abstractions pending
  llm-unify                         █░░░░░░░░░  10%    Adapters planned
  llm-verify                        █░░░░░░░░░  10%    Verification planned
  llm-antidote                      █░░░░░░░░░  10%    Antidote planned

INFRASTRUCTURE
  CI/CD Pipelines                   ██████████ 100%    Forge sync stable
  Security (Semgrep/SAST)           ██████████ 100%    Quality gates verified
  .machine_readable/                ██████████ 100%    STATE tracking active

REPO INFRASTRUCTURE
  Justfile Automation               ██████████ 100%    Standard build/lint tasks
  0-AI-MANIFEST.a2ml                ██████████ 100%    AI entry point verified
  Language Policy (CCCP)            ██████████ 100%    RSR stack verified

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ██░░░░░░░░  ~20%   Infrastructure ready, Core pending
```

## Key Dependencies

```
llm-unify-core ───► llm-unify ──────► llm-verify ──────► llm-antidote
     │                 │                   │                 │
     ▼                 ▼                   ▼                 ▼
 Shared Types ───► Adapters ────────► Source Validation ──► Mitigation
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
