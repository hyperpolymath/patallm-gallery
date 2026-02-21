# Maintainers

This document describes the governance model and maintainer responsibilities for LLM Unify.

## Current Maintainers

| Name | GitHub | Role | Focus Area |
|------|--------|------|------------|
| *To be filled* | @username | Lead Maintainer | Overall direction, releases |
| *To be filled* | @username | Core Maintainer | Storage, parsing |
| *To be filled* | @username | Core Maintainer | TUI, CLI |

## Governance Model

LLM Unify follows a **consensus-seeking decision-making model** with voting fallback.

### Consensus-Seeking

1. **Propose** - Maintainer or contributor proposes a change
2. **Discuss** - Community discusses in GitHub Discussions or Issues
3. **Iterate** - Proposer refines based on feedback
4. **Consensus** - If no blocking objections after 7 days, proposal accepted

### Voting Fallback

If consensus cannot be reached:

1. **Vote Called** - Any maintainer can call for a vote
2. **Voting Period** - 7 days for maintainers to vote
3. **Simple Majority** - >50% of maintainer votes required
4. **Lead Tie-Break** - Lead maintainer breaks ties

### Veto Power

**No individual veto power.** All decisions require consensus or majority vote.

## Maintainer Responsibilities

### Code Review

- Review pull requests within 7 days
- Provide constructive, actionable feedback
- Ensure code meets quality standards
- Check for RSR compliance

### Release Management

- Coordinate release cycles
- Update CHANGELOG.md
- Tag releases following semantic versioning
- Publish to crates.io

### Community Engagement

- Respond to issues and discussions
- Welcome new contributors
- Mentor contributors toward maintainership
- Enforce Code of Conduct

### Security

- Respond to security reports per SECURITY.md
- Coordinate security patches
- Maintain security audit schedule

### Documentation

- Keep documentation current
- Review documentation PRs
- Ensure RSR compliance

## Decision-Making Authority

### Requires Consensus/Vote

- Major architectural changes
- Breaking API changes
- License changes
- Governance changes
- Adding/removing maintainers

### Maintainer Discretion

- Bug fixes
- Documentation updates
- Dependency updates (minor/patch)
- Code style improvements

### Community Input Required

- Feature additions
- Deprecations
- Roadmap planning

## Adding Maintainers

### Nomination

1. **Nominator** - Existing maintainer nominates contributor
2. **Rationale** - Provide contribution history and justification
3. **Discussion** - Maintainers discuss privately
4. **Vote** - Consensus or majority vote required
5. **Invitation** - Lead maintainer extends invitation
6. **Onboarding** - New maintainer added to GitHub org, docs updated

### Criteria

- **Sustained contributions** - 10+ merged PRs over 3+ months
- **Code quality** - Demonstrates high standards
- **Community engagement** - Helpful in discussions, reviews
- **Judgment** - Makes sound technical decisions
- **Alignment** - Shares project values and philosophy
- **Commitment** - Willing to dedicate time long-term

## Removing Maintainers

### Voluntary

- Maintainer can step down anytime
- Transition period for open responsibilities
- Recognition as Emeritus Maintainer

### Involuntary

Removal for:
- Code of Conduct violations
- Prolonged inactivity (6+ months without response)
- Breach of trust or security

**Process:**
1. Private discussion among maintainers
2. Attempt to resolve concerns
3. Consensus/vote required for removal
4. Private notification to affected maintainer
5. Public announcement (brief, respectful)

## Emeritus Maintainers

Past maintainers who stepped down in good standing:

| Name | GitHub | Period | Contributions |
|------|--------|--------|---------------|
| *None yet* | - | - | - |

Emeritus maintainers:
- Retain recognition for contributions
- Can participate in discussions
- Can return to active maintainer status (via re-nomination)

## Conflict Resolution

### Process

1. **Direct Discussion** - Parties attempt to resolve privately
2. **Mediation** - Neutral maintainer mediates
3. **Escalation** - Bring to all maintainers for consensus
4. **Vote** - If consensus fails, majority vote

### Code of Conduct Violations

Follow enforcement guidelines in [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

## Communication Channels

### Public

- **GitHub Issues** - Bug reports, feature requests
- **GitHub Discussions** - General questions, ideas
- **Pull Requests** - Code contributions

### Private (Maintainers Only)

- **Maintainer Email** - maintainers@llm-unify.dev
- **Maintainer Chat** - Private channel for sensitive topics

## Meeting Schedule

- **Monthly** - Maintainer sync (first Monday of month)
- **Quarterly** - Roadmap planning
- **Ad-hoc** - As needed for urgent matters

Meeting notes published in GitHub Discussions (sensitive topics redacted).

## Amendment Process

This governance document can be amended by:

1. Pull request proposing changes
2. Discussion period (14 days minimum)
3. Consensus or 2/3 majority vote of maintainers
4. Changes take effect upon merge

## Contact

- **General:** hello@llm-unify.dev
- **Maintainers:** maintainers@llm-unify.dev
- **Security:** security@llm-unify.dev
- **Code of Conduct:** conduct@llm-unify.dev

---

*Last updated: 2025-11-28*
