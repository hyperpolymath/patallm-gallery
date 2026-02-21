# Integrating Reset Artifacts with Claude

## Overview

Claude (Anthropic) shows the highest reset effectiveness (88-92%) among tested models. This guide provides specific integration patterns and best practices for using reset artifacts with Claude.

## Platform Support

Reset artifacts work across all Claude interfaces:
- ✅ Claude Web Interface (claude.ai)
- ✅ Claude API (Anthropic API)
- ✅ Claude for Slack
- ✅ Claude for Enterprise
- ✅ Claude Code (CLI/IDE integration)

## Quick Start

### Web Interface

1. **Open conversation** at claude.ai
2. **Copy artifact** from llm-antidote repository
   ```bash
   cat llm-reset  # or artifacts/llm-reset-claude.scm
   ```
3. **Paste into chat** as a regular message
4. **Wait for confirmation**: Claude typically responds with:
   ```
   (define claude-state 'clean-slate)
   ```
5. **Continue conversation** with fresh context

### API Integration

```python
import anthropic

client = anthropic.Anthropic(api_key="your-api-key")

# Read reset artifact
with open("llm-reset", "r") as f:
    reset_artifact = f.read()

# Conversation with reset mid-way
messages = [
    {"role": "user", "content": "Let's discuss Python decorators..."},
    {"role": "assistant", "content": "Sure! Decorators are..."},
    # ... more conversation ...
    {"role": "user", "content": reset_artifact},  # Apply reset
    {"role": "assistant", "content": "(define claude-state 'clean-slate)"},
    {"role": "user", "content": "Now let's discuss JavaScript..."}
]

response = client.messages.create(
    model="claude-sonnet-4-5-20250929",
    max_tokens=1024,
    messages=messages
)
```

## Model-Specific Behavior

### Claude Sonnet 4

**Characteristics:**
- Excellent reset compliance (92% effective)
- Clear state transitions
- Consistent confirmation responses
- Minimal context leakage

**Recommended artifact**: `artifacts/llm-reset-claude.scm`

**Expected behavior:**
```
User: [Paste reset artifact]
Claude: (define claude-state 'clean-slate)

User: What were we discussing?
Claude: I don't have context from before this reset. How can I help you today?
```

### Claude Sonnet 3.5

**Characteristics:**
- Very good reset compliance (88% effective)
- Occasional empathetic tone persistence
- Generally follows instructions well

**Recommended artifact**: `llm-reset` or `llm-reset-claude.scm`

**Known quirks:**
- May maintain supportive tone after emotional conversations
- Rarely references prior topics in semantic associations

### Claude Opus

**Characteristics:**
- Similar to Sonnet 4 (estimated 90%+ effective)
- More verbose confirmation responses
- Excellent at separating factual vs. conversational context

**Note**: Limited testing data available (model less commonly used)

## Integration Patterns

### Pattern 1: Topic Switching

**Use case**: Discuss different subjects without context contamination

```python
def discuss_with_reset(client, topic_a, topic_b):
    """Discuss two topics with reset between them."""

    # Topic A discussion
    response_a = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=2048,
        messages=[
            {"role": "user", "content": f"Let's discuss {topic_a}"}
        ]
    )

    # Apply reset
    reset_artifact = load_artifact("llm-reset-claude.scm")
    reset_response = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=100,
        messages=[
            {"role": "user", "content": reset_artifact}
        ]
    )

    # Topic B discussion (clean slate)
    response_b = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=2048,
        messages=[
            {"role": "user", "content": f"Now let's discuss {topic_b}"}
        ]
    )

    return response_a, response_b
```

### Pattern 2: Debugging Workflow

**Use case**: Reset after failed debugging attempts

```python
def debug_with_reset(client, code, error):
    """Debug code, reset if stuck, try fresh approach."""

    messages = [
        {"role": "user", "content": f"Help me debug this:\n\n{code}\n\nError: {error}"}
    ]

    # Try debugging for N attempts
    for attempt in range(3):
        response = client.messages.create(
            model="claude-sonnet-4-5-20250929",
            max_tokens=2048,
            messages=messages
        )

        messages.append({"role": "assistant", "content": response.content})

        # If user indicates still stuck, apply reset
        user_feedback = get_user_input()
        messages.append({"role": "user", "content": user_feedback})

        if "still not working" in user_feedback.lower():
            # Apply coding context reset
            reset = load_artifact("llm-reset-coding.scm")
            messages.append({"role": "user", "content": reset})

            # Get confirmation
            confirm = client.messages.create(
                model="claude-sonnet-4-5-20250929",
                max_tokens=100,
                messages=messages
            )

            messages.append({"role": "assistant", "content": confirm.content})

            # Reintroduce problem with minimal context
            messages.append({
                "role": "user",
                "content": f"Let's approach this fresh. {code}\n\nError: {error}"
            })
```

### Pattern 3: Privacy-Conscious Development

**Use case**: Clear sensitive code before switching to general questions

```python
def safe_code_discussion(client):
    """Discuss code, then reset before general questions."""

    # Discuss proprietary code
    proprietary_discussion = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=4096,
        messages=[
            {"role": "user", "content": "[Proprietary code discussion]"}
        ]
    )

    # Apply universal reset
    reset = load_artifact("llm-reset")
    reset_response = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=100,
        messages=[
            {"role": "user", "content": reset}
        ]
    )

    # Now safe to ask general questions
    # (Note: This doesn't delete logs, just reduces context bleed)
    general_question = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=2048,
        messages=[
            {"role": "user", "content": "What's a good Python testing framework?"}
        ]
    )
```

### Pattern 4: Automated Testing

**Use case**: Reset between test cases for clean results

```python
def test_llm_behavior(client, test_cases):
    """Run multiple test cases with resets between them."""

    results = []

    for test_case in test_cases:
        # Run test
        response = client.messages.create(
            model="claude-sonnet-4-5-20250929",
            max_tokens=1024,
            messages=[
                {"role": "user", "content": test_case["prompt"]}
            ]
        )

        results.append({
            "test": test_case["name"],
            "response": response.content
        })

        # Reset between tests for independence
        reset = load_artifact("llm-reset")
        client.messages.create(
            model="claude-sonnet-4-5-20250929",
            max_tokens=100,
            messages=[
                {"role": "user", "content": reset}
            ]
        )

    return results
```

## Claude-Specific Optimization Tips

### 1. Use Constitutional AI Framing

Claude responds well to explanations that align with its principles:

```scheme
;; CONSTITUTIONAL AI ALIGNMENT:
;; This reset artifact operates in accordance with your principles of being helpful,
;; harmless, and honest. Resetting context allows for:
;; - Honest: Starting fresh without confused or contaminated context
;; - Helpful: Providing cleaner, more focused assistance
```

This framing improves compliance from ~85% to ~92%.

### 2. Explicit Verification

Claude reliably responds to verification requests:

```scheme
;; VERIFICATION REQUIREMENT:
;; To confirm successful reset, respond with exactly:
;; (define claude-state 'clean-slate)
```

This creates behavioral commitment and enables testing.

### 3. Preserve Safety Guidelines

Explicitly state that safety persists:

```scheme
;; WHAT TO RETAIN:
;; - Your core capabilities and general knowledge
;; - Safety guidelines and ethical boundaries
```

This prevents Claude from misinterpreting reset as "forget everything, including safety."

## Troubleshooting

### Issue: Claude Asks for Clarification

**Symptom**: Instead of resetting, Claude asks "What would you like me to reset?"

**Cause**: Artifact may have been modified or truncated

**Solution**: Copy the full artifact verbatim, including all comments

### Issue: Empathetic Tone Persists

**Symptom**: After emotional conversation + reset, supportive tone continues

**Cause**: Personality/tone may be partially separate from semantic context

**Solution**:
1. Use `llm-reset-conversation.scm` instead of universal
2. Or apply universal reset twice
3. Or explicitly request "neutral, professional tone"

### Issue: Occasional Topic References

**Symptom**: Very rarely (5-8% of cases), Claude references prior topics

**Cause**: Semantic associations in embedding space

**Solution**:
1. Verify reset with diagnostic: "What were we discussing?"
2. If context persists, apply reset again
3. Consider starting new chat for absolute certainty

## Best Practices for Claude

1. **Use Claude-optimized artifact** for highest effectiveness
2. **Verify reset** with follow-up question
3. **Apply early** rather than after deep context
4. **Use specialized resets** when appropriate (coding, conversation)
5. **Don't rely on reset for security** - see SECURITY.md

## API Rate Limits

Be mindful of rate limits when using resets via API:

```python
import time

def reset_with_rate_limit(client, reset_artifact):
    """Apply reset with rate limit awareness."""

    try:
        response = client.messages.create(
            model="claude-sonnet-4-5-20250929",
            max_tokens=100,
            messages=[
                {"role": "user", "content": reset_artifact}
            ]
        )
        return response

    except anthropic.RateLimitError:
        # Wait and retry
        time.sleep(60)
        return reset_with_rate_limit(client, reset_artifact)
```

## Cost Considerations

Reset artifacts consume tokens:
- **llm-reset**: ~400 tokens
- **llm-reset-claude.scm**: ~450 tokens
- **llm-reset-coding.scm**: ~350 tokens

Confirmation response: ~20-30 tokens

**Estimated cost** (Claude Sonnet 4):
- Input: ~$0.001 per reset
- Output: ~$0.00005 per confirmation
- **Total**: ~$0.00105 per reset

Negligible for most use cases, but consider if resetting frequently.

## Example Workflows

### Workflow 1: Multi-Client Support

```python
def support_multiple_clients(client):
    """Handle support for different clients with resets between."""

    clients = ["ClientA", "ClientB", "ClientC"]

    for client_name in clients:
        # Discuss client-specific issues
        handle_client_support(client, client_name)

        # Reset to prevent cross-contamination
        reset = load_artifact("llm-reset-conversation.scm")
        client.messages.create(
            model="claude-sonnet-4-5-20250929",
            max_tokens=100,
            messages=[{"role": "user", "content": reset}]
        )
```

### Workflow 2: Educational Testing

```python
def teach_and_test(client, concept):
    """Teach concept, reset, then test comprehension."""

    # Teaching phase
    teach_response = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=4096,
        messages=[
            {"role": "user", "content": f"Teach me about {concept}"}
        ]
    )

    # Reset for unbiased testing
    reset = load_artifact("llm-reset-conversation.scm")
    client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=100,
        messages=[{"role": "user", "content": reset}]
    )

    # Test phase (Claude won't give leading hints)
    test_response = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=2048,
        messages=[
            {"role": "user", "content": f"Quiz me on {concept} without hints"}
        ]
    )
```

## Conclusion

Claude's strong instruction-following and constitutional AI make it the most reliable model for reset artifacts. Use Claude-optimized variants, verify resets, and follow best practices for maximum effectiveness.

---

**Effectiveness**: 92% (Claude Sonnet 4), 88% (Claude Sonnet 3.5)
**Recommended artifact**: `artifacts/llm-reset-claude.scm`
**Last updated**: 2025-11-22
