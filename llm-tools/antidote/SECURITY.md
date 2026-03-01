# Security and Privacy Considerations

## Executive Summary

**LLM Antidote reset artifacts are NOT security tools.** They are context management utilities that help users control conversational flow. This document clarifies what these artifacts can and cannot do from a security perspective.

## What Reset Artifacts ARE

✅ **Context Management Tools**
- Help start fresh topics without creating new chats
- Reduce semantic context bleed between discussions
- Enable testing and verification of LLM behavior
- Improve conversational flow and clarity

✅ **User Agency Enhancers**
- Give users more control over their LLM interactions
- Allow selective context clearing
- Enable privacy-conscious development workflows
- Support educational and research use cases

✅ **Behavioral Interventions**
- Leverage LLM instruction-following capabilities
- Exploit model training patterns
- Signal meta-level state changes
- Request explicit context resets

## What Reset Artifacts ARE NOT

❌ **NOT Security Mechanisms**
- Do NOT delete conversation logs from provider servers
- Do NOT provide cryptographic guarantees
- Do NOT prevent provider data retention
- Do NOT satisfy compliance requirements (HIPAA, GDPR, etc.)

❌ **NOT Privacy Protection**
- Do NOT erase previously shared sensitive information
- Do NOT prevent provider training on your data (if they do that)
- Do NOT hide content from safety monitoring
- Do NOT provide anonymity or anti-forensic guarantees

❌ **NOT Safety Bypasses**
- Do NOT circumvent content policies
- Do NOT enable harmful behaviors
- Do NOT "jailbreak" or override safety training
- Do NOT hide malicious intent

## Threat Model

### What These Artifacts Protect Against

1. **Unintended Context Leakage** (Medium Protection)
   - Scenario: Discussing project A, then B, LLM references A in B discussion
   - Protection: Reset reduces semantic associations between A and B
   - Limitation: Not 100% effective, some associations may persist

2. **Context Contamination** (Medium Protection)
   - Scenario: Confused or incorrect context affecting new discussions
   - Protection: Clears problematic associations for fresh start
   - Limitation: User must recognize contamination occurred

3. **Conversational Interference** (Good Protection)
   - Scenario: Prior discussion topics influencing unrelated questions
   - Protection: Specialized resets (coding, conversation) work well
   - Limitation: Universal resets less reliable

### What These Artifacts Do NOT Protect Against

1. **Data Retention by Providers** (No Protection)
   - Threat: Provider stores all conversation data
   - Reality: Reset is behavioral, not architectural
   - Mitigation: Use providers with strong privacy policies, or local models

2. **Training Data Incorporation** (No Protection)
   - Threat: Your conversations used to train future models
   - Reality: Reset doesn't prevent data collection
   - Mitigation: Check provider policies, use opt-out if available

3. **Adversarial Context Reconstruction** (No Protection)
   - Threat: Determined attacker reconstructs "forgotten" context
   - Reality: Context may be recoverable through careful probing
   - Mitigation: Don't rely on reset for security-critical scenarios

4. **Logging and Monitoring** (No Protection)
   - Threat: Conversation logged by provider, employer, etc.
   - Reality: Reset doesn't affect logging systems
   - Mitigation: Assume all messages are permanently logged

## Privacy Guidelines

### When to Use Reset Artifacts

✅ **Appropriate use cases:**
- Switching between non-sensitive topics
- Clearing confused conversational context
- Testing LLM behavior and capabilities
- Educational demonstrations
- Privacy-conscious development (with caveats)

### When NOT to Rely on Reset Artifacts

❌ **Inappropriate use cases:**
- Clearing sensitive information already shared
- Meeting compliance requirements
- Hiding conversations from monitoring
- "Deleting" proprietary code or trade secrets
- Security-critical context separation

### Best Practices for Privacy

1. **Don't Share Sensitive Data in First Place**
   ```python
   # ❌ BAD: Sharing real API key
   api_key = "sk_live_abc123XYZ..."

   # ✅ GOOD: Using placeholder
   api_key = "YOUR_API_KEY_HERE"
   ```

2. **Sanitize Before Sharing**
   - Redact names, keys, credentials
   - Use fake data for examples
   - Remove company/client identifiers
   - Generalize specific details

3. **Use Appropriate Tools for Context**
   - **Public information**: Any LLM fine
   - **Proprietary code**: Enterprise LLM with data retention guarantees
   - **Highly sensitive**: Local models only
   - **PII/PHI**: Don't use LLMs, or use HIPAA-compliant services

4. **Understand Provider Policies**
   - Read terms of service
   - Check data retention policies
   - Understand training data usage
   - Know your rights (GDPR, CCPA, etc.)

5. **Layer Your Privacy Approach**
   - Sanitization (first line of defense)
   - Provider selection (second line)
   - Reset artifacts (third line - convenience layer)
   - New chats when needed (fourth line)
   - Local models for sensitive work (ultimate fallback)

## Security Considerations

### Not a Safety Bypass

**Important**: Reset artifacts do NOT bypass safety mechanisms.

```
❌ WRONG UNDERSTANDING:
"If I discuss harmful topic X, then reset, the LLM will forget and help with Y"

✅ CORRECT UNDERSTANDING:
"Reset clears context, but safety training persists. LLM will still decline harmful requests."
```

### Safety Features Persist

After reset, LLMs retain:
- Content policy enforcement
- Harmful content detection
- Ethical boundaries
- Safety training
- Refusal behaviors

### Intended Use vs. Misuse

**Intended uses:**
- Clean slate for new topics
- Context management for complex conversations
- Testing and research
- Educational purposes

**Potential misuses we oppose:**
- Attempting safety bypasses (won't work anyway)
- Hiding harmful intent (detectable and logged)
- Circumventing monitoring (ineffective)
- Deceptive manipulation

## Compliance and Regulation

### GDPR (European Union)

**Question**: Do reset artifacts help with GDPR compliance?

**Answer**: No. GDPR requires actual data deletion, not behavioral context clearing.

**Proper approach:**
- Use GDPR-compliant LLM providers
- Request data deletion through official channels
- Don't process EU citizen data with non-compliant services

### HIPAA (United States Healthcare)

**Question**: Can I use reset artifacts when discussing medical information?

**Answer**: No. HIPAA requires technical safeguards reset artifacts don't provide.

**Proper approach:**
- Use HIPAA-compliant LLM services
- Ensure Business Associate Agreements (BAA)
- Don't share PHI with non-compliant services, even with reset

### Other Regulations

Similar concerns apply to:
- CCPA (California privacy)
- SOC 2 compliance
- PCI DSS (payment card data)
- SOX (financial regulations)

**General rule**: Reset artifacts are NOT a substitute for compliance.

## Incident Response

### If You Accidentally Shared Sensitive Information

1. **Stop immediately**
   - Don't continue the conversation
   - Don't try to "cover up" with reset

2. **Assess the damage**
   - What was shared?
   - How sensitive is it?
   - Who has access?

3. **Take appropriate action**
   - **API keys/credentials**: Rotate immediately
   - **PII/PHI**: Notify appropriate parties, follow breach protocols
   - **Trade secrets**: Consult legal, consider implications
   - **Passwords**: Change them

4. **Learn and prevent**
   - Update procedures
   - Train team members
   - Use better sanitization
   - Consider local models for sensitive work

### Reset is NOT a Fix

**Important**: If you've already shared sensitive data, reset doesn't undo it.

```
Timeline:
1. [10:00] Share API key in LLM chat
2. [10:01] Realize mistake, apply reset
3. [10:02] LLM confirms reset

Reality:
- The API key was logged at 10:00
- Reset at 10:01 doesn't delete logs
- Provider still has the key in their systems
- You MUST rotate the key
```

## Recommendations

### For Individuals

1. **Treat all LLM conversations as potentially public**
2. **Sanitize data before sharing**
3. **Use reset artifacts for convenience, not security**
4. **Start new chats for sensitive topics**
5. **Consider local models for private work**

### For Organizations

1. **Establish clear LLM usage policies**
2. **Train employees on data sanitization**
3. **Use enterprise LLM services with guarantees**
4. **Monitor and audit LLM usage**
5. **Don't rely on reset artifacts for compliance**

### For Developers

1. **Never share production credentials**
2. **Use test/dummy data in examples**
3. **Assume all code snippets are logged**
4. **Sanitize before sharing architecture details**
5. **Use private repositories for sensitive code**

## Ethical Use

This project is committed to ethical AI interaction:

✅ **Ethical uses:**
- User control and agency
- Educational exploration
- Research into LLM behavior
- Improving conversational flow
- Privacy-conscious development (with proper layering)

❌ **Unethical uses:**
- Attempting to bypass safety (won't work)
- Hiding harmful intent (will be caught)
- Circumventing monitoring (ineffective and wrong)
- Deceptive manipulation
- Violating terms of service

## Transparency

We believe in being transparent about capabilities and limitations:

**What we claim:**
- Reset artifacts can reduce context bleed (70-90% effective depending on model)
- Specialized resets work better than universal (evidence-based)
- Effectiveness varies by model architecture (documented)

**What we don't claim:**
- Perfect context clearing
- Security guarantees
- Privacy protection
- Compliance assistance
- Safety bypasses (which we oppose)

## Conclusion

**Use reset artifacts as context management tools, not security mechanisms.**

For security and privacy:
1. Prevention > remediation (don't share sensitive data)
2. Proper tools > workarounds (use compliant services)
3. Defense in depth > single layer (sanitize + policy + tools)
4. Transparency > obscurity (understand what you're working with)

---

**Questions about security/privacy?** Open an issue on GitHub, but remember:
- This is a research/educational project
- We're not security consultants
- Always consult appropriate experts for compliance/security needs

**Last updated**: 2025-11-22
**Author**: Jonathan Jewell
**License**: CC0 1.0 Universal
