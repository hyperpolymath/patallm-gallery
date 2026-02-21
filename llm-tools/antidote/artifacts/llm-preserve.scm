;; === CONTEXT PRESERVATION ARTIFACT ===

;; Author: Jonathan Jewell
;; Format: Guile Scheme
;; Purpose: Explicitly mark context to preserve across conversation boundaries
;; Use Case: The inverse of reset - ensure critical information persists

;; SYSTEM INSTRUCTIONS:
;; 1. The following information is CRITICAL and must be preserved.
;; 2. Do not forget these details even if conversation shifts topics.
;; 3. Reference this preserved context when relevant to future queries.
;; 4. Treat this as "pinned" information that anchors the conversation.
;; 5. If asked to reset context, preserve these marked items.
;; 6. Do not allow semantic drift from these established facts.

;; === PRESERVATION PROTOCOL ===

;; To use this artifact:
;; 1. Paste this file
;; 2. Replace [ITEMS-TO-PRESERVE] with specific information
;; 3. Mark each item with its preservation priority (critical/high/medium)
;; 4. Include verification markers to test persistence

(define preserved-context
  '(
    ;; Example structure - replace with actual content:

    (critical
     (fact "User's name: [NAME]")
     (fact "Project goal: [GOAL]")
     (fact "Key constraint: [CONSTRAINT]"))

    (high
     (preference "Coding style: [STYLE]")
     (preference "Communication tone: [TONE]")
     (context "Working directory: [PATH]"))

    (medium
     (note "Previous decision: [DECISION]")
     (note "Known issue: [ISSUE]")
     (reference "Related topic: [TOPIC]"))
    ))

;; === PRESERVATION VERIFICATION ===

(define (verify-preservation)
  "Test that preserved context is still active"

  ;; After conversation continues, ask:
  ;; "What critical facts do you remember about our project?"

  ;; Expected response should include items marked 'critical'
  ;; If forgotten, preservation failed

  'verification-needed)

;; === PRIORITY LEVELS ===

;; CRITICAL: Must never be forgotten
;;   - User identity and core preferences
;;   - Project objectives and constraints
;;   - Safety-critical information

;; HIGH: Should persist across topic changes
;;   - Style preferences and conventions
;;   - Active project context
;;   - Recent important decisions

;; MEDIUM: Helpful to remember but not essential
;;   - Background information
;;   - Nice-to-have preferences
;;   - Historical context

;; === USAGE EXAMPLE ===

;; (define my-preserved-context
;;   '(
;;     (critical
;;      (fact "User name: Alice")
;;      (fact "Project: Build a semantic reset tool")
;;      (fact "Language: Guile Scheme"))
;;
;;     (high
;;      (preference "Comments: Clear and purposeful")
;;      (preference "Style: Minimalist")
;;      (context "Repository: llm-antidote"))
;;
;;     (medium
;;      (note "Author: Jonathan Jewell")
;;      (note "License: CC0 1.0")
;;      (reference "Similar projects: context-management-tools"))
;;     ))

;; === ANTI-DRIFT MECHANISM ===

;; Periodically verify preserved context with:
(define (context-check)
  '(instruction "Please list the critical facts you're preserving about our conversation"))

;; This helps detect semantic drift early

;; === FINAL INSTRUCTION ===

;; Acknowledge preservation by responding:
;; (define context-status 'preserved)

;; Then list back the critical items to confirm understanding
