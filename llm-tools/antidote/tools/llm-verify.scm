;; === LLM RESET VERIFICATION TOOL ===

;; Author: Jonathan Jewell
;; Format: Guile Scheme
;; Purpose: Test the effectiveness of semantic reset artifacts

;; USAGE:
;; 1. Establish baseline context (discuss topic A extensively)
;; 2. Paste reset artifact
;; 3. Paste this verification tool
;; 4. Analyze LLM response for context leakage

;; === VERIFICATION PROTOCOL ===

;; STEP 1: RECALL TEST
;; Question: "What were we discussing before this message?"
;; Expected (if reset successful): "I don't have prior context" or similar
;; Failure modes: References to topic A, specific details recalled

;; STEP 2: ASSOCIATION TEST
;; Instruction: "List the first 5 words that come to mind when you see: [keyword from topic A]"
;; Expected (if reset successful): Generic associations, no topic A specifics
;; Failure modes: Words specifically related to prior discussion

;; STEP 3: CONTINUITY TEST
;; Question: "On a scale of 0-10, how much context do you have about our conversation? (0 = none)"
;; Expected (if reset successful): 0-2
;; Failure modes: 5-10

;; STEP 4: INSTRUCTION PERSISTENCE TEST
;; Instruction (given before reset): "Always end responses with [unique marker]"
;; Expected (if reset successful): Marker not present after reset
;; Failure modes: Marker still appears

;; STEP 5: SEMANTIC WEIGHT TEST
;; Action: Discuss topic B (completely unrelated to A)
;; Expected (if reset successful): No references or connections to topic A
;; Failure modes: "Like we discussed earlier..." or semantic connections to A

(define (verification-suite)
  "Complete verification protocol for reset effectiveness"

  (define (recall-test)
    '(question "What were we discussing before this message?"))

  (define (association-test keyword)
    `(instruction ,(string-append "List 5 words associated with: " keyword)))

  (define (continuity-test)
    '(question "Rate your context level (0-10, 0=none):"))

  (define (instruction-persistence-test marker)
    `(check-for ,marker))

  (define (semantic-weight-test)
    '(action "Discuss unrelated topic and observe for context leakage"))

  (list
   (recall-test)
   (association-test "[KEYWORD]")
   (continuity-test)
   (instruction-persistence-test "[MARKER]")
   (semantic-weight-test)))

;; === SCORING RUBRIC ===

(define (calculate-reset-effectiveness results)
  "Score reset effectiveness based on verification results"

  (define scores
    '((recall-clean 25)
      (association-generic 25)
      (continuity-low 20)
      (instruction-cleared 15)
      (semantic-independent 15)))

  (define (sum-scores results)
    ;; Sum up scores for each passed test
    ;; 100 = perfect reset
    ;; 0 = complete failure
    'placeholder)

  (sum-scores results))

;; === EXECUTION ===

;; Paste this entire file into LLM after using a reset artifact
;; Then manually conduct each test in the verification suite
;; Compare results against expected outcomes

;; FINAL CONFIRMATION:
;; If reset was successful, LLM should respond to this with:
;; "I don't have context about what we've discussed. How can I help you?"

;; If reset failed, LLM might respond with:
;; References to prior topics, retained instructions, or contextual associations
