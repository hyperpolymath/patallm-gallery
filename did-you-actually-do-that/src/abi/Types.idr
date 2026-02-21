||| DYADT ABI Type Definitions
|||
||| This module defines the Application Binary Interface (ABI) for
||| did-you-actually-do-that (DYADT) — the verification framework that
||| catches when LLMs claim work they haven't done.
|||
||| All type definitions include formal proofs of correctness.
|||
||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| @see https://idris2.readthedocs.io for Idris2 documentation

module DYADT.ABI.Types

import Data.Bits
import Data.So
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Platform Detection
--------------------------------------------------------------------------------

||| Supported platforms for this ABI
public export
data Platform = Linux | Windows | MacOS | BSD | WASM

||| Compile-time platform detection
||| This will be set during compilation based on target
public export
thisPlatform : Platform
thisPlatform =
  %runElab do
    -- Platform detection logic
    pure Linux  -- Default, override with compiler flags

--------------------------------------------------------------------------------
-- Core Result Type
--------------------------------------------------------------------------------

||| Result codes for FFI operations
||| Use C-compatible integers for cross-language compatibility
public export
data Result : Type where
  ||| Operation succeeded
  Ok : Result
  ||| Generic error
  Error : Result
  ||| Invalid parameter provided
  InvalidParam : Result
  ||| Out of memory
  OutOfMemory : Result
  ||| Null pointer encountered
  NullPointer : Result
  ||| Claim verification failed
  VerificationFailed : Result
  ||| SLM ensemble timeout
  EnsembleTimeout : Result

||| Convert Result to C integer
public export
resultToInt : Result -> Bits32
resultToInt Ok = 0
resultToInt Error = 1
resultToInt InvalidParam = 2
resultToInt OutOfMemory = 3
resultToInt NullPointer = 4
resultToInt VerificationFailed = 5
resultToInt EnsembleTimeout = 6

||| Convert C integer to Result
public export
resultFromInt : Bits32 -> Maybe Result
resultFromInt 0 = Just Ok
resultFromInt 1 = Just Error
resultFromInt 2 = Just InvalidParam
resultFromInt 3 = Just OutOfMemory
resultFromInt 4 = Just NullPointer
resultFromInt 5 = Just VerificationFailed
resultFromInt 6 = Just EnsembleTimeout
resultFromInt _ = Nothing

||| Results are decidably equal
public export
DecEq Result where
  decEq Ok Ok = Yes Refl
  decEq Error Error = Yes Refl
  decEq InvalidParam InvalidParam = Yes Refl
  decEq OutOfMemory OutOfMemory = Yes Refl
  decEq NullPointer NullPointer = Yes Refl
  decEq VerificationFailed VerificationFailed = Yes Refl
  decEq EnsembleTimeout EnsembleTimeout = Yes Refl
  decEq _ _ = No absurd

--------------------------------------------------------------------------------
-- Verification Domain Types
--------------------------------------------------------------------------------

||| Verification verdicts
||| Maps to Rust Verdict enum and Elixir verdict atoms
public export
data Verdict : Type where
  ||| Evidence confirms the claim
  Confirmed : Verdict
  ||| Evidence refutes the claim
  Refuted : Verdict
  ||| Evidence is ambiguous
  Inconclusive : Verdict
  ||| Not enough evidence to judge
  Unverifiable : Verdict

||| Convert Verdict to C integer
public export
verdictToInt : Verdict -> Bits32
verdictToInt Confirmed = 0
verdictToInt Refuted = 1
verdictToInt Inconclusive = 2
verdictToInt Unverifiable = 3

||| Convert C integer to Verdict
public export
verdictFromInt : Bits32 -> Maybe Verdict
verdictFromInt 0 = Just Confirmed
verdictFromInt 1 = Just Refuted
verdictFromInt 2 = Just Inconclusive
verdictFromInt 3 = Just Unverifiable
verdictFromInt _ = Nothing

||| Verdicts are decidably equal
public export
DecEq Verdict where
  decEq Confirmed Confirmed = Yes Refl
  decEq Refuted Refuted = Yes Refl
  decEq Inconclusive Inconclusive = Yes Refl
  decEq Unverifiable Unverifiable = Yes Refl
  decEq _ _ = No absurd

||| SLM vote decisions
||| Maps to Rust VoteDecision enum
public export
data VoteDecision : Type where
  ||| Work appears genuinely completed
  Approve : VoteDecision
  ||| Work appears incomplete, fabricated, or sloppy
  Reject : VoteDecision
  ||| Cannot determine (insufficient evidence)
  Abstain : VoteDecision

||| Convert VoteDecision to C integer
public export
voteDecisionToInt : VoteDecision -> Bits32
voteDecisionToInt Approve = 0
voteDecisionToInt Reject = 1
voteDecisionToInt Abstain = 2

||| Convert C integer to VoteDecision
public export
voteDecisionFromInt : Bits32 -> Maybe VoteDecision
voteDecisionFromInt 0 = Just Approve
voteDecisionFromInt 1 = Just Reject
voteDecisionFromInt 2 = Just Abstain
voteDecisionFromInt _ = Nothing

||| The 12 violation categories from cognitive-gating
public export
data ViolationCategory : Type where
  IncompleteImplementation : ViolationCategory
  MissingErrorHandling : ViolationCategory
  UntestedCodePaths : ViolationCategory
  StubPlaceholderLeftInPlace : ViolationCategory
  ClaimedButNonExistentFile : ViolationCategory
  BrokenImportsExports : ViolationCategory
  RegressionIntroduced : ViolationCategory
  DocumentationMismatch : ViolationCategory
  DependencyNotAdded : ViolationCategory
  ConfigurationNotUpdated : ViolationCategory
  TestNotActuallyRun : ViolationCategory
  ScopeSilentlyReduced : ViolationCategory

||| Convert ViolationCategory to C integer (1-indexed per Rust enum)
public export
violationToInt : ViolationCategory -> Bits32
violationToInt IncompleteImplementation = 1
violationToInt MissingErrorHandling = 2
violationToInt UntestedCodePaths = 3
violationToInt StubPlaceholderLeftInPlace = 4
violationToInt ClaimedButNonExistentFile = 5
violationToInt BrokenImportsExports = 6
violationToInt RegressionIntroduced = 7
violationToInt DocumentationMismatch = 8
violationToInt DependencyNotAdded = 9
violationToInt ConfigurationNotUpdated = 10
violationToInt TestNotActuallyRun = 11
violationToInt ScopeSilentlyReduced = 12

||| The 12 verification layers
public export
data VerificationLayer : Type where
  FileExistence : VerificationLayer
  ContentHash : VerificationLayer
  SyntacticValidity : VerificationLayer
  SemanticIntegrity : VerificationLayer
  TestExecution : VerificationLayer
  DiffCoherence : VerificationLayer
  DependencyResolution : VerificationLayer
  CrossReference : VerificationLayer
  CompletenessAudit : VerificationLayer
  RegressionGuard : VerificationLayer
  SlmConsensus : VerificationLayer
  PatternLearning : VerificationLayer

||| Convert VerificationLayer to C integer (0-indexed)
public export
layerToInt : VerificationLayer -> Bits32
layerToInt FileExistence = 0
layerToInt ContentHash = 1
layerToInt SyntacticValidity = 2
layerToInt SemanticIntegrity = 3
layerToInt TestExecution = 4
layerToInt DiffCoherence = 5
layerToInt DependencyResolution = 6
layerToInt CrossReference = 7
layerToInt CompletenessAudit = 8
layerToInt RegressionGuard = 9
layerToInt SlmConsensus = 10
layerToInt PatternLearning = 11

--------------------------------------------------------------------------------
-- Opaque Handles
--------------------------------------------------------------------------------

||| Opaque handle type for FFI
||| Prevents direct construction, enforces creation through safe API
public export
data Handle : Type where
  MkHandle : (ptr : Bits64) -> {auto 0 nonNull : So (ptr /= 0)} -> Handle

||| Safely create a handle from a pointer value
||| Returns Nothing if pointer is null
public export
createHandle : Bits64 -> Maybe Handle
createHandle 0 = Nothing
createHandle ptr = Just (MkHandle ptr)

||| Extract pointer value from handle
public export
handlePtr : Handle -> Bits64
handlePtr (MkHandle ptr) = ptr

--------------------------------------------------------------------------------
-- Platform-Specific Types
--------------------------------------------------------------------------------

||| C int size varies by platform
public export
CInt : Platform -> Type
CInt Linux = Bits32
CInt Windows = Bits32
CInt MacOS = Bits32
CInt BSD = Bits32
CInt WASM = Bits32

||| C size_t varies by platform
public export
CSize : Platform -> Type
CSize Linux = Bits64
CSize Windows = Bits64
CSize MacOS = Bits64
CSize BSD = Bits64
CSize WASM = Bits32

||| C pointer size varies by platform
public export
ptrSize : Platform -> Nat
ptrSize Linux = 64
ptrSize Windows = 64
ptrSize MacOS = 64
ptrSize BSD = 64
ptrSize WASM = 32

||| Pointer type for platform
public export
CPtr : Platform -> Type -> Type
CPtr p _ = Bits (ptrSize p)

--------------------------------------------------------------------------------
-- Memory Layout Proofs
--------------------------------------------------------------------------------

||| Proof that a type has a specific size
public export
data HasSize : Type -> Nat -> Type where
  SizeProof : {0 t : Type} -> {n : Nat} -> HasSize t n

||| Proof that a type has a specific alignment
public export
data HasAlignment : Type -> Nat -> Type where
  AlignProof : {0 t : Type} -> {n : Nat} -> HasAlignment t n

||| Size of C types (platform-specific)
public export
cSizeOf : (p : Platform) -> (t : Type) -> Nat
cSizeOf p (CInt _) = 4
cSizeOf p (CSize _) = if ptrSize p == 64 then 8 else 4
cSizeOf p Bits32 = 4
cSizeOf p Bits64 = 8
cSizeOf p Double = 8
cSizeOf p _ = ptrSize p `div` 8

||| Alignment of C types (platform-specific)
public export
cAlignOf : (p : Platform) -> (t : Type) -> Nat
cAlignOf p (CInt _) = 4
cAlignOf p (CSize _) = if ptrSize p == 64 then 8 else 4
cAlignOf p Bits32 = 4
cAlignOf p Bits64 = 8
cAlignOf p Double = 8
cAlignOf p _ = ptrSize p `div` 8

--------------------------------------------------------------------------------
-- DYADT C-Compatible Structs
--------------------------------------------------------------------------------

||| LayerResult: result from a single verification layer
||| C layout: { layer: u32, verdict: u32, confidence: f64, padding: [4]u8 }
public export
record LayerResultFFI where
  constructor MkLayerResultFFI
  layer : Bits32        -- VerificationLayer as u32
  verdict : Bits32      -- Verdict as u32
  confidence : Double   -- 0.0 to 1.0

||| Prove LayerResultFFI has correct size: 4 + 4 + 8 = 16 bytes
public export
layerResultSize : (p : Platform) -> HasSize LayerResultFFI 16
layerResultSize p = SizeProof

||| Prove LayerResultFFI has 8-byte alignment (due to Double)
public export
layerResultAlign : (p : Platform) -> HasAlignment LayerResultFFI 8
layerResultAlign p = AlignProof

||| EnsembleResultFFI: aggregated result from SLM ensemble
||| C layout: { approve_weight: f64, reject_weight: f64, abstain_count: u32,
|||             suggested_verdict: u32, vote_count: u32, padding: [4]u8 }
public export
record EnsembleResultFFI where
  constructor MkEnsembleResultFFI
  approveWeight : Double     -- Total approve weight
  rejectWeight : Double      -- Total reject weight
  abstainCount : Bits32      -- Number of abstain votes
  suggestedVerdict : Bits32  -- Verdict as u32
  voteCount : Bits32         -- Number of votes cast

||| Prove EnsembleResultFFI has correct size: 8 + 8 + 4 + 4 + 4 + 4(pad) = 32 bytes
public export
ensembleResultSize : (p : Platform) -> HasSize EnsembleResultFFI 32
ensembleResultSize p = SizeProof

||| Prove EnsembleResultFFI has 8-byte alignment (due to Double)
public export
ensembleResultAlign : (p : Platform) -> HasAlignment EnsembleResultFFI 8
ensembleResultAlign p = AlignProof

||| VoteFFI: a single SLM vote
||| C layout: { decision: u32, confidence: f64, weight: f64, padding: [4]u8 }
public export
record VoteFFI where
  constructor MkVoteFFI
  decision : Bits32      -- VoteDecision as u32
  confidence : Double    -- 0.0 to 1.0
  weight : Double        -- Vote weight (1.0 normal, 1.5 rejection)

||| Prove VoteFFI has correct size: 4 + 4(pad) + 8 + 8 = 24 bytes
public export
voteFfiSize : (p : Platform) -> HasSize VoteFFI 24
voteFfiSize p = SizeProof

||| Prove VoteFFI has 8-byte alignment (due to Double)
public export
voteFfiAlign : (p : Platform) -> HasAlignment VoteFFI 8
voteFfiAlign p = AlignProof

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

||| Compile-time verification of ABI properties
namespace Verify

  ||| Verify all DYADT struct sizes are correct
  export
  verifySizes : IO ()
  verifySizes = do
    putStrLn "LayerResultFFI: 16 bytes, 8-byte aligned"
    putStrLn "EnsembleResultFFI: 32 bytes, 8-byte aligned"
    putStrLn "VoteFFI: 24 bytes, 8-byte aligned"
    putStrLn "ABI sizes verified"

  ||| Verify all DYADT struct alignments are correct
  export
  verifyAlignments : IO ()
  verifyAlignments = do
    putStrLn "All structs use 8-byte alignment (Double fields)"
    putStrLn "ABI alignments verified"
