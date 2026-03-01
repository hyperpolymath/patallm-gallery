{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.Internal.Types
-- Description : Core types for verification conditions and results
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- Core type definitions used throughout claude-verify.
-- These types represent verification conditions, prover results,
-- and the feedback loop data structures.

module ClaudeVerify.Internal.Types
    ( -- * Verification Conditions
      VerificationCondition(..)
    , VCId(..)
    , VCFormula(..)
    , FormulaFormat(..)
    , SourceLocation(..)

      -- * Verification Results
    , VerificationResult(..)
    , ProverResult(..)
    , ProverStatus(..)
    , ProofCertificate(..)
    , Counterexample(..)

      -- * Feedback
    , FeedbackItem(..)
    , FeedbackCategory(..)
    , FeedbackSource(..)

      -- * Configuration
    , ProjectConfig(..)
    , KnownIssue(..)
    , VerificationLevel(..)

      -- * Session
    , SessionId(..)
    , TaskId(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- | Unique identifier for a verification condition
newtype VCId = VCId { unVCId :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Unique identifier for a session
newtype SessionId = SessionId { unSessionId :: UUID }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Unique identifier for a verification task
newtype TaskId = TaskId { unTaskId :: UUID }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Source location in a file
data SourceLocation = SourceLocation
    { slFile   :: FilePath
    , slLine   :: Int
    , slColumn :: Maybe Int
    , slEndLine :: Maybe Int
    , slEndColumn :: Maybe Int
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Format of a verification condition formula
data FormulaFormat
    = SMTLIB2          -- ^ SMT-LIB v2 format (Z3, CVC5, Yices2)
    | TPTP             -- ^ TPTP format (Vampire, E)
    | Coq              -- ^ Coq/Gallina
    | Lean4            -- ^ Lean 4
    | Agda             -- ^ Agda
    | Isabelle         -- ^ Isabelle/HOL
    | TLAPlus          -- ^ TLA+
    | Alloy            -- ^ Alloy
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A formula to be verified
data VCFormula = VCFormula
    { vcfFormat  :: FormulaFormat
    , vcfContent :: Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | A verification condition extracted from source code
data VerificationCondition = VerificationCondition
    { vcId          :: VCId
    , vcDescription :: Text
    , vcFormula     :: VCFormula
    , vcLocation    :: SourceLocation
    , vcKind        :: Text  -- ^ e.g., "bounds_check", "null_safety", "invariant"
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Status of a prover attempt
data ProverStatus
    = Proved            -- ^ Formula is valid (or satisfiable, depending on context)
    | Counterexample    -- ^ Formula is invalid, model provided
    | Unknown           -- ^ Prover couldn't determine (timeout, resource limit)
    | Error Text        -- ^ Prover encountered an error
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A proof certificate (prover-specific)
newtype ProofCertificate = ProofCertificate { unProofCertificate :: Text }
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | A counterexample/model
newtype Counterexample = Counterexample { unCounterexample :: Text }
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Result from a single prover
data ProverResult = ProverResult
    { prProver       :: Text
    , prStatus       :: ProverStatus
    , prTimeMs       :: Int
    , prCertificate  :: Maybe ProofCertificate
    , prCounterexample :: Maybe Counterexample
    , prRawOutput    :: Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Aggregated verification result
data VerificationResult = VerificationResult
    { vrTaskId           :: TaskId
    , vrVCId             :: VCId
    , vrResults          :: [ProverResult]
    , vrAggregateStatus  :: ProverStatus
    , vrConfidence       :: Double  -- ^ 0.0 to 1.0
    , vrTimestamp        :: UTCTime
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Category of feedback
data FeedbackCategory
    = SyntaxError
    | TypeError
    | LogicError
    | SecurityFlaw
    | PerformanceIssue
    | SemanticMismatch
    | StyleViolation
    | Other Text
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | How the issue was detected
data FeedbackSource
    = Compiler
    | TypeChecker
    | Linter
    | TestSuite
    | FormalProver
    | StaticAnalyzer
    | UserReported
    | RuntimeError
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A feedback item capturing a Claude mistake
data FeedbackItem = FeedbackItem
    { fiId               :: UUID
    , fiTimestamp        :: UTCTime
    , fiProject          :: Maybe Text
    , fiSessionId        :: Maybe SessionId
    , fiCategory         :: FeedbackCategory
    , fiSubcategory      :: Maybe Text
    , fiSourceFile       :: Maybe FilePath
    , fiSourceLines      :: Maybe (Int, Int)  -- ^ (start, end)
    , fiClaudeOutput     :: Text
    , fiActualIssue      :: Text
    , fiCorrection       :: Maybe Text
    , fiDetectedBy       :: FeedbackSource
    , fiVerified         :: Bool
    , fiSharedWithAnthropic :: Bool
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Level of verification rigor
data VerificationLevel
    = Quick       -- ^ Just compilation and basic lints
    | Standard    -- ^ Plus type checking, tests
    | Rigorous    -- ^ Plus static analysis
    | Exhaustive  -- ^ Plus formal verification
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A known issue that Claude should avoid
data KnownIssue = KnownIssue
    { kiPattern     :: Text      -- ^ What to look for
    , kiDescription :: Text      -- ^ What went wrong before
    , kiGuidance    :: Text      -- ^ What to do instead
    , kiOccurrences :: Int       -- ^ How many times this has happened
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Project-specific configuration
data ProjectConfig = ProjectConfig
    { pcName             :: Text
    , pcLanguages        :: [Text]
    , pcVerificationLevel :: VerificationLevel
    , pcKnownIssues      :: [KnownIssue]
    , pcRequiredChecks   :: [Text]
    , pcOptionalChecks   :: [Text]
    , pcFormalSpecs      :: [(Text, FilePath)]  -- ^ (format, path)
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)
