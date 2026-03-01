-- |
-- Module      : ClaudeVerify
-- Description : Real verification for LLM-generated code
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
-- Stability   : experimental
--
-- = What This Is
--
-- Claude-verify provides /actual/ formal verification for code generated
-- by Claude or other LLMs, rather than relying on pattern matching.
--
-- = The Problem
--
-- When Claude "validates" code, it's doing pattern matching against training
-- data. It's often right (having seen millions of code examples), but it's
-- making educated guesses, not running formal verification.
--
-- This means Claude can and does make mistakes:
--
-- * Off-by-one errors
-- * Subtle type mismatches
-- * Logic errors in edge cases
-- * Security vulnerabilities
--
-- = The Solution
--
-- Claude-verify integrates with ECHIDNA, a neurosymbolic theorem proving
-- platform that provides access to 12 theorem provers:
--
-- * SMT Solvers: Z3, CVC5
-- * Theorem Provers: Lean 4, Coq, Agda, Isabelle, HOL Light, Mizar
-- * First-Order: Vampire, E
-- * Model Checkers: TLC (TLA+), Alloy Analyzer
--
-- = Quick Start
--
-- @
-- import ClaudeVerify
--
-- main :: IO ()
-- main = do
--     -- Load project configuration
--     Right config <- loadConfig
--
--     -- Parse source file
--     Right ast <- parseFile "src/lib.rs"
--
--     -- Extract verification conditions
--     let vcs = extractVCs ast
--
--     -- Verify with ECHIDNA
--     client <- createClient defaultConfig
--     results <- verifyBatch client vcs
--
--     -- Generate report
--     report <- generateReport "Verification" (Just "my-project") results
--     printReport Terminal report
--
--     closeClient client
-- @
--
-- = Feedback Loop
--
-- Claude-verify captures when verification fails, building a database of
-- "known issues" that can be injected into future Claude sessions:
--
-- @
-- db <- openDatabase Nothing
--
-- -- Record a verification failure
-- item <- recordVerificationFailure db
--     (Just "my-project")
--     Nothing
--     failedVC
--     failedResult
--     claudeOutput
--
-- -- Export known issues for next session
-- context <- exportForSession db "my-project"
-- @
--
-- = Integration Points
--
-- * __Pre-commit hook__: Run verification before commits
-- * __CI\/CD__: GitHub Action for automated verification
-- * __Editor__: LSP integration (planned)
-- * __Claude Code__: Slash command integration (planned)

module ClaudeVerify
    ( -- * Re-exports
      module ClaudeVerify.Config
    , module ClaudeVerify.EchidnaClient
    , module ClaudeVerify.Feedback
    , module ClaudeVerify.PropertyExtraction
    , module ClaudeVerify.Report

      -- * Parsing
    , parseFile
    , parseString
    , Language(..)
    , detectLanguage

      -- * High-level API
    , verifyFile
    , verifyProject
    , VerifyOptions(..)
    , defaultOptions
    ) where

import ClaudeVerify.Config
import ClaudeVerify.EchidnaClient
import ClaudeVerify.Feedback
import ClaudeVerify.PropertyExtraction
import ClaudeVerify.Report
import ClaudeVerify.Internal.AST
import ClaudeVerify.Internal.Types

import qualified ClaudeVerify.Languages.Rust as Rust
import qualified ClaudeVerify.Languages.Haskell as Haskell
import qualified ClaudeVerify.Languages.Ada as Ada

import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (takeExtension)

-- | Supported languages
data Language
    = LangRust
    | LangHaskell
    | LangAda
    | LangUnknown Text
    deriving stock (Eq, Show)

-- | Detect language from file extension
detectLanguage :: FilePath -> Language
detectLanguage path = case takeExtension path of
    ".rs"   -> LangRust
    ".hs"   -> LangHaskell
    ".lhs"  -> LangHaskell
    ".adb"  -> LangAda
    ".ads"  -> LangAda
    ext     -> LangUnknown (T.pack ext)

-- | Parse a source file
parseFile :: FilePath -> IO (Either Text Module)
parseFile path = do
    content <- TIO.readFile path
    pure $ parseString (detectLanguage path) content

-- | Parse source code
parseString :: Language -> Text -> Either Text Module
parseString lang content = case lang of
    LangRust -> case Rust.parseRust content of
        Left err -> Left $ T.pack $ show err
        Right ast -> Right $ Rust.toGenericAST ast

    LangHaskell -> case Haskell.parseHaskell content of
        Left err -> Left $ T.pack $ show err
        Right ast -> Right $ Haskell.toGenericAST ast

    LangAda -> case Ada.parseAda content of
        Left err -> Left $ T.pack $ show err
        Right ast -> Right $ Ada.toGenericAST ast

    LangUnknown ext -> Left $ "Unsupported language: " <> ext

-- | Verification options
data VerifyOptions = VerifyOptions
    { voProvers      :: [Prover]
    , voTimeoutMs    :: Int
    , voVerbose      :: Bool
    , voFailFast     :: Bool
    , voGenerateReport :: Bool
    } deriving stock (Eq, Show)

-- | Default options
defaultOptions :: VerifyOptions
defaultOptions = VerifyOptions
    { voProvers = smtProvers
    , voTimeoutMs = 30000
    , voVerbose = False
    , voFailFast = False
    , voGenerateReport = True
    }

-- | Verify a single file
verifyFile :: EchidnaClient -> FilePath -> VerifyOptions -> IO (Either Text [VerificationResult])
verifyFile client path opts = do
    parseResult <- parseFile path
    case parseResult of
        Left err -> pure $ Left err
        Right ast -> do
            let vcs = extractVCs ast
            results <- verifyWithProvers client vcs (voProvers opts)
            pure $ Right $ map echidnaToResult results

-- | Verify all files in a project
verifyProject :: EchidnaClient -> [FilePath] -> VerifyOptions -> IO [Either Text [VerificationResult]]
verifyProject client paths opts = forM paths $ \path ->
    verifyFile client path opts

-- | Convert ECHIDNA result to our result type
echidnaToResult :: EchidnaResult -> VerificationResult
echidnaToResult er = VerificationResult
    { vrTaskId = erTaskId er
    , vrVCId = erVCId er
    , vrResults = map proverResponseToResult (erProverResponses er)
    , vrAggregateStatus = erAggregateStatus er
    , vrConfidence = erConfidence er
    , vrTimestamp = read $ T.unpack $ erTimestamp er  -- Simplified
    }

-- | Convert prover response
proverResponseToResult :: ProverResponse -> ProverResult
proverResponseToResult pr = ProverResult
    { prProver = T.pack $ show $ prpProver pr
    , prStatus = prpStatus pr
    , prTimeMs = prpTimeMs pr
    , prCertificate = ProofCertificate <$> prpCertificate pr
    , prCounterexample = Counterexample <$> prpCounterexample pr
    , prRawOutput = prpRawOutput pr
    }
