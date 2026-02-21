{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.EchidnaClient
-- Description : Client for ECHIDNA neurosymbolic theorem proving platform
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- This module provides the integration between claude-verify and ECHIDNA
-- (Extensible Cognitive Hybrid Intelligence for Deductive Neural Assistance).
--
-- ECHIDNA provides access to 12 theorem provers:
--
-- * SMT Solvers: Z3, CVC5
-- * Theorem Provers: Lean 4, Coq, Agda, Isabelle, HOL Light, Mizar
-- * First-Order: Vampire, E
-- * Model Checkers: TLC (TLA+), Alloy Analyzer
--
-- Plus neurosymbolic integration via DeepProbLog and OpenCyc.

module ClaudeVerify.EchidnaClient
    ( -- * Client
      EchidnaClient(..)
    , EchidnaConfig(..)
    , createClient
    , closeClient

      -- * Verification
    , verify
    , verifyBatch
    , verifyWithProvers

      -- * Prover Selection
    , Prover(..)
    , allProvers
    , smtProvers
    , theoremProvers
    , modelCheckers

      -- * Results
    , EchidnaResult(..)
    , ProverResponse(..)
    , NeurosymbolicAssessment(..)

      -- * Portfolio Strategies
    , PortfolioStrategy(..)
    , defaultStrategy
    , fastStrategy
    , thoroughStrategy
    ) where

import ClaudeVerify.Internal.Types

import Control.Exception (bracket, try, SomeException)
import Control.Monad (forM, when)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.Process
import System.IO (hGetContents, hClose)

-- | Available provers in ECHIDNA
data Prover
    = Z3          -- ^ SMT solver from Microsoft Research
    | CVC5        -- ^ SMT solver from Stanford/Iowa
    | Lean4       -- ^ Dependent type theory prover
    | Coq         -- ^ Calculus of Inductive Constructions
    | Agda        -- ^ Dependently typed functional language
    | Isabelle    -- ^ Generic proof assistant (HOL)
    | HOLLight    -- ^ HOL implementation
    | Mizar       -- ^ Set-theoretic prover
    | Vampire     -- ^ First-order ATP
    | EProver     -- ^ Equational theorem prover
    | TLC         -- ^ TLA+ model checker
    | AlloyAnalyzer -- ^ Relational model finder
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | All available provers
allProvers :: [Prover]
allProvers = [minBound..maxBound]

-- | SMT solvers (fast, decidable fragments)
smtProvers :: [Prover]
smtProvers = [Z3, CVC5]

-- | Theorem provers (powerful, may not terminate)
theoremProvers :: [Prover]
theoremProvers = [Lean4, Coq, Agda, Isabelle, HOLLight, Mizar, Vampire, EProver]

-- | Model checkers (state space exploration)
modelCheckers :: [Prover]
modelCheckers = [TLC, AlloyAnalyzer]

-- | Portfolio execution strategy
data PortfolioStrategy
    = Sequential              -- ^ Run provers one at a time
    | Parallel Int            -- ^ Run N provers in parallel
    | Racing                  -- ^ First success wins
    | Voting Int              -- ^ Require N provers to agree
    | Cascade                 -- ^ Try fast provers first, escalate
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Default strategy: cascade from fast to thorough
defaultStrategy :: PortfolioStrategy
defaultStrategy = Cascade

-- | Fast strategy: SMT only, parallel
fastStrategy :: PortfolioStrategy
fastStrategy = Parallel 2

-- | Thorough strategy: all provers, voting
thoroughStrategy :: PortfolioStrategy
thoroughStrategy = Voting 3

-- | ECHIDNA configuration
data EchidnaConfig = EchidnaConfig
    { ecBinaryPath    :: FilePath           -- ^ Path to echidna binary
    , ecDefaultProvers :: [Prover]          -- ^ Default prover set
    , ecTimeoutMs     :: Int                -- ^ Per-prover timeout
    , ecStrategy      :: PortfolioStrategy  -- ^ Portfolio strategy
    , ecUseTempFiles  :: Bool               -- ^ Use temp files for input
    , ecVerbose       :: Bool               -- ^ Verbose output
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Default configuration
defaultConfig :: EchidnaConfig
defaultConfig = EchidnaConfig
    { ecBinaryPath    = "echidna"
    , ecDefaultProvers = [Z3, CVC5, Lean4]
    , ecTimeoutMs     = 30000
    , ecStrategy      = defaultStrategy
    , ecUseTempFiles  = True
    , ecVerbose       = False
    }

-- | ECHIDNA client handle
data EchidnaClient = EchidnaClient
    { clientConfig  :: EchidnaConfig
    , clientProcess :: Maybe ProcessHandle  -- ^ For persistent connection
    } deriving stock (Generic)

-- | Create a new ECHIDNA client
createClient :: EchidnaConfig -> IO EchidnaClient
createClient config = do
    -- Check if ECHIDNA is available
    (exitCode, _, _) <- readProcessWithExitCode (ecBinaryPath config) ["--version"] ""
    case exitCode of
        ExitSuccess -> pure EchidnaClient
            { clientConfig = config
            , clientProcess = Nothing
            }
        ExitFailure _ -> do
            -- Try to find echidna in common locations
            let fallbacks = ["/usr/local/bin/echidna", "~/.local/bin/echidna"]
            pure EchidnaClient
                { clientConfig = config { ecBinaryPath = head fallbacks }
                , clientProcess = Nothing
                }

-- | Close the client
closeClient :: EchidnaClient -> IO ()
closeClient client = case clientProcess client of
    Just ph -> terminateProcess ph
    Nothing -> pure ()

-- | Response from a single prover
data ProverResponse = ProverResponse
    { prpProver        :: Prover
    , prpStatus        :: ProverStatus
    , prpTimeMs        :: Int
    , prpCertificate   :: Maybe Text
    , prpCounterexample :: Maybe Text
    , prpRawOutput     :: Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Neurosymbolic assessment from DeepProbLog/OpenCyc
data NeurosymbolicAssessment = NeurosymbolicAssessment
    { nsaDeepProbLogConfidence :: Double
    , nsaSymbolicCoverage      :: Double
    , nsaOpenCycRelevance      :: Double
    , nsaExplanation           :: Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Result from ECHIDNA verification
data EchidnaResult = EchidnaResult
    { erTaskId              :: TaskId
    , erVCId                :: VCId
    , erProverResponses     :: [ProverResponse]
    , erAggregateStatus     :: ProverStatus
    , erConfidence          :: Double
    , erNeurosymbolic       :: Maybe NeurosymbolicAssessment
    , erTimestamp           :: Text
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Verify a single verification condition
verify :: EchidnaClient -> VerificationCondition -> IO EchidnaResult
verify client vc = verifyWithProvers client [vc] (ecDefaultProvers $ clientConfig client) >>= \case
    (r:_) -> pure r
    [] -> do
        now <- getCurrentTime
        taskId <- TaskId <$> UUID.nextRandom
        pure EchidnaResult
            { erTaskId = taskId
            , erVCId = vcId vc
            , erProverResponses = []
            , erAggregateStatus = Error "No results returned"
            , erConfidence = 0.0
            , erNeurosymbolic = Nothing
            , erTimestamp = T.pack $ show now
            }

-- | Verify multiple VCs with default provers
verifyBatch :: EchidnaClient -> [VerificationCondition] -> IO [EchidnaResult]
verifyBatch client vcs = verifyWithProvers client vcs (ecDefaultProvers $ clientConfig client)

-- | Verify with specific provers
verifyWithProvers :: EchidnaClient -> [VerificationCondition] -> [Prover] -> IO [EchidnaResult]
verifyWithProvers client vcs provers = do
    let config = clientConfig client
    results <- forM vcs $ \vc -> do
        taskId <- TaskId <$> UUID.nextRandom
        now <- getCurrentTime

        -- Build ECHIDNA command
        let formula = vcfContent $ vcFormula vc
            format = case vcfFormat $ vcFormula vc of
                SMTLIB2 -> "smtlib2"
                TPTP    -> "tptp"
                Lean4   -> "lean4"
                Coq     -> "coq"
                Agda    -> "agda"
                _       -> "smtlib2"

        let proverArgs = ["--provers", T.unpack $ T.intercalate "," (map (T.pack . show) provers)]
            timeoutArgs = ["--timeout", show (ecTimeoutMs config)]
            formatArgs = ["--format", format]

        -- Run ECHIDNA
        result <- try $ runEchidna config (proverArgs ++ timeoutArgs ++ formatArgs) formula

        case result of
            Right echidnaOutput -> parseEchidnaOutput taskId (vcId vc) echidnaOutput
            Left (e :: SomeException) -> pure EchidnaResult
                { erTaskId = taskId
                , erVCId = vcId vc
                , erProverResponses = []
                , erAggregateStatus = Error (T.pack $ show e)
                , erConfidence = 0.0
                , erNeurosymbolic = Nothing
                , erTimestamp = T.pack $ show now
                }

    pure results

-- | Run ECHIDNA binary
runEchidna :: EchidnaConfig -> [String] -> Text -> IO ByteString
runEchidna config args formula = do
    let cmd = ecBinaryPath config
        allArgs = "prove" : args

    -- Create process with stdin
    let procSpec = (proc cmd allArgs)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }

    bracket
        (createProcess procSpec)
        (\(_, _, _, ph) -> terminateProcess ph)
        $ \(Just hin, Just hout, Just herr, _ph) -> do
            -- Send formula to stdin
            BL.hPut hin (BL.fromStrict $ TE.encodeUtf8 formula)
            hClose hin

            -- Read output
            output <- hGetContents hout
            _errors <- hGetContents herr
            pure $ BL.fromStrict $ TE.encodeUtf8 $ T.pack output

-- | Parse ECHIDNA JSON output
parseEchidnaOutput :: TaskId -> VCId -> ByteString -> IO EchidnaResult
parseEchidnaOutput taskId vcid output = do
    now <- getCurrentTime
    case decode output of
        Just result -> pure result
        Nothing -> pure EchidnaResult
            { erTaskId = taskId
            , erVCId = vcid
            , erProverResponses = []
            , erAggregateStatus = Error "Failed to parse ECHIDNA output"
            , erConfidence = 0.0
            , erNeurosymbolic = Nothing
            , erTimestamp = T.pack $ show now
            }

-- | Convert prover name to string
proverToString :: Prover -> String
proverToString = \case
    Z3 -> "z3"
    CVC5 -> "cvc5"
    Lean4 -> "lean4"
    Coq -> "coq"
    Agda -> "agda"
    Isabelle -> "isabelle"
    HOLLight -> "hollight"
    Mizar -> "mizar"
    Vampire -> "vampire"
    EProver -> "eprover"
    TLC -> "tlc"
    AlloyAnalyzer -> "alloy"
