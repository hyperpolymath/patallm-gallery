{-# LANGUAGE ApplicativeDo #-}
-- |
-- Module      : Main
-- Description : CLI for claude-verify
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT

module Main (main) where

import ClaudeVerify

import Control.Monad (when, forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- | Command line options
data Options = Options
    { optCommand :: Command
    , optVerbose :: Bool
    }

-- | Available commands
data Command
    = CmdVerify VerifyOpts
    | CmdCheck CheckOpts
    | CmdFeedback FeedbackCmd
    | CmdInit
    | CmdStats

-- | Verify command options
data VerifyOpts = VerifyOpts
    { vFiles     :: [FilePath]
    , vProvers   :: [Text]
    , vTimeout   :: Int
    , vFormat    :: Text
    , vOutput    :: Maybe FilePath
    , vFailFast  :: Bool
    }

-- | Check command options (quick checks without ECHIDNA)
data CheckOpts = CheckOpts
    { cFiles  :: [FilePath]
    , cLevel  :: Text
    }

-- | Feedback subcommands
data FeedbackCmd
    = FbRecord Text Text Text  -- ^ claude_output, issue, correction
    | FbList Int               -- ^ limit
    | FbExport (Maybe FilePath)
    | FbStats

-- | Parser for options
optionsParser :: Parser Options
optionsParser = Options
    <$> commandParser
    <*> switch (long "verbose" <> short 'v' <> help "Verbose output")

-- | Parser for commands
commandParser :: Parser Command
commandParser = subparser
    ( command "verify"
        (info (CmdVerify <$> verifyOptsParser)
            (progDesc "Verify files with ECHIDNA provers"))
   <> command "check"
        (info (CmdCheck <$> checkOptsParser)
            (progDesc "Quick checks without formal verification"))
   <> command "feedback"
        (info (CmdFeedback <$> feedbackCmdParser)
            (progDesc "Manage feedback database"))
   <> command "init"
        (info (pure CmdInit)
            (progDesc "Initialize .claude-context.toml"))
   <> command "stats"
        (info (pure CmdStats)
            (progDesc "Show verification statistics"))
    )

-- | Verify options parser
verifyOptsParser :: Parser VerifyOpts
verifyOptsParser = VerifyOpts
    <$> many (argument str (metavar "FILES..."))
    <*> many (strOption
        (long "prover" <> short 'p' <> metavar "PROVER"
        <> help "Prover to use (z3, cvc5, lean4, coq, agda, ...)"))
    <*> option auto
        (long "timeout" <> short 't' <> metavar "MS"
        <> value 30000 <> help "Timeout per prover in milliseconds")
    <*> strOption
        (long "format" <> short 'f' <> metavar "FORMAT"
        <> value "terminal" <> help "Output format: terminal, markdown, json, sarif")
    <*> optional (strOption
        (long "output" <> short 'o' <> metavar "FILE"
        <> help "Output file (default: stdout)"))
    <*> switch
        (long "fail-fast" <> help "Stop on first failure")

-- | Check options parser
checkOptsParser :: Parser CheckOpts
checkOptsParser = CheckOpts
    <$> many (argument str (metavar "FILES..."))
    <*> strOption
        (long "level" <> short 'l' <> metavar "LEVEL"
        <> value "standard" <> help "Check level: quick, standard, rigorous, exhaustive")

-- | Feedback command parser
feedbackCmdParser :: Parser FeedbackCmd
feedbackCmdParser = subparser
    ( command "record"
        (info recordParser (progDesc "Record feedback"))
   <> command "list"
        (info listParser (progDesc "List recent feedback"))
   <> command "export"
        (info exportParser (progDesc "Export feedback"))
   <> command "stats"
        (info (pure FbStats) (progDesc "Feedback statistics"))
    )
  where
    recordParser = FbRecord
        <$> strOption (long "output" <> help "Claude's output")
        <*> strOption (long "issue" <> help "What was wrong")
        <*> strOption (long "correction" <> help "The correction")

    listParser = FbList
        <$> option auto (long "limit" <> short 'n' <> value 10)

    exportParser = FbExport
        <$> optional (strOption (long "file" <> short 'o'))

-- | Main entry point
main :: IO ()
main = do
    opts <- execParser parserInfo
    runCommand opts

-- | Parser info
parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Real verification for LLM-generated code"
   <> header "claude-verify - formal verification via ECHIDNA"
    )

-- | Run the selected command
runCommand :: Options -> IO ()
runCommand (Options cmd verbose) = case cmd of
    CmdVerify vOpts -> runVerify vOpts verbose
    CmdCheck cOpts -> runCheck cOpts verbose
    CmdFeedback fbCmd -> runFeedback fbCmd verbose
    CmdInit -> runInit verbose
    CmdStats -> runStats verbose

-- | Run verify command
runVerify :: VerifyOpts -> Bool -> IO ()
runVerify opts verbose = do
    when verbose $ hPutStrLn stderr "Loading configuration..."

    configResult <- loadConfig
    let config = either (const defaultConfig) id configResult

    when verbose $ hPutStrLn stderr "Connecting to ECHIDNA..."

    let echidnaConfig = EchidnaConfig
            { ecBinaryPath = "echidna"
            , ecDefaultProvers = if null (vProvers opts)
                then [Z3, CVC5]
                else map textToProver (vProvers opts)
            , ecTimeoutMs = vTimeout opts
            , ecStrategy = defaultStrategy
            , ecUseTempFiles = True
            , ecVerbose = verbose
            }

    client <- createClient echidnaConfig

    when verbose $ hPutStrLn stderr "Verifying files..."

    results <- verifyProject client (vFiles opts) VerifyOptions
        { voProvers = ecDefaultProvers echidnaConfig
        , voTimeoutMs = vTimeout opts
        , voVerbose = verbose
        , voFailFast = vFailFast opts
        , voGenerateReport = True
        }

    closeClient client

    -- Collect all results
    let allResults = concat [rs | Right rs <- results]
        errors = [e | Left e <- results]

    forM_ errors $ \e ->
        hPutStrLn stderr $ "Error: " <> T.unpack e

    -- Generate report
    report <- generateReport "Verification Results" (Just $ psName $ cfgProject config) allResults

    let fmt = case vFormat opts of
            "markdown" -> Markdown
            "json" -> JSON
            "sarif" -> SARIF
            _ -> Terminal

    case vOutput opts of
        Just path -> writeReport path fmt report
        Nothing -> printReport fmt report

    -- Exit with failure if any counterexamples
    let hasFailures = any (\r -> vrAggregateStatus r == Counterexample) allResults
    if hasFailures then exitFailure else exitSuccess

-- | Run check command
runCheck :: CheckOpts -> Bool -> IO ()
runCheck opts verbose = do
    when verbose $ hPutStrLn stderr "Running quick checks..."

    forM_ (cFiles opts) $ \path -> do
        when verbose $ hPutStrLn stderr $ "Checking " <> path <> "..."
        result <- parseFile path
        case result of
            Left err -> do
                TIO.hPutStrLn stderr $ "Parse error: " <> err
                exitFailure
            Right ast -> do
                let vcs = extractVCs ast
                TIO.putStrLn $ "Found " <> T.pack (show $ length vcs) <> " verification conditions in " <> T.pack path

-- | Run feedback command
runFeedback :: FeedbackCmd -> Bool -> IO ()
runFeedback cmd _verbose = do
    db <- openDatabase Nothing

    case cmd of
        FbRecord claudeOut issue correction -> do
            _ <- recordUserCorrection db Nothing Nothing
                claudeOut issue correction LogicError
            TIO.putStrLn "Feedback recorded"

        FbList limit -> do
            items <- getRecentFeedback db limit
            forM_ items $ \item -> do
                TIO.putStrLn $ "- [" <> categoryToText' (fiCategory item) <> "] " <>
                    T.take 60 (fiActualIssue item)

        FbExport mPath -> do
            context <- exportForSession db "default"
            case mPath of
                Just path -> TIO.writeFile path context
                Nothing -> TIO.putStrLn context

        FbStats -> do
            stats <- getStats db
            TIO.putStrLn $ "Total feedback items: " <> T.pack (show $ fsTotal stats)
            TIO.putStrLn $ "Verified: " <> T.pack (show $ fsVerifiedCount stats)
            TIO.putStrLn $ "Shared with Anthropic: " <> T.pack (show $ fsSharedCount stats)

    closeDatabase db

-- | Run init command
runInit :: Bool -> IO ()
runInit _verbose = do
    TIO.writeFile ".claude-context.toml" defaultContextContent
    TIO.putStrLn "Created .claude-context.toml"

-- | Run stats command
runStats :: Bool -> IO ()
runStats _verbose = do
    db <- openDatabase Nothing
    stats <- getStats db
    TIO.putStrLn "=== Feedback Statistics ==="
    TIO.putStrLn $ "Total: " <> T.pack (show $ fsTotal stats)
    TIO.putStrLn ""
    TIO.putStrLn "By Category:"
    forM_ (fsByCategory stats) $ \(cat, n) ->
        TIO.putStrLn $ "  " <> categoryToText' cat <> ": " <> T.pack (show n)
    TIO.putStrLn ""
    TIO.putStrLn "By Source:"
    forM_ (fsBySource stats) $ \(src, n) ->
        TIO.putStrLn $ "  " <> sourceToText' src <> ": " <> T.pack (show n)
    closeDatabase db

-- | Convert text to prover
textToProver :: Text -> Prover
textToProver t = case T.toLower t of
    "z3" -> Z3
    "cvc5" -> CVC5
    "lean4" -> Lean4
    "lean" -> Lean4
    "coq" -> Coq
    "agda" -> Agda
    "isabelle" -> Isabelle
    "hollight" -> HOLLight
    "mizar" -> Mizar
    "vampire" -> Vampire
    "eprover" -> EProver
    "tlc" -> TLC
    "alloy" -> AlloyAnalyzer
    _ -> Z3

-- | Category to text (simplified)
categoryToText' :: FeedbackCategory -> Text
categoryToText' = \case
    SyntaxError -> "syntax"
    TypeError -> "type"
    LogicError -> "logic"
    SecurityFlaw -> "security"
    PerformanceIssue -> "perf"
    SemanticMismatch -> "semantic"
    StyleViolation -> "style"
    Other t -> t

-- | Source to text
sourceToText' :: FeedbackSource -> Text
sourceToText' = \case
    Compiler -> "compiler"
    TypeChecker -> "typechecker"
    Linter -> "linter"
    TestSuite -> "test"
    FormalProver -> "prover"
    StaticAnalyzer -> "analyzer"
    UserReported -> "user"
    RuntimeError -> "runtime"

-- | Default context file content
defaultContextContent :: Text
defaultContextContent = T.unlines
    [ "# Claude Context Configuration"
    , "# This file configures claude-verify for your project"
    , ""
    , "[project]"
    , "name = \"my-project\""
    , "languages = [\"rust\"]"
    , "safety_level = \"standard\"  # low, standard, high, critical"
    , ""
    , "[known_issues]"
    , "# Things Claude has gotten wrong before on THIS project"
    , "items = ["
    , "    # \"Off-by-one in pagination - always use Range not manual arithmetic\","
    , "    # \"This project uses no_std - don't assume alloc is available\","
    , "]"
    , ""
    , "[verification]"
    , "# What MUST pass before Claude says \"done\""
    , "required = [\"compile\", \"lint\", \"test\"]"
    , "optional = [\"audit\", \"miri\", \"doc\"]"
    , ""
    , "[formal_specs]"
    , "# Paths to formal specifications"
    , "tla_plus = []"
    , "alloy = []"
    , "lean = []"
    , "coq = []"
    ]
