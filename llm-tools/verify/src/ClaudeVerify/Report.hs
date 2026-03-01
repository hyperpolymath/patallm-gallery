{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.Report
-- Description : Report generation for verification results
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- Generates verification reports in multiple formats:
--
-- * Markdown (for humans)
-- * JSON (for tools)
-- * SARIF (for IDE integration)
-- * Terminal (colorized output)

module ClaudeVerify.Report
    ( -- * Report Types
      Report(..)
    , ReportFormat(..)
    , ReportSection(..)

      -- * Generation
    , generateReport
    , formatReport

      -- * Output
    , writeReport
    , printReport
    ) where

import ClaudeVerify.Internal.Types
import ClaudeVerify.EchidnaClient (EchidnaResult(..), ProverResponse(..))

import Data.Aeson (ToJSON, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)

-- | Report format
data ReportFormat
    = Markdown
    | JSON
    | SARIF        -- ^ Static Analysis Results Interchange Format
    | Terminal
    deriving stock (Eq, Show)

-- | A section of the report
data ReportSection
    = SectionHeader Text
    | SectionSummary
        { sumTotal   :: Int
        , sumPassed  :: Int
        , sumFailed  :: Int
        , sumUnknown :: Int
        }
    | SectionVCResult VCId VerificationResult
    | SectionEchidnaResult EchidnaResult
    | SectionKnownIssues [KnownIssue]
    | SectionFooter
    deriving stock (Show, Generic)

-- | A verification report
data Report = Report
    { rptTitle     :: Text
    , rptTimestamp :: UTCTime
    , rptProject   :: Maybe Text
    , rptSections  :: [ReportSection]
    , rptRawResults :: [VerificationResult]
    } deriving stock (Show, Generic)

-- | Generate a report from verification results
generateReport :: Text -> Maybe Text -> [VerificationResult] -> IO Report
generateReport title project results = do
    now <- getCurrentTime

    let total = length results
        passed = length $ filter isProved results
        failed = length $ filter isCounterexample results
        unknown = total - passed - failed

    let sections =
            [ SectionHeader title
            , SectionSummary total passed failed unknown
            ] ++
            map (\r -> SectionVCResult (vrVCId r) r) results ++
            [ SectionFooter ]

    pure Report
        { rptTitle = title
        , rptTimestamp = now
        , rptProject = project
        , rptSections = sections
        , rptRawResults = results
        }
  where
    isProved r = case vrAggregateStatus r of
        Proved -> True
        _ -> False

    isCounterexample r = case vrAggregateStatus r of
        Counterexample -> True
        _ -> False

-- | Format a report to the specified format
formatReport :: ReportFormat -> Report -> Text
formatReport fmt rpt = case fmt of
    Markdown -> formatMarkdown rpt
    JSON -> formatJSON rpt
    SARIF -> formatSARIF rpt
    Terminal -> formatTerminal rpt

-- | Format as Markdown
formatMarkdown :: Report -> Text
formatMarkdown rpt = T.unlines $
    [ "# " <> rptTitle rpt
    , ""
    , "*Generated: " <> formatTimestamp (rptTimestamp rpt) <> "*"
    , ""
    ] ++
    concatMap sectionToMarkdown (rptSections rpt)
  where
    sectionToMarkdown = \case
        SectionHeader t ->
            ["## " <> t, ""]

        SectionSummary total passed failed unknown ->
            [ "## Summary"
            , ""
            , "| Metric | Count |"
            , "|--------|-------|"
            , "| Total VCs | " <> showT total <> " |"
            , "| Proved | " <> showT passed <> " |"
            , "| Counterexamples | " <> showT failed <> " |"
            , "| Unknown | " <> showT unknown <> " |"
            , ""
            , progressBar passed total
            , ""
            ]

        SectionVCResult vcid result ->
            [ "### " <> unVCId vcid
            , ""
            , "**Status**: " <> statusToEmoji (vrAggregateStatus result) <>
                " " <> showT (vrAggregateStatus result)
            , ""
            , "**Confidence**: " <> showT (round (vrConfidence result * 100) :: Int) <> "%"
            , ""
            , "#### Prover Results"
            , ""
            ] ++ resultTable (vrResults result) ++
            [""]

        SectionEchidnaResult er ->
            [ "### " <> unVCId (erVCId er)
            , ""
            , "**Status**: " <> statusToEmoji (erAggregateStatus er)
            , "**Confidence**: " <> showT (round (erConfidence er * 100) :: Int) <> "%"
            , ""
            ]

        SectionKnownIssues issues ->
            [ "## Known Issues"
            , ""
            ] ++ map issueToMarkdown issues ++
            [""]

        SectionFooter ->
            [ "---"
            , ""
            , "*Report generated by claude-verify with ECHIDNA backend*"
            ]

    resultTable results =
        [ "| Prover | Status | Time (ms) |"
        , "|--------|--------|-----------|"
        ] ++ map resultRow results

    resultRow pr =
        "| " <> prProver pr <> " | " <>
        statusToEmoji (prStatus pr) <> " " <> showT (prStatus pr) <>
        " | " <> showT (prTimeMs pr) <> " |"

    issueToMarkdown ki =
        "- **" <> kiPattern ki <> "**: " <> kiDescription ki

    statusToEmoji = \case
        Proved -> "+"
        Counterexample -> "X"
        Unknown -> "?"
        Error _ -> "!"

    progressBar done total =
        let pct = if total == 0 then 0 else (done * 100) `div` total
            filled = pct `div` 5
            empty = 20 - filled
            bar = T.replicate filled "#" <> T.replicate empty "-"
        in "[" <> bar <> "] " <> showT pct <> "% proved"

-- | Format as JSON
formatJSON :: Report -> Text
formatJSON rpt =
    let json = AesonPretty.encodePretty $ reportToJSON rpt
    in T.pack $ BLC.unpack json

-- | Convert report to JSON-serializable structure
reportToJSON :: Report -> Aeson.Value
reportToJSON rpt = Aeson.object
    [ ("title", Aeson.String $ rptTitle rpt)
    , ("timestamp", Aeson.String $ formatTimestamp $ rptTimestamp rpt)
    , ("project", maybe Aeson.Null Aeson.String $ rptProject rpt)
    , ("results", Aeson.toJSON $ rptRawResults rpt)
    ]

-- | Format as SARIF (Static Analysis Results Interchange Format)
formatSARIF :: Report -> Text
formatSARIF rpt =
    let sarif = Aeson.object
            [ ("version", "2.1.0")
            , ("$schema", "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json")
            , ("runs", Aeson.toJSON [runObject])
            ]
        runObject = Aeson.object
            [ ("tool", toolObject)
            , ("results", Aeson.toJSON $ map resultToSarif $ rptRawResults rpt)
            ]
        toolObject = Aeson.object
            [ ("driver", Aeson.object
                [ ("name", "claude-verify")
                , ("version", "0.1.0")
                , ("informationUri", "https://github.com/Hyperpolymath/llm-verify")
                ])
            ]
    in T.pack $ BLC.unpack $ AesonPretty.encodePretty sarif

-- | Convert a result to SARIF result object
resultToSarif :: VerificationResult -> Aeson.Value
resultToSarif result = Aeson.object
    [ ("ruleId", Aeson.String $ unVCId $ vrVCId result)
    , ("level", Aeson.String $ statusToLevel $ vrAggregateStatus result)
    , ("message", Aeson.object
        [ ("text", Aeson.String $ statusToMessage $ vrAggregateStatus result)
        ])
    ]
  where
    statusToLevel = \case
        Proved -> "none"
        Counterexample -> "error"
        Unknown -> "warning"
        Error _ -> "error"

    statusToMessage = \case
        Proved -> "Verification condition proved"
        Counterexample -> "Counterexample found"
        Unknown -> "Verification inconclusive"
        Error e -> "Verification error: " <> e

-- | Format for terminal (with colors if supported)
formatTerminal :: Report -> Text
formatTerminal rpt = T.unlines $
    [ bold (rptTitle rpt)
    , "Generated: " <> formatTimestamp (rptTimestamp rpt)
    , ""
    ] ++
    concatMap sectionToTerminal (rptSections rpt)
  where
    sectionToTerminal = \case
        SectionSummary total passed failed unknown ->
            [ "Summary:"
            , "  " <> green (showT passed) <> " proved / " <>
                    red (showT failed) <> " failed / " <>
                    yellow (showT unknown) <> " unknown"
            , "  (Total: " <> showT total <> ")"
            , ""
            ]

        SectionVCResult vcid result ->
            [ dim (unVCId vcid) <> ": " <>
                statusColored (vrAggregateStatus result)
            ]

        SectionHeader t ->
            ["", bold t, ""]

        SectionFooter ->
            ["", dim "claude-verify + ECHIDNA", ""]

        _ -> []

    statusColored = \case
        Proved -> green "PROVED"
        Counterexample -> red "COUNTEREXAMPLE"
        Unknown -> yellow "UNKNOWN"
        Error e -> red ("ERROR: " <> e)

    -- ANSI color codes (simplified)
    bold t = "\ESC[1m" <> t <> "\ESC[0m"
    dim t = "\ESC[2m" <> t <> "\ESC[0m"
    green t = "\ESC[32m" <> t <> "\ESC[0m"
    red t = "\ESC[31m" <> t <> "\ESC[0m"
    yellow t = "\ESC[33m" <> t <> "\ESC[0m"

-- | Write report to file
writeReport :: FilePath -> ReportFormat -> Report -> IO ()
writeReport path fmt rpt = TIO.writeFile path (formatReport fmt rpt)

-- | Print report to stdout
printReport :: ReportFormat -> Report -> IO ()
printReport fmt rpt = TIO.putStrLn (formatReport fmt rpt)

-- | Format timestamp
formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"

-- | Show as Text
showT :: Show a => a -> Text
showT = T.pack . show
