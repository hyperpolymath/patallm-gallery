{-# LANGUAGE StrictData #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : ClaudeVerify.Feedback
-- Description : Feedback capture and persistence for LLM learning loop
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- This module implements the feedback loop for capturing Claude's mistakes
-- and corrections, enabling:
--
-- 1. Immediate feedback within a session
-- 2. Persistent feedback across sessions
-- 3. Optional sharing with Anthropic for model improvement
--
-- The feedback system stores:
--
-- * What Claude generated (the problematic output)
-- * What the issue was (category, description)
-- * How it was detected (compiler, test, prover, user)
-- * What the correction was
-- * Whether the correction was verified

module ClaudeVerify.Feedback
    ( -- * Database
      FeedbackDB(..)
    , openDatabase
    , closeDatabase
    , initSchema

      -- * Recording Feedback
    , recordFeedback
    , recordVerificationFailure
    , recordUserCorrection

      -- * Querying Feedback
    , getRecentFeedback
    , getFeedbackByProject
    , getFeedbackByCategory
    , getKnownIssues

      -- * Export
    , exportForSession
    , exportForAnthropic
    , toKnownIssue

      -- * Statistics
    , FeedbackStats(..)
    , getStats
    ) where

import ClaudeVerify.Internal.Types

import Control.Exception (bracket)
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

-- | Handle to the feedback database
newtype FeedbackDB = FeedbackDB { dbConnection :: Connection }

-- | Open the feedback database
openDatabase :: Maybe FilePath -> IO FeedbackDB
openDatabase mpath = do
    path <- case mpath of
        Just p -> pure p
        Nothing -> do
            home <- getHomeDirectory
            let dir = home </> ".claude-verify"
            createDirectoryIfMissing True dir
            pure $ dir </> "feedback.db"

    conn <- open path
    let db = FeedbackDB conn
    initSchema db
    pure db

-- | Close the database
closeDatabase :: FeedbackDB -> IO ()
closeDatabase (FeedbackDB conn) = close conn

-- | Initialize database schema
initSchema :: FeedbackDB -> IO ()
initSchema (FeedbackDB conn) = do
    execute_ conn
        "CREATE TABLE IF NOT EXISTS feedback (\
        \  id TEXT PRIMARY KEY,\
        \  timestamp TEXT NOT NULL,\
        \  project TEXT,\
        \  session_id TEXT,\
        \  category TEXT NOT NULL,\
        \  subcategory TEXT,\
        \  source_file TEXT,\
        \  source_line_start INTEGER,\
        \  source_line_end INTEGER,\
        \  claude_output TEXT NOT NULL,\
        \  actual_issue TEXT NOT NULL,\
        \  correction TEXT,\
        \  detected_by TEXT NOT NULL,\
        \  verified INTEGER NOT NULL DEFAULT 0,\
        \  shared_with_anthropic INTEGER NOT NULL DEFAULT 0\
        \)"

    execute_ conn
        "CREATE INDEX IF NOT EXISTS idx_feedback_project ON feedback(project)"

    execute_ conn
        "CREATE INDEX IF NOT EXISTS idx_feedback_category ON feedback(category)"

    execute_ conn
        "CREATE INDEX IF NOT EXISTS idx_feedback_timestamp ON feedback(timestamp)"

-- | SQLite row representation
data FeedbackRow = FeedbackRow
    { frId :: Text
    , frTimestamp :: Text
    , frProject :: Maybe Text
    , frSessionId :: Maybe Text
    , frCategory :: Text
    , frSubcategory :: Maybe Text
    , frSourceFile :: Maybe Text
    , frSourceLineStart :: Maybe Int
    , frSourceLineEnd :: Maybe Int
    , frClaudeOutput :: Text
    , frActualIssue :: Text
    , frCorrection :: Maybe Text
    , frDetectedBy :: Text
    , frVerified :: Bool
    , frSharedWithAnthropic :: Bool
    } deriving stock (Show, Generic)

instance FromRow FeedbackRow where
    fromRow = FeedbackRow
        <$> field <*> field <*> field <*> field <*> field
        <*> field <*> field <*> field <*> field <*> field
        <*> field <*> field <*> field <*> field <*> field

instance ToRow FeedbackRow where
    toRow fr =
        [ toField (frId fr)
        , toField (frTimestamp fr)
        , toField (frProject fr)
        , toField (frSessionId fr)
        , toField (frCategory fr)
        , toField (frSubcategory fr)
        , toField (frSourceFile fr)
        , toField (frSourceLineStart fr)
        , toField (frSourceLineEnd fr)
        , toField (frClaudeOutput fr)
        , toField (frActualIssue fr)
        , toField (frCorrection fr)
        , toField (frDetectedBy fr)
        , toField (frVerified fr)
        , toField (frSharedWithAnthropic fr)
        ]

-- | Convert row to feedback item
rowToFeedback :: FeedbackRow -> Maybe FeedbackItem
rowToFeedback fr = do
    uuid <- UUID.fromText (frId fr)
    let category = textToCategory (frCategory fr)
        source = textToSource (frDetectedBy fr)
        sessionId = frSessionId fr >>= fmap SessionId . UUID.fromText

    pure FeedbackItem
        { fiId = uuid
        , fiTimestamp = read (T.unpack $ frTimestamp fr)  -- Simplified
        , fiProject = frProject fr
        , fiSessionId = sessionId
        , fiCategory = category
        , fiSubcategory = frSubcategory fr
        , fiSourceFile = T.unpack <$> frSourceFile fr
        , fiSourceLines = case (frSourceLineStart fr, frSourceLineEnd fr) of
            (Just s, Just e) -> Just (s, e)
            _ -> Nothing
        , fiClaudeOutput = frClaudeOutput fr
        , fiActualIssue = frActualIssue fr
        , fiCorrection = frCorrection fr
        , fiDetectedBy = source
        , fiVerified = frVerified fr
        , fiSharedWithAnthropic = frSharedWithAnthropic fr
        }

-- | Convert feedback item to row
feedbackToRow :: FeedbackItem -> FeedbackRow
feedbackToRow fi = FeedbackRow
    { frId = UUID.toText (fiId fi)
    , frTimestamp = T.pack $ show (fiTimestamp fi)
    , frProject = fiProject fi
    , frSessionId = fmap (UUID.toText . unSessionId) (fiSessionId fi)
    , frCategory = categoryToText (fiCategory fi)
    , frSubcategory = fiSubcategory fi
    , frSourceFile = T.pack <$> fiSourceFile fi
    , frSourceLineStart = fmap fst (fiSourceLines fi)
    , frSourceLineEnd = fmap snd (fiSourceLines fi)
    , frClaudeOutput = fiClaudeOutput fi
    , frActualIssue = fiActualIssue fi
    , frCorrection = fiCorrection fi
    , frDetectedBy = sourceToText (fiDetectedBy fi)
    , frVerified = fiVerified fi
    , frSharedWithAnthropic = fiSharedWithAnthropic fi
    }

-- | Category text conversion
categoryToText :: FeedbackCategory -> Text
categoryToText = \case
    SyntaxError -> "syntax_error"
    TypeError -> "type_error"
    LogicError -> "logic_error"
    SecurityFlaw -> "security_flaw"
    PerformanceIssue -> "performance_issue"
    SemanticMismatch -> "semantic_mismatch"
    StyleViolation -> "style_violation"
    Other t -> "other:" <> t

textToCategory :: Text -> FeedbackCategory
textToCategory t
    | t == "syntax_error" = SyntaxError
    | t == "type_error" = TypeError
    | t == "logic_error" = LogicError
    | t == "security_flaw" = SecurityFlaw
    | t == "performance_issue" = PerformanceIssue
    | t == "semantic_mismatch" = SemanticMismatch
    | t == "style_violation" = StyleViolation
    | "other:" `T.isPrefixOf` t = Other (T.drop 6 t)
    | otherwise = Other t

-- | Source text conversion
sourceToText :: FeedbackSource -> Text
sourceToText = \case
    Compiler -> "compiler"
    TypeChecker -> "type_checker"
    Linter -> "linter"
    TestSuite -> "test_suite"
    FormalProver -> "formal_prover"
    StaticAnalyzer -> "static_analyzer"
    UserReported -> "user_reported"
    RuntimeError -> "runtime_error"

textToSource :: Text -> FeedbackSource
textToSource = \case
    "compiler" -> Compiler
    "type_checker" -> TypeChecker
    "linter" -> Linter
    "test_suite" -> TestSuite
    "formal_prover" -> FormalProver
    "static_analyzer" -> StaticAnalyzer
    "user_reported" -> UserReported
    "runtime_error" -> RuntimeError
    _ -> UserReported

-- | Record a feedback item
recordFeedback :: FeedbackDB -> FeedbackItem -> IO ()
recordFeedback (FeedbackDB conn) item = do
    let row = feedbackToRow item
    execute conn
        "INSERT OR REPLACE INTO feedback VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        row

-- | Record a verification failure
recordVerificationFailure
    :: FeedbackDB
    -> Maybe Text           -- ^ Project name
    -> Maybe SessionId      -- ^ Session ID
    -> VerificationCondition -- ^ The VC that failed
    -> VerificationResult   -- ^ The result
    -> Text                 -- ^ Claude's original output
    -> IO FeedbackItem
recordVerificationFailure db project sessionId vc result claudeOutput = do
    uuid <- UUID.nextRandom
    now <- getCurrentTime

    let item = FeedbackItem
            { fiId = uuid
            , fiTimestamp = now
            , fiProject = project
            , fiSessionId = sessionId
            , fiCategory = SemanticMismatch
            , fiSubcategory = Just (vcKind vc)
            , fiSourceFile = Just (slFile $ vcLocation vc)
            , fiSourceLines = Just (slLine $ vcLocation vc, fromMaybe (slLine $ vcLocation vc) (slEndLine $ vcLocation vc))
            , fiClaudeOutput = claudeOutput
            , fiActualIssue = formatVerificationFailure vc result
            , fiCorrection = Nothing
            , fiDetectedBy = FormalProver
            , fiVerified = True
            , fiSharedWithAnthropic = False
            }

    recordFeedback db item
    pure item

-- | Format a verification failure message
formatVerificationFailure :: VerificationCondition -> VerificationResult -> Text
formatVerificationFailure vc result =
    "Verification condition failed: " <> vcDescription vc <> "\n" <>
    "Location: " <> T.pack (slFile $ vcLocation vc) <> ":" <>
        T.pack (show $ slLine $ vcLocation vc) <> "\n" <>
    "Status: " <> T.pack (show $ vrAggregateStatus result) <> "\n" <>
    "Confidence: " <> T.pack (show $ vrConfidence result)

-- | Record a user correction
recordUserCorrection
    :: FeedbackDB
    -> Maybe Text           -- ^ Project name
    -> Maybe SessionId      -- ^ Session ID
    -> Text                 -- ^ Claude's original output
    -> Text                 -- ^ What was wrong
    -> Text                 -- ^ The correction
    -> FeedbackCategory     -- ^ Category
    -> IO FeedbackItem
recordUserCorrection db project sessionId claudeOutput issue correction category = do
    uuid <- UUID.nextRandom
    now <- getCurrentTime

    let item = FeedbackItem
            { fiId = uuid
            , fiTimestamp = now
            , fiProject = project
            , fiSessionId = sessionId
            , fiCategory = category
            , fiSubcategory = Nothing
            , fiSourceFile = Nothing
            , fiSourceLines = Nothing
            , fiClaudeOutput = claudeOutput
            , fiActualIssue = issue
            , fiCorrection = Just correction
            , fiDetectedBy = UserReported
            , fiVerified = False
            , fiSharedWithAnthropic = False
            }

    recordFeedback db item
    pure item

-- | Get recent feedback items
getRecentFeedback :: FeedbackDB -> Int -> IO [FeedbackItem]
getRecentFeedback (FeedbackDB conn) limit = do
    rows <- query conn
        "SELECT * FROM feedback ORDER BY timestamp DESC LIMIT ?"
        (Only limit)
    pure $ catMaybes $ map rowToFeedback rows

-- | Get feedback by project
getFeedbackByProject :: FeedbackDB -> Text -> IO [FeedbackItem]
getFeedbackByProject (FeedbackDB conn) project = do
    rows <- query conn
        "SELECT * FROM feedback WHERE project = ? ORDER BY timestamp DESC"
        (Only project)
    pure $ catMaybes $ map rowToFeedback rows

-- | Get feedback by category
getFeedbackByCategory :: FeedbackDB -> FeedbackCategory -> IO [FeedbackItem]
getFeedbackByCategory (FeedbackDB conn) category = do
    rows <- query conn
        "SELECT * FROM feedback WHERE category = ? ORDER BY timestamp DESC"
        (Only $ categoryToText category)
    pure $ catMaybes $ map rowToFeedback rows

-- | Get known issues for a project
getKnownIssues :: FeedbackDB -> Text -> IO [KnownIssue]
getKnownIssues (FeedbackDB conn) project = do
    rows <- query conn
        "SELECT category, subcategory, actual_issue, COUNT(*) as cnt \
        \FROM feedback \
        \WHERE project = ? \
        \GROUP BY category, subcategory, actual_issue \
        \ORDER BY cnt DESC"
        (Only project)
    pure $ map toKnownIssueFromRow rows
  where
    toKnownIssueFromRow :: (Text, Maybe Text, Text, Int) -> KnownIssue
    toKnownIssueFromRow (cat, subcat, issue, cnt) = KnownIssue
        { kiPattern = cat <> maybe "" (":" <>) subcat
        , kiDescription = issue
        , kiGuidance = "Avoid: " <> T.take 100 issue
        , kiOccurrences = cnt
        }

-- | Convert a feedback item to a known issue
toKnownIssue :: FeedbackItem -> KnownIssue
toKnownIssue fi = KnownIssue
    { kiPattern = categoryToText (fiCategory fi) <> maybe "" (":" <>) (fiSubcategory fi)
    , kiDescription = fiActualIssue fi
    , kiGuidance = fromMaybe ("Avoid: " <> T.take 100 (fiActualIssue fi)) (fiCorrection fi)
    , kiOccurrences = 1
    }

-- | Export feedback for injecting into a Claude session
exportForSession :: FeedbackDB -> Text -> IO Text
exportForSession db project = do
    issues <- getKnownIssues db project
    recent <- getFeedbackByProject db project
    let recentTake = take 10 recent

    pure $ T.unlines
        [ "# Known Issues for " <> project
        , ""
        , "## Patterns to Avoid"
        , ""
        , T.unlines $ map formatKnownIssue issues
        , ""
        , "## Recent Corrections"
        , ""
        , T.unlines $ map formatRecentFeedback recentTake
        ]
  where
    formatKnownIssue ki =
        "- **" <> kiPattern ki <> "** (" <> T.pack (show $ kiOccurrences ki) <> " occurrences)\n" <>
        "  " <> kiDescription ki <> "\n" <>
        "  Guidance: " <> kiGuidance ki

    formatRecentFeedback fi =
        "- [" <> categoryToText (fiCategory fi) <> "] " <>
        T.take 80 (fiActualIssue fi)

-- | Export feedback for Anthropic (anonymized)
exportForAnthropic :: FeedbackDB -> IO ByteString
exportForAnthropic (FeedbackDB conn) = do
    rows <- query_ conn
        "SELECT * FROM feedback WHERE shared_with_anthropic = 0"
    let items = catMaybes $ map rowToFeedback rows
        anonymized = map anonymize items
    pure $ encode anonymized
  where
    anonymize fi = fi
        { fiProject = Nothing  -- Remove project name
        , fiSessionId = Nothing  -- Remove session
        , fiSourceFile = Nothing  -- Remove file paths
        }

-- | Feedback statistics
data FeedbackStats = FeedbackStats
    { fsTotal             :: Int
    , fsByCategory        :: [(FeedbackCategory, Int)]
    , fsBySource          :: [(FeedbackSource, Int)]
    , fsVerifiedCount     :: Int
    , fsSharedCount       :: Int
    , fsMostCommonIssues  :: [KnownIssue]
    } deriving stock (Show, Generic)
      deriving anyclass (ToJSON)

-- | Get feedback statistics
getStats :: FeedbackDB -> IO FeedbackStats
getStats (FeedbackDB conn) = do
    [(total :: Int)] <- query_ conn "SELECT COUNT(*) FROM feedback"
    [(verified :: Int)] <- query_ conn "SELECT COUNT(*) FROM feedback WHERE verified = 1"
    [(shared :: Int)] <- query_ conn "SELECT COUNT(*) FROM feedback WHERE shared_with_anthropic = 1"

    catCounts <- query_ conn
        "SELECT category, COUNT(*) FROM feedback GROUP BY category ORDER BY COUNT(*) DESC"
    let byCat = map (\(c, n) -> (textToCategory c, n)) catCounts

    sourceCounts <- query_ conn
        "SELECT detected_by, COUNT(*) FROM feedback GROUP BY detected_by ORDER BY COUNT(*) DESC"
    let bySource = map (\(s, n) -> (textToSource s, n)) sourceCounts

    commonRows <- query_ conn
        "SELECT category, subcategory, actual_issue, COUNT(*) as cnt \
        \FROM feedback \
        \GROUP BY category, subcategory, actual_issue \
        \ORDER BY cnt DESC \
        \LIMIT 10"
    let common = map (\(cat, subcat, issue, cnt) -> KnownIssue
            { kiPattern = cat <> maybe "" (":" <>) subcat
            , kiDescription = issue
            , kiGuidance = ""
            , kiOccurrences = cnt
            }) commonRows

    pure FeedbackStats
        { fsTotal = total
        , fsByCategory = byCat
        , fsBySource = bySource
        , fsVerifiedCount = verified
        , fsSharedCount = shared
        , fsMostCommonIssues = common
        }

-- | Helper for Maybe Int
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\mx acc -> maybe acc (:acc) mx) []
