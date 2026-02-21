{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.Config
-- Description : Configuration management for claude-verify
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- This module handles loading and managing project configuration from
-- .claude-context.toml files. These files specify:
--
-- * Project metadata (name, languages)
-- * Verification level and required checks
-- * Known issues to avoid
-- * Paths to formal specifications

module ClaudeVerify.Config
    ( -- * Configuration
      Config(..)
    , ProjectSection(..)
    , VerificationSection(..)
    , FormalSpecsSection(..)

      -- * Loading
    , loadConfig
    , loadConfigFromPath
    , findConfigFile

      -- * Defaults
    , defaultConfig

      -- * Validation
    , validateConfig
    , ConfigError(..)
    ) where

import ClaudeVerify.Internal.Types (KnownIssue(..), VerificationLevel(..))

import Control.Exception (try, SomeException)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>), takeDirectory)
import Toml (TomlCodec, (.=))
import qualified Toml

-- | Project section of config
data ProjectSection = ProjectSection
    { psName        :: Text
    , psLanguages   :: [Text]
    , psSafetyLevel :: Text
    } deriving stock (Eq, Show, Generic)

projectSectionCodec :: TomlCodec ProjectSection
projectSectionCodec = ProjectSection
    <$> Toml.text "name" .= psName
    <*> Toml.arrayOf Toml._Text "languages" .= psLanguages
    <*> Toml.text "safety_level" .= psSafetyLevel

-- | Verification section of config
data VerificationSection = VerificationSection
    { vsRequired :: [Text]
    , vsOptional :: [Text]
    } deriving stock (Eq, Show, Generic)

verificationSectionCodec :: TomlCodec VerificationSection
verificationSectionCodec = VerificationSection
    <$> Toml.arrayOf Toml._Text "required" .= vsRequired
    <*> Toml.arrayOf Toml._Text "optional" .= vsOptional

-- | Formal specs section of config
data FormalSpecsSection = FormalSpecsSection
    { fsTlaPlus  :: [Text]
    , fsAlloy    :: [Text]
    , fsLean     :: [Text]
    , fsCoq      :: [Text]
    } deriving stock (Eq, Show, Generic)

formalSpecsSectionCodec :: TomlCodec FormalSpecsSection
formalSpecsSectionCodec = FormalSpecsSection
    <$> Toml.arrayOf Toml._Text "tla_plus" .= fsTlaPlus
    <*> Toml.arrayOf Toml._Text "alloy" .= fsAlloy
    <*> Toml.arrayOf Toml._Text "lean" .= fsLean
    <*> Toml.arrayOf Toml._Text "coq" .= fsCoq

-- | Known issue in config
data KnownIssueConfig = KnownIssueConfig
    { kicPattern   :: Text
    , kicGuidance  :: Text
    } deriving stock (Eq, Show, Generic)

knownIssueConfigCodec :: TomlCodec KnownIssueConfig
knownIssueConfigCodec = KnownIssueConfig
    <$> Toml.text "pattern" .= kicPattern
    <*> Toml.text "guidance" .= kicGuidance

-- | Full configuration
data Config = Config
    { cfgProject      :: ProjectSection
    , cfgKnownIssues  :: [Text]  -- Simplified: just strings for now
    , cfgVerification :: VerificationSection
    , cfgFormalSpecs  :: Maybe FormalSpecsSection
    } deriving stock (Eq, Show, Generic)

configCodec :: TomlCodec Config
configCodec = Config
    <$> Toml.table projectSectionCodec "project" .= cfgProject
    <*> Toml.arrayOf Toml._Text "known_issues.items" .= cfgKnownIssues
    <*> Toml.table verificationSectionCodec "verification" .= cfgVerification
    <*> Toml.dioptional (Toml.table formalSpecsSectionCodec "formal_specs") .= cfgFormalSpecs

-- | Configuration error
data ConfigError
    = ConfigNotFound FilePath
    | ConfigParseError Text
    | ConfigValidationError Text
    deriving stock (Eq, Show)

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config
    { cfgProject = ProjectSection
        { psName = "unnamed-project"
        , psLanguages = []
        , psSafetyLevel = "standard"
        }
    , cfgKnownIssues = []
    , cfgVerification = VerificationSection
        { vsRequired = ["compile", "lint"]
        , vsOptional = ["test", "audit"]
        }
    , cfgFormalSpecs = Nothing
    }

-- | Load config from current directory or parent directories
loadConfig :: IO (Either ConfigError Config)
loadConfig = do
    mpath <- findConfigFile
    case mpath of
        Just path -> loadConfigFromPath path
        Nothing -> pure $ Right defaultConfig

-- | Load config from specific path
loadConfigFromPath :: FilePath -> IO (Either ConfigError Config)
loadConfigFromPath path = do
    exists <- doesFileExist path
    if not exists
        then pure $ Left $ ConfigNotFound path
        else do
            content <- TIO.readFile path
            case Toml.decode configCodec content of
                Left err -> pure $ Left $ ConfigParseError $ T.pack $ show err
                Right cfg -> case validateConfig cfg of
                    Left err -> pure $ Left err
                    Right () -> pure $ Right cfg

-- | Find .claude-context.toml in current or parent directories
findConfigFile :: IO (Maybe FilePath)
findConfigFile = do
    cwd <- getCurrentDirectory
    searchUpwards cwd
  where
    configName = ".claude-context.toml"

    searchUpwards dir = do
        let path = dir </> configName
        exists <- doesFileExist path
        if exists
            then pure $ Just path
            else do
                let parent = takeDirectory dir
                if parent == dir  -- Reached root
                    then checkHome
                    else searchUpwards parent

    checkHome = do
        home <- getHomeDirectory
        let path = home </> configName
        exists <- doesFileExist path
        pure $ if exists then Just path else Nothing

-- | Validate a configuration
validateConfig :: Config -> Either ConfigError ()
validateConfig cfg = do
    -- Check safety level is valid
    let level = psSafetyLevel (cfgProject cfg)
    if level `elem` ["low", "standard", "high", "critical"]
        then Right ()
        else Left $ ConfigValidationError $
            "Invalid safety_level: " <> level <> ". Must be one of: low, standard, high, critical"

-- | Convert config to ProjectConfig type
toProjectConfig :: Config -> ProjectConfig
toProjectConfig cfg = ProjectConfig
    { pcName = psName (cfgProject cfg)
    , pcLanguages = psLanguages (cfgProject cfg)
    , pcVerificationLevel = safetyToLevel (psSafetyLevel $ cfgProject cfg)
    , pcKnownIssues = map textToKnownIssue (cfgKnownIssues cfg)
    , pcRequiredChecks = vsRequired (cfgVerification cfg)
    , pcOptionalChecks = vsOptional (cfgVerification cfg)
    , pcFormalSpecs = case cfgFormalSpecs cfg of
        Nothing -> []
        Just fs -> map ("tla+",) (fsTlaPlus fs) ++
                   map ("alloy",) (fsAlloy fs) ++
                   map ("lean",) (fsLean fs) ++
                   map ("coq",) (fsCoq fs)
    }
  where
    safetyToLevel "low" = Quick
    safetyToLevel "standard" = Standard
    safetyToLevel "high" = Rigorous
    safetyToLevel "critical" = Exhaustive
    safetyToLevel _ = Standard

    textToKnownIssue t = KnownIssue
        { kiPattern = ""
        , kiDescription = t
        , kiGuidance = t
        , kiOccurrences = 0
        }
