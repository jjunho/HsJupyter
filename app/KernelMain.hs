module Main (main) where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (die)

import HsJupyter.KernelProcess
  ( KernelProcessConfig(..)
  , LoadConfigError(..)
  , LogLevel(..)
  , loadKernelProcessConfig
  , runKernel
  , summariseConfig
  )

-- | CLI arguments supported by the prototype
data Options = Options
  { optConnection :: FilePath
  , optLogLevel   :: Maybe LogLevel
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
        ( long "connection"
       <> metavar "FILE"
       <> help "Path to Jupyter connection file"
        )
  <*> optional (option auto
        ( long "log-level"
       <> metavar "LEVEL"
       <> help "Log level (Debug|Info|Warn|Error)"
        ))

main :: IO ()
main = do
  opts <- execParser optsInfo
  envLevel <- traverse parseLogLevel =<< lookupEnv "HSJUPYTER_LOG_LEVEL"
  let effectiveLevel = optLogLevel opts <|> envLevel
  eConfig <- loadKernelProcessConfig (optConnection opts) effectiveLevel
  case eConfig of
    Left err -> die (renderError err)
    Right cfg -> do
      putStrLn $ "[hsjupyter] binding kernel: " <> T.unpack (summariseConfig cfg)
      putStrLn "[hsjupyter] kernel ready – waiting for Jupyter messages (Ctrl+C to exit)"
      runKernel cfg
  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Phase 1 protocol bridge prototype for HsJupyter"
     <> header "hs-jupyter-kernel"
      )

    renderError :: LoadConfigError -> String
    renderError err = case err of
      ConfigFileMissing path -> "Connection file missing: " <> path
      ConfigDecodeError msg  -> "Failed to parse connection file: " <> msg
      ConfigValidationError msg -> "Connection file invalid: " <> msg

    parseLogLevel :: String -> Maybe LogLevel
    parseLogLevel input = case map toLower input of
      "debug" -> Just LogDebug
      "info"  -> Just LogInfo
      "warn"  -> Just LogWarn
      "error" -> Just LogError
      _       -> Nothing
