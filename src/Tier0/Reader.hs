module Tier0.Reader (Environment (..), EnvironmentM, formatUserName, formatHost, formatCurrentDir, formatPrompt) where

import Control.Monad.Reader

data Environment = Environment
  { username :: String
  , isSuperUser :: Bool
  , host :: String
  , currentDir :: String
  } deriving Eq

type EnvironmentM = Reader Environment

formatUserName :: EnvironmentM String
formatUserName = do
  env <- ask
  return $ if isSuperUser env then "root" else username env

formatHost :: EnvironmentM String
formatHost = do
  env <- ask
  return $ host env

formatCurrentDir :: EnvironmentM String
formatCurrentDir = do
  env <- ask
  return $ currentDir env

formatPrompt :: EnvironmentM String
formatPrompt = do
  userName <- formatUserName
  hostName <- formatHost
  currentDir <- formatCurrentDir
  return $ userName ++ "@" ++ hostName ++ ":" ++ currentDir ++ "$"
