{-# LANGUAGE OverloadedStrings #-}
module Web.Mackerel.Config
  ( Config(..)
  , HostStatusConfig(..)
  , loadConfig
  , parseConfig
  , loadClient
  , mackerelRoot
  , confFile
  , pidFile
  ) where

import Data.Aeson
import Data.Aeson.Types (Value(..), Result(..), typeMismatch)
import Data.Default (Default(..))
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Info (os)
import Text.Toml (parseTomlDoc)

import Web.Mackerel.Client
import Web.Mackerel.Types.Host

data Config
  = Config {
    configApiBase :: Maybe String,
    configApiKey :: Maybe String,
    configRoot :: Maybe String,
    configPidfile :: Maybe String,
    configRoles :: Maybe [String],
    configVerbose :: Maybe Bool,
    configDiagnostic :: Maybe Bool,
    configDisplayname :: Maybe String,
    configHostStatus :: Maybe HostStatusConfig
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object o)
    = Config <$> o .:? "apibase" <*> o .:? "apikey" <*> o .:? "root"
             <*> o .:? "pidfile" <*> o .:? "roles" <*> o .:? "verbose"
             <*> o .:? "diagnostic" <*> o .:? "display_name" <*> o .:? "host_status"
  parseJSON o = typeMismatch "Config" o

instance Default Config where
  def = Config def def def def def def def def def

data HostStatusConfig
  = HostStatusConfig {
    hostStatusOnStart :: Maybe HostStatus,
    hostStatusOnStop :: Maybe HostStatus
  } deriving (Eq, Show)

instance FromJSON HostStatusConfig where
  parseJSON (Object o)
    = HostStatusConfig <$> o .:? "on_start" <*> o .:? "on_stop"
  parseJSON o = typeMismatch "HostStatusConfig" o

instance Default HostStatusConfig where
  def = HostStatusConfig def def

agentName :: String
agentName = "mackerel-agent"

loadConfig :: IO (Either String Config)
loadConfig = parseConfig <$> (Text.readFile =<< confFile)

parseConfig :: Text.Text -> Either String Config
parseConfig cnt
  = case parseTomlDoc "" cnt of
         Left _ -> Left "toml parse error"
         Right res -> case fromJSON $ toJSON res of
                           Error err -> Left err
                           Success r -> Right r

loadClient :: IO (Either String Client)
loadClient = fmap (\c -> def { apiKey = fromMaybe (apiKey def) $ configApiKey c,
                               apiBase = fromMaybe (apiBase def) $ configApiBase c }) <$> loadConfig

mackerelRoot :: IO FilePath
mackerelRoot | isBSD || isDarwin = getHomeDirectory >>= \home -> return $ home </> "Library" </> agentName
             | isWindows = error "Windows is not supported yet."
             | otherwise = return "/var/lib/mackerel-agent"

confFile :: IO FilePath
confFile | isLinux = return $ "/etc" </> agentName </> agentName ++ ".conf"
         | otherwise = mackerelRoot >>= \root -> return $ root </> agentName ++ ".conf"

pidFile :: IO FilePath
pidFile | isLinux = return "/var/run/mackerel-agent.pid"
        | otherwise = mackerelRoot >>= \root -> return $ root </> agentName ++ ".pid"

isBSD :: Bool
isBSD = "bsd" `isInfixOf` os

isDarwin :: Bool
isDarwin = os == "darwin"

isWindows :: Bool
isWindows = "mingw" `isPrefixOf` os

isLinux :: Bool
isLinux = not isBSD && not isDarwin && not isWindows
