{-# LANGUAGE OverloadedStrings #-}

-- IMAP configuration as a YAML file
module RSS2IMAP.Config where

import Data.Yaml
import Data.Functor ((<$>))
import Control.Applicative (pure, (<*>))
import Network.Socket -- .Types -- TODO: limit imported symbols

data Config = Config {
  imapServer :: String
, imapPort :: PortNumber
, imapUsername :: String
, imapPassword :: String
}

instance FromJSON Config where
  parseJSON (Object cfg) = Config <$>
                           cfg .: "server" <*>
                           cfg .: "port" <*>
                           cfg .: "username" <*>
                           cfg .: "password"

instance FromJSON PortNumber where
  parseJSON (Number p) = pure $ PortNum $ floor p

loadRSS2IMAPConfig :: FilePath -> IO (Maybe Config)
loadRSS2IMAPConfig = decodeFile
