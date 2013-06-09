{-# LANGUAGE OverloadedStrings #-}

-- IMAP configuration as a YAML file
module RSS2IMAP.Config where

import Data.Yaml
import Data.Functor ((<$>))
import Control.Applicative (pure, (<*>))
import Network.Socket (PortNumber(PortNum))

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

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig = decodeFile
