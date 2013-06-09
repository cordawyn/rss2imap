{-# LANGUAGE OverloadedStrings #-}

-- IMAP configuration as a YAML file
module RSS2IMAP.Config where

import Data.Yaml
import Data.Functor ((<$>))
import Control.Applicative (pure, (<*>))
import Network.Socket (PortNumber(PortNum))

data Config = Config {
  imapServer    :: String
, imapPort      :: PortNumber
, imapUsername  :: String
, imapPassword  :: String
, imapEmailTo   :: String
, imapEmailFrom :: String
}

instance FromJSON Config where
  parseJSON (Object cfg) = Config <$>
                           cfg .: "server" <*>
                           cfg .: "port" <*>
                           cfg .: "username" <*>
                           cfg .: "password" <*>
                           cfg .: "email-to" <*>
                           cfg .: "email-from"

instance FromJSON PortNumber where
  parseJSON (Number p) = pure $ PortNum $ floor p

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig = decodeFile
