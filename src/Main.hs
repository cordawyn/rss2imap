{-# LANGUAGE OverloadedStrings #-}

import RSS2IMAP.Config
import RSS2IMAP.RSSMail

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection (IMAPConnection (..))
import Network.HaskellNet.IMAP.Types (MailboxName)
import Network.HaskellNet.IMAP (append)
import Text.Feed.Query
import Text.Feed.Types (Feed (..), Item (..))
import Text.Feed.Import (parseFeedString)
import Network.Mail.Mime (Mail (..), renderMail')
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import Control.Monad (liftM)
import System.Directory (getAppUserDataDirectory)
import Data.Foldable (foldrM)

main :: IO ()
main = do
  appDataDir <- getAppUserDataDirectory "rss2imap"
  config     <- loadConfig $ appDataDir ++ "config.yml"
  case config of
    Nothing -> fail "Configuration data is empty"
    Just c  -> do
      readIds <- (liftM lines . readFile) $ appDataDir ++ "read.txt"
      feeds   <- (liftM lines . readFile) $ appDataDir ++ "feeds.txt"
      newReadIds <- foldrM (sendFeedToIMAP c) readIds feeds
      writeFile (appDataDir ++ "read.yml") $ unlines newReadIds

sendFeedToIMAP :: Config -> String -> [String] -> IO [String]
sendFeedToIMAP config feedURL readIds = do
  feed <- getFeed feedURL
  case feed of
    Nothing -> fail $ "Feed " ++ feedURL ++ " is empty"
    Just f  -> do
      imap <- connectIMAPPort (imapServer config) (imapPort config)
      login imap (imapUsername config) (imapPassword config)
      unreadItems <- return $ filterReadItems (getFeedItems f) readIds
      newReadIds <- foldrM (appendItem imap) readIds unreadItems
      logout imap
      return newReadIds

getFeed :: String -> IO (Maybe Feed)
getFeed uri = simpleHTTP (getRequest uri) >>= getResponseBody >>= return . parseFeedString

filterReadItems :: [Item] -> [String] -> [Item]
filterReadItems items ids = filter (\i -> isReadItem i ids) items

isReadItem :: Item -> [String] -> Bool
isReadItem i = elem (getFeedItemId i)
               where getFeedItemId itm = case getItemId itm of
                                           Nothing -> ""
                                           Just (_, iid) -> iid

appendMail :: IMAPConnection -> MailboxName -> Mail -> IO ()
appendMail c mbox mail = renderMail' mail >>= (append c mbox) . LBS.toStrict

appendItem :: IMAPConnection -> Item -> [String] -> IO [String]
appendItem con item readIds = do
  mail <- createMailFromItem item
  appendMail con "RSS" mail
  case getItemId item of
    Nothing -> return readIds
    Just (_, iid) -> return (iid : readIds)
