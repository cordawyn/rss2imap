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

-- TODO: proper user home dir
configFile = "/home/cordawyn/.rss2imap/config.yml"
readIdsFile = "/home/cordawyn/.rss2imap/read.yml"
feedListFile = "/home/cordawyn/.rss2imap/feeds.txt"

main :: IO ()
main = (liftM lines . readFile) feedListFile >>= mapM_ sendFeedToIMAP

sendFeedToIMAP :: String -> IO ()
sendFeedToIMAP feedURL = do
    feed <- getFeed feedURL
    case feed of
      Nothing -> fail "Feed is empty"
      Just f  -> do conf <- loadRSS2IMAPConfig configFile
                    case conf of
                      Nothing -> fail "Configuration data is empty"
                      Just c  -> do imap <- connectIMAPPort (imapServer c) (imapPort c)
                                    login imap (imapUsername c) (imapPassword c)
                                    readIds <- readFile readIdsFile
                                    unreadItems <- return $ filterReadItems (getFeedItems f) (lines readIds)
                                    mapM_ (appendItem imap) unreadItems
                                    logout imap

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

appendItem :: IMAPConnection -> Item -> IO ()
appendItem con item = do mail <- createMailFromItem item
                         appendMail con "RSS" mail
                         case getItemId item of
                            Nothing -> return ()
                            Just (_, iid) -> writeFile readIdsFile iid
