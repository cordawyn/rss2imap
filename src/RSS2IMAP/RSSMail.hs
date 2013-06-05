{-# LANGUAGE OverloadedStrings #-}

-- Everything related to converting a feed into e-mails.
module RSS2IMAP.RSSMail where

import Text.Html
import Text.Feed.Query
import Text.Feed.Types (Item (..))
import Network.Mail.Mime (Mail (..), Address (..), simpleMail)
import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as LT (pack)

createMailFromItem :: Item -> IO Mail
createMailFromItem i = simpleMail
                       (Address Nothing "slava.kravchenko@gmail.com")
                       (Address Nothing "rss2imap@localhost")
                       (T.pack subject)
                       (LT.pack plainBody)
                       (LT.pack htmlBody)
                       []
                       where htmlBody = renderHtml $ convertItemToHtml i
                             plainBody = mailItemSummary i
                             subject = mailItemTitle i

convertItemToHtml :: Item -> Html
convertItemToHtml i = h1 << (mailItemLink i)
                      +++ paragraph << (mailItemSummary i)

mailItemLink :: Item -> Html
mailItemLink i = case getItemLink i of
                   Nothing -> stringToHtml $ mailItemTitle i
                   Just l  -> anchor ! [href l] << mailItemTitle i

mailItemSummary :: Item -> String
mailItemSummary i = case getItemSummary i of
                      Nothing -> "No Summary"
                      Just s  -> s

mailItemTitle :: Item -> String
mailItemTitle i = case getItemTitle i of
                    Nothing -> "No Title"
                    Just t  -> t
