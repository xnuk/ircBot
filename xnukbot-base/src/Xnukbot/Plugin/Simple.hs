{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Xnukbot.Plugin.Simple (simplePlugin) where

import Xnukbot.Plugin (MessageT(Message), PrefixT(..), Channel, Plugin, Plug, Setting, privmsgNoPref, privmsg, hasAttribute, removePrefix)
import "pcre-heavy" Text.Regex.PCRE.Heavy (Regex, scan)
import "text" Data.Text (Text, pack)
import "base" Control.Concurrent (forkIO)
import "base" Data.Monoid ((<>))
import "base" Control.Applicative ((<|>))
import Safe (headMay)

simplePlugin :: String -> (Regex, Regex) -> (Setting -> Channel -> Text -> Either [Text] [Text] -> IO [Text]) -> Plugin
simplePlugin name (pref, nopref) func = (name, plug)
  where plug :: Plug
        plug setting send (Message (Just NickName{nickName}) "PRIVMSG" [chan, msg])
            | hasAttribute chan setting (pack name <> ".disabled") = Nothing
            | otherwise =
                let withPrefix = do
                        a <- removePrefix chan setting msg
                        (Left  . snd) <$> headMay (scan pref a)
                    withoutPrefix = (Right . snd) <$> headMay (scan nopref msg)
                in (withPrefix <|> withoutPrefix) >>= \x -> Just $ do
                    forkIO $ do
                        msgs <- func setting chan nickName x
                        send $ case x of
                            Left  _ -> map (privmsg chan nickName) msgs
                            Right _ -> map (privmsgNoPref chan nickName) msgs
                    return setting
        plug _ _ _ = Nothing
