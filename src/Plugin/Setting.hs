{-# LANGUAGE PackageImports, QuasiQuotes #-}

import "irc" Network.IRC.Base (Message(..), Prefix NickName)
import "text" Data.Text.Encoding (encodeUtf8)
import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~))

import Plugin.Type (Plugin, Setting, Sender, removePrefix)

checker :: Message -> Bool
checker (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg])
    | nick == encodeUtf8 "즈눅" = maybe False (removePrefix setting msg)
