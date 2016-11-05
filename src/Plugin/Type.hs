{-# LANGUAGE PackageImports #-}
import "text" Data.Text (Text)
import "pcre-heavy" Text.Regex.PCRE.Heavy

data Plugin = CmdPlugin Text Text | RegexPlugin
