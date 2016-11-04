import "text" Data.Text (Text)
import "regex-pcre" Text.Regex.PCRE.Heavy

data Plugin = CmdPlugin Text Text | RegexPlugin
