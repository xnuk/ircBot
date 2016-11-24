# :tada: IRC bot

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- choose your favorite server
import Xnukbot.Server.Uriirc (connect)

import Xnukbot.IrcBot (bot)
import Xnukbot.Plugin.Base.Types (Attr(Global))

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Map.Strict as M

import qualified Xnukbot.Plugin.Base.Echo as Echo
import qualified Xnukbot.Plugin.Base.Setting as Setting
import qualified Xnukbot.Plugin.Base.Logger as Logger

nick = encodeUtf8 "무당벌레장구벌레풍뎅이벌레"
channels = "#botworld,#botworld2"

plugins = [Setting.plugin, Echo.plugin, Logger.plugin]

setting = M.fromList [ (Global "prefix", "@") ]

main :: IO ()
main = do
    (_, bye) <- bot connect nick channels setting plugins

    _ <- getLine -- prevents to quit the program itself

    bye
```
