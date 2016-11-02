{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Plugin.Plugin (runPlugin) where
import "irc" Network.IRC.Base (Message(..))
import "bytestring" Data.ByteString (ByteString)
import "containers" Data.Map (Map)
import qualified "containers" Data.Map as M
import "text" Data.Text (Text)
import Control.Concurrent.MVar (newMVar, MVar)
import System.IO.Unsafe (unsafePerformIO)

config :: MVar (Map Text Text)
config = unsafePerformIO $ newMVar M.empty

runPlugin :: Message -> IO [ByteString]
runPlugin = undefined
