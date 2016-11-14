{-# LANGUAGE PackageImports, OverloadedStrings, NamedFieldPuns #-}
module IrcBot (bot) where
import Recv (ChunkFunc, loop, sendingIO)
import qualified Sender as S (sender)
import Plugin.Plugin (runPlugin, PluginWrapper(..))
import Plugin.Type (Plugin, Setting)
import "bytestring" Data.ByteString (ByteString)
import "tls" Network.TLS (Context, bye)
import "stm" Control.Concurrent.STM.TQueue (TQueue, newTQueue)
import "stm" Control.Concurrent.STM (atomically)
import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newMVar)
import qualified "containers" Data.Map.Strict as M

bot :: (ByteString -> ByteString -> IO (Context, ChunkFunc))
    -> ByteString -- nick
    -> ByteString -- chan
    -> Setting
    -> [Plugin]
    -> IO (TQueue ByteString, IO ())

bot connect nick chan setting' plugins = do
    setting <- newMVar $ M.insert "Protected.nickname" nick setting'
    sendingQueue <- atomically newTQueue
    let sender = S.sender sendingQueue
        plugin = PluginWrapper { setting, plugins, sender }
    x@(ctx, _) <- connect nick chan
    let close = const (putStrLn "&wat*" >> bye ctx)
    ircSendThread <- forkFinally (sendingIO ctx sendingQueue) close
    ircRecvThread <- forkFinally (loop x $ runPlugin plugin) close
    return (sendingQueue, bye ctx >> killThread ircSendThread >> killThread ircRecvThread)
