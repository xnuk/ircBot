{-# LANGUAGE QuasiQuotes #-}
module Xnukbot.Plugin.Hyeong (plugin) where

import Prelude hiding (writeFile)

import "unix" System.Posix.Temp (mkstemp)
import "directory" System.Directory (removeFile)
import "process" System.Process (createProcess, proc, CreateProcess(std_in, std_out, std_err, close_fds), StdStream(CreatePipe, NoStream), terminateProcess, waitForProcess)
import "base" Control.Concurrent (forkFinally, forkIO, threadDelay)
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import "xnukbot" Xnukbot.Plugin (Plugin)
import "xnukbot" Xnukbot.Plugin.Simple (simplePlugin)
import "pcre-heavy" Text.Regex.PCRE.Heavy (re, Regex)

import "bytestring" Data.ByteString (hGet)
import "text" Data.Text (Text)
import qualified "text" Data.Text as T (null, takeWhile)
import "text" Data.Text.IO (hPutStr)
import "text" Data.Text.Encoding (decodeUtf8With)
import System.IO (hClose, hSetBinaryMode)
import Control.Exception (bracket, catch, SomeException)

regex, prefRegex :: Regex
regex = [re|^\s*((?:(?:[형항핫흣흡흑]|혀어*엉|하아*[앙앗]|흐으*[읏읍윽])[\.…⋯⋮]*[!\?♥❤💕💖💗💘💙💚💛💜💝♡]*\s*)+)$|]

prefRegex = [re|^hyeong\s*(.+)\s*$|]

runHyeong :: Text -> IO Text
runHyeong code = do
    path <- bracket (mkstemp "/tmp/hyeong") (hClose . snd) $ \ (path, h) -> do
        hSetBinaryMode h False
        hPutStr h code
        return path

    bracket (launchImpl path) (closeImpl path) $ \ (hout, process) -> do
        res <- newEmptyMVar
        _ <- forkFinally (hGet hout 600) $
            putMVar res . either (const mempty) id

        forkIO $ do
            threadDelay 5000000 -- 5 sec
            terminateProcess process

        output <- takeMVar res
        return . T.takeWhile (\x -> x /= '\r' && x /= '\n') $ decodeUtf8With (\_ _ -> Nothing) output

  where
    launchImpl path = do
        (_, Just hout, _, process) <- createProcess (proc "rshyeong" [path])
            { std_in = NoStream
            , std_out = CreatePipe
            , std_err = NoStream
            , close_fds = True
            }
        return (hout, process)

    closeImpl path (hout, process) = do
        hClose hout
        waitForProcess process
        catch (removeFile path) $ \e -> print (e :: SomeException)


plugin :: Plugin
plugin = simplePlugin "Hyeong" (prefRegex, regex) $ \_ _ _ ->
    let toList x = if T.null x then [] else [x]
    in fmap toList . runHyeong . either head head
