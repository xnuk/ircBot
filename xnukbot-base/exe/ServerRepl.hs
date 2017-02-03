{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main (main) where

import "xnukbot" Xnukbot.Connect.TLSConnect (tlsConnect)
import "tls" Network.TLS (recvData, sendData)
import "network" Network.Socket (close)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import "text" Data.Text.IO (putStrLn, getLine)
import "bytestring" Data.ByteString.Lazy (fromStrict)
import System.Environment (getArgs)
import System.IO (IO)
import Control.Monad (forever, (>>=))
import Data.Function ((.))
import Control.Concurrent (forkFinally)
import Data.Monoid ((<>))

main :: IO ()
main = do
    [server, port] <- getArgs
    (ctx, sock) <- tlsConnect server port
    _ <- forkFinally (forever (recvData ctx >>= putStrLn . decodeUtf8)) (\_ -> close sock)
    forever (getLine >>= sendData ctx . fromStrict . encodeUtf8 . (<> "\r\n"))
