{-# LANGUAGE PackageImports, OverloadedStrings #-}

module Main where

import TLSConnect
import Recv

import "tls" Network.TLS (bye, sendData)
import Control.Concurrent (forkFinally, killThread)

--import "text" Data.Text.Encoding (decodeUtf8)
import qualified "text" Data.Text.Lazy.Encoding as TL (encodeUtf8)
import qualified "text" Data.Text.IO as T
import "text" Data.Text.Lazy (fromStrict)

import Control.Monad (forever, mplus)
import Data.Monoid ((<>))

main :: IO ()
main = do
    ctx <- tlsConnect "irc.uriirc.org" "16664"
    let send = sendData ctx . TL.encodeUtf8 . fromStrict . (<> "\r\n")
    output <- forkFinally (q ctx) $ \_ -> bye ctx
    forever (T.getLine >>= send) `mplus` (killThread output >> putStrLn "bye")
