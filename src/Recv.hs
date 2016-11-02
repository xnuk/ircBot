{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Recv(q) where

import "tls" Network.TLS (Context, recvData, sendData)
import "irc" Network.IRC.Parser (prefix, command, spaces, crlf, parameter)
import "irc" Network.IRC.Base (Message(..), showMessage)
import "attoparsec" Data.Attoparsec.ByteString (IResult(Done, Partial, Fail), parse, option, many', Parser)
import "mtl" Control.Monad.State.Lazy (State, StateT, get, put, runState, evalStateT)
import "bytestring" Data.ByteString (ByteString)
import Control.Monad (forever, forM_)
import "base" Control.Monad.IO.Class (liftIO)
import "bytestring" Data.ByteString.Lazy (fromStrict)
import Data.Monoid ((<>))
import Plugin.Plugin (runPlugin)

message :: Parser Message
message = Message
    <$> option Nothing (Just <$> (prefix <* spaces))
    <*> command
    <*> many' (spaces *> parameter)
    <*  crlf

q :: Context -> IO ()
q ctx = evalStateT (p ctx) (parse message)

p :: Context -> StateT (ByteString -> IResult ByteString Message) IO ()
p ctx = forever $ do
    str <- recvData ctx
    sf <- get
    let (msgs, sf') = runState (parseit str) sf
    put sf'
    liftIO . forM_ msgs $ \msg -> case msg_command msg of
        "NOTICE" | msg_params msg == ["AUTH", "*** Checking Ident"] -> sendData ctx "NICK Xnuk\r\nUSER Xnuk xnuk xnuk :xnuk\r\n" >> print (showMessage msg)
        "001" -> sendData ctx "JOIN #botworld\r\n" >> print (showMessage msg)
        "PING" -> sendData ctx . fromStrict $ "PONG " <> head (msg_params msg) <> "\r\n"
        _ -> runPlugin msg >>= mapM_ (sendData ctx . fromStrict)
    return ()

parseit :: ByteString -> State (ByteString -> IResult ByteString Message) [Message]
parseit msg = do
    f <- get
    case f msg of
        Done i r -> do
            put (parse message)
            xs <- parseit i
            return $ r : xs
        Partial func -> do
            put func
            return []
        Fail i _ _ -> do
            put (parse message)
            parseit i
