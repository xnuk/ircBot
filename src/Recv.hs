{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Recv (parseit, parseMessage, ChunkFunc, loop, sendingIO) where

import "tls" Network.TLS (Context, recvData, sendData)
import "irc" Network.IRC.Parser (prefix, command, spaces, crlf, parameter)
import "irc" Network.IRC.Base (Message(..))
import "attoparsec" Data.Attoparsec.ByteString (IResult(Done, Partial, Fail), parse, option, many', Parser)
import "mtl" Control.Monad.State.Lazy (State, get, put, runState, evalStateT)
import "bytestring" Data.ByteString (ByteString)
import Control.Monad (forever, forM_, void)
import "base" Control.Monad.IO.Class (liftIO)
import "bytestring" Data.ByteString.Lazy (fromStrict)
import "stm" Control.Concurrent.STM (atomically)
import "stm" Control.Concurrent.STM.TQueue (TQueue, readTQueue)

import Data.Monoid ((<>))
import Control.Concurrent (forkIO)

message :: Parser Message
message = Message
    <$> option Nothing (Just <$> (prefix <* spaces))
    <*> command
    <*> many' (spaces *> parameter)
    <*  crlf

type ChunkFunc = ByteString -> IResult ByteString Message

parseMessage :: ChunkFunc
parseMessage = parse message

{-

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

-}

loop :: (Context, ChunkFunc) -> (Message -> IO ()) -> IO ()
loop (ctx, func) plugin = flip evalStateT func . forever $ do
    str <- recvData ctx
    sf <- get
    let (msgs, sf') = runState (parseit str) sf
    put sf'
    liftIO . forM_ msgs $ \msg -> case msg_command msg of
        "PING" -> sendData ctx . fromStrict $ "PONG " <> head (msg_params msg) <> "\r\n"
        _ -> void $ forkIO (plugin msg)
    return ()

sendingIO :: Context -> TQueue ByteString -> IO a
sendingIO ctx tq = forever $ do
    msg <- atomically $ readTQueue tq
    sendData ctx . fromStrict $ msg

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
        Fail i errs err -> errorWithoutStackTrace $ unlines errs ++ "\n\n" ++ err
            -- put (parse message)
            -- parseit i
