{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Main where

import "hspec" Test.Hspec
import qualified "xnukbot-plugins" Xnukbot.Plugin.Mueval as Mueval

import "irc" Network.IRC.Commands (privmsg)
import "irc" Network.IRC.Base (Message(..), Prefix(..))
import qualified "containers" Data.Map.Strict as M

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)

import Control.Monad (unless, when, void)

import Data.Maybe (fromMaybe)

main :: IO ()
main = hspec $
    describe "Mueval" $
        it "default test" $
            msgPlugTest Mueval.plugin M.empty (privmsg "#test" ">> 1+1") $ \err msgs -> do
                when (null msgs) $ err "not enough"
                let ps = msg_params (head msgs)
                unless (ps == ["#test", "2"]) . err $
                    "mueval doesn't work correctly. Actual: " ++ show ps

msgPlugTest plugin setting message send = do
    m <- newEmptyMVar
    let (name, plug) = plugin
        err str = tryPutMVar m (Just str) >> fail str
        send' msgs = send err msgs >> void (tryPutMVar m Nothing)
        g = plug setting send' (message {msg_prefix = Just $ NickName "Xnuk" Nothing Nothing})

    fromMaybe (fail (name ++ ": checker does not match")) g
    a <- takeMVar m
    a `shouldBe` Nothing
