{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Data.Random.Util (key, choice) where

import Prelude hiding (lookup)
import "xnukbot" Xnukbot.Plugin.Types (AttrT(Protected), Attr, Setting)
import "random" System.Random (newStdGen, randomR)
import "text" Data.Text (pack, unpack)
import "unordered-containers" Data.HashMap.Strict (lookup, insert)
import "safe" Safe (readMay)

key :: Attr
key = Protected "Random.seed"

choice :: [a] -> Setting -> IO (a, Setting)
choice [] _ = fail "Random choice: this is empty list"
choice [a] setting = return (a, setting)
choice xs setting = do
    seed <- maybe newStdGen return (lookup key setting >>= readMay . unpack)
    let (index, seed') = randomR (0, length xs - 1) seed
    return (xs !! index, insert key (pack $ show seed') setting)
