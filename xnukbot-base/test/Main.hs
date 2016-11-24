{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Main where

import "hspec" Test.Hspec
import "pcre-heavy" Text.Regex.PCRE.Heavy (re, (=~))
import "pcre-light" Text.Regex.PCRE.Light (match)
import "bytestring" Data.ByteString (ByteString)
import qualified "containers" Data.Map.Strict as M

import qualified "xnukbot" Xnukbot.Plugin.Base.Setting as Setting
import "xnukbot" Xnukbot.Plugin (AttrT(..), getAttribute, getAttributes)
import Data.List (sort)

main :: IO ()
main = hspec $ do
    describe "General test" $
        it "does pcre-light works correctly?" $ do
            match [re|^(a)b(c)|] "abc" [] `shouldBe` Just ["abc", "a", "c"]
            match [re|^(a?)b(c)|] "cbc" [] `shouldBe` Nothing
            match [re|^(a?)b(c)|] "abc" [] `shouldBe` Just ["abc", "a", "c"]
    describe "Plugin: Setting" $ do
        let simpleSetting = M.fromList
                [ (Protected "prefix", "@")
                , (Global "prefix", "$")
                , (Local "#botworld" "kawaii", "Xnuk")
                , (Global "kawaii", "No")
                ]
        it "have correct regex" $ do
            ("set nickname Xnuk" :: ByteString) =~ Setting.regexSet `shouldBe` True
            ("set nickname" :: ByteString) =~ Setting.regexGet `shouldBe` True
        it "getAttribute" $ do
            getAttribute "" simpleSetting "prefix" `shouldBe` Just "@"
            getAttribute "#botworld" simpleSetting "kawaii" `shouldBe` Just "Xnuk"
        it "getAttributes" $ do
            sort (getAttributes "" simpleSetting "prefix") `shouldBe` sort ["@", "$"]
            getAttributes "#botworld" simpleSetting "kawaii" `shouldBe` ["Xnuk"]

