{-# LANGUAGE QuasiQuotes, OverloadedStrings, PackageImports #-}
import Test.Hspec
import "pcre-heavy" Text.Regex.PCRE.Heavy (re, (=~))
import "pcre-light" Text.Regex.PCRE.Light (match)
import "bytestring" Data.ByteString (ByteString)

import qualified Plugin.Setting as Setting

main :: IO ()
main = hspec $ do
    describe "General test" $
        it "does pcre-light works correctly?" $ do
            match [re|^(a)b(c)|] "abc" [] `shouldBe` Just ["abc", "a", "c"]
            match [re|^(a?)b(c)|] "cbc" [] `shouldBe` Nothing
            match [re|^(a?)b(c)|] "abc" [] `shouldBe` Just ["abc", "a", "c"]
    describe "Plugin: Setting" $
        it "have correct regex" $ do
            ("set nickname Xnuk" :: ByteString) =~ Setting.regexSet `shouldBe` True
            ("set nickname" :: ByteString) =~ Setting.regexGet `shouldBe` True
