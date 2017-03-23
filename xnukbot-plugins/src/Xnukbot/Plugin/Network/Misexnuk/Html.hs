{-# LANGUAGE OverloadedStrings, Rank2Types, PackageImports #-}
module Xnukbot.Plugin.Network.Misexnuk.Html (crawl) where

import Xnukbot.Plugin.Network.Misexnuk.Javascript (javascript)

import "language-javascript" Language.JavaScript.Parser (readJs)

import "tagstream-conduit" Text.HTML.TagStream.Text (Token, tokenStream)
import "tagstream-conduit" Text.HTML.TagStream.Types (Token'(TagOpen, TagClose, Text))

import "conduit" Data.Conduit ((=$=), Consumer, await)
import qualified "conduit-combinators" Data.Conduit.Combinators as C

import qualified "text" Data.Text as T
import "text" Data.Text (Text, splitOn, pack, unpack)

import "unordered-containers" Data.HashMap.Lazy (HashMap, lookupDefault)
import "containers" Data.IntMap.Lazy (IntMap, splitLookup, lookupGT, lookupLE, fromList, toDescList)

import "monad-loops" Control.Monad.Loops (untilJust)

--import "conduit-extra" Data.Conduit.Text (decode, utf8)

--import "resourcet" Control.Monad.Trans.Resource (runResourceT)

import Data.Maybe (catMaybes)

import Data.Char (isDigit)

import Data.Monoid ((<>))
import Data.Ratio (numerator)
import Control.Arrow (first)

data Level = Good | Normal | Caution | Bad | Terrible

instance Show Level where
    show Good = "좋음"
    show Normal = "보통"
    show Caution = "주의"
    show Bad = "나쁨"
    show Terrible = "개같음"

pm10, pm25 :: IntMap Level
pm10 = fromList [(0, Good), (31, Normal), (51, Caution), (81, Bad), (151, Terrible)]
pm25 = fromList [(0, Good), (16, Normal), (36, Caution), (51, Bad), (75,  Terrible)]

level :: IntMap Level -> Int -> Maybe String
level m x = do
    (k, a) <- lookupLE x m
    case a of
        Terrible -> return $ show a ++ '(':show k ++ "~)"
        _ -> do
            (k', _) <- lookupGT x m
            return $ show a ++ '(':show k ++ '~':show (pred k') ++ ")"

expandRev :: Show a => Int -> IntMap a -> (Maybe a, [Maybe a])
expandRev time m =
    let (prev, a, next) = splitLookup time m
        z = toDescList prev ++ map (first $ subtract 24) (toDescList next)
        f pk ((k, v):xs)
            | pred pk == k = Just v : f k xs
            | pred pk >  k = replicate (pred pk - k) Nothing ++ Just v : f k xs
            | otherwise = []
        f _ [] = []
    in (a, f time z)

render' :: Show a => [Maybe a] -> Text
render' [] = mempty
render' [x] = maybe mempty (pack . show) x
render' (x:xs) = render' xs <> (" → " <> maybe mempty (pack . show) x)

render :: Int -> IntMap Level -> Int -> IntMap Rational -> Text
render len z time m =
    let (a, b) = expandRev time m'
        m' = fmap (fromIntegral . numerator) m
        t :: Maybe String
        t = a >>= level z
    in maybe "정보 없음" pack t <> (' ' `T.cons` render' (a:take len b))

fromText :: Token' a -> Maybe a
fromText (Text a) = Just a
fromText _ = Nothing

getText :: Monad m => (Token' a -> Bool) -> Consumer (Token' a) m [a]
getText f = do
    a <- C.takeWhile f =$= C.map fromText =$= C.sinkList
    return $ catMaybes a

script :: Monad m => Consumer Token m (HashMap String (IntMap Rational))
script = untilJust $ do
    let notOpen (TagOpen "script" _ _) = False
        notOpen _ = True
    C.dropWhile notOpen
    _ <- await

    txt <- C.mapWhile fromText =$= C.sinkList
    case splitOn "\nvar wrapper;" (mconcat txt) of
        x:_:_ -> return (Just . javascript . readJs $ unpack x)
        _ -> return Nothing

crawl' :: Monad m => Consumer Token m (HashMap String (IntMap Rational), Int, Text, Text)
crawl' = do
    a <- script
    b <- ttr2
    placeName <- tr
    addr <- tr
    return
        ( a
        , read . unpack . T.takeWhile isDigit . T.tail . T.dropWhile (/= ' ') $ mconcat b
        , mconcat placeName
        , mconcat addr
        )

crawl :: Monad m => Consumer Text m [Text]
crawl = tokenStream =$= do
    (m, time, placeName, addr) <- crawl'
    let pm10' = render 12 pm10 time $ lookupDefault mempty "pm10" m
        pm25' = render 12 pm25 time $ lookupDefault mempty "pm25" m
        place = placeName <> ": " <> addr
    return
        [ pack (show time) <> "시 기준, " <>  place
        , "PM10 (㎍/㎥, 시간 단위): " <> pm10'
        , "PM2.5(㎍/㎥, 시간 단위): " <> pm25'
        ]

tr :: Monad m => Consumer Token m [Text]
tr = C.find nextTr >> getText (/= TagClose "tr")
  where nextTr (TagOpen "tr" [] _) = True
        nextTr _ = False

ttr2 :: Monad m => Consumer Token m [Text]
ttr2 = C.find dropFirstTr >> tr
  where dropFirstTr (TagOpen "tr" xs _)
            | lookup "id" xs == Just "ttr2" = True
            | otherwise = False
        dropFirstTr _ = False

--main = runResourceT (sourceFile "../sub11.jsp" =$= decode utf8 =$= tokenStream $$ crawl) >>= mapM_ T.putStrLn
