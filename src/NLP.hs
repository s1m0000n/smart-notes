{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module NLP where

import qualified Data.Text as Text
import qualified Data.List as LST
import DataStructures ( TextSplits(..), TextStats(..) )
import Finder ( hasAny )

splitSents :: String -> [String]
splitSents text = map Text.unpack $ foldr
  ((\ y x -> if y == "" then x else y : x) . Text.strip) []
  (Text.split (\ x -> x == '.' || x == '!' || x == '?') $ Text.pack text)


splitNgrams :: Int -> [a] -> [[a]]
splitNgrams n (x:xs) = (x : take (n - 1) xs) : splitNgrams n xs
splitNgrams n [] = []

sentsNGrams :: Int -> String -> [[[Char]]]
sentsNGrams n text = map (concatMap (splitNgrams n) . words) (splitSents text)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

summarize :: Int -> [Char] -> [String]
summarize n text = map snd $ take n sorted_pairs where
-- summarize n text = sorted_pairs where
  sents = splitSents text
  max_sent_len = maximum $ map (length . words) sents
  sents_ngrams = sentsNGrams 4 text ++ sentsNGrams 3 text ++ sentsNGrams 5 text
  all_ngrams = concat sents_ngrams
  unique_ngrams = foldl (\x y -> if [y] `hasAny` x then x else y:x) [] all_ngrams
  ngrams_stats = map (\ungram -> (count ungram all_ngrams, ungram)) unique_ngrams
  -- min-max normalize !
  sentence_scores = map (sum . map (\sngram -> fst $ head $ filter (\x -> snd x == sngram) ngrams_stats)) sents_ngrams
  sorted_pairs = LST.sortBy (\(a, _) (b, _) -> compare b a) 
    $ filter (\(_, sent) -> length (words sent) >= 3)
    -- $ map (\(score, sent) -> (score * (max_sent_len/(length (words sent))), sent))
    $ zip sentence_scores sents


getNoteStats :: [Char] -> TextStats
getNoteStats text = TextStats {
  symbols_ = length text, 
  words_ = length $ words text, 
  sents_ = length $ splitSents text
}

getNoteSplits :: String -> TextSplits
getNoteSplits text = TextSplits{
  _words_=words text,
  _sents_=splitSents text
}