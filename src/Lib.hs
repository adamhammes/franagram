module Lib
  ( combinations
  , anagramN
  , anagram
  , canonicalForm
  , buildMapping
  ) where

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize

buildMapping :: [String] -> (String -> [String])
buildMapping words = \s -> Map.findWithDefault [] s mapping
  where mapping = Map.fromListWith (++) (zip keys values)
        keys = map canonicalForm words
        values = map (: []) words


anagram :: (String -> [String]) -> String -> [String]
anagram mapping input = concatMap (anagramN mapping input) [len, len - 1 .. 1]
  where len = length input

anagramN :: (String -> [String]) -> String -> Int -> [String]
anagramN mapping input len = concatMap mapping (combinations len input)


combinations :: Int -> [b] -> [[b]]
combinations n l
  | n <= 0 = [[]]
  | n < len = withFirst ++ without
  | n == len = [l]
  | n >= len = []
  where
    withFirst = map (x :) (combinations (n - 1) xs)
    without = combinations n xs
    (x:xs) = l
    len = length l

canonicalForm :: String -> String
canonicalForm s = sort (T.unpack noAccents)
  where noAccents = T.filter (not . property Diacritic) normalized
        normalized = normalize NFD (T.pack s)
