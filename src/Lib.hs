module Lib
  ( combinations
  ) where

combinations :: Integral a => a -> [b] -> [[b]]
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
