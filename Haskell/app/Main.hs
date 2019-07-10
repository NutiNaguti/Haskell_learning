module Main where

import Lib

evenSum :: [Integer] -> Integer
evenSum l = accumSum 0 l

accumSum n l = if l == []
                  then n
                  else let x = head l 
                           xs = tail l 
                       in if even x
                              then accumSum (n+x) xs
                              else accumSum n xs

main :: IO ()
main = do
    print $ evenSum [1..4]
    