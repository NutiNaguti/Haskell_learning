module Main where

import Lib

evenSum :: Integral a => [a] -> a
evenSum l = accumSum 0 l
    where accumSum n l = 
            if l == []
                then n
                else let x = head l 
                         xs = tail l 
                     in if even x
                            then accumSum (n+x) xs
                            else accumSum n xs
 
fib :: Int -> Integer
fib n = 
    if n < 2
        then n
        else fib (n-1) + fib (n-2)

factorial :: Int -> Int
factorial n = 
    if n < 1
        then 1
        else n * factorial(n-1)

bubbleSort :: [Integer] -> [Integer]
bubbleSort l = sort 0 l
    where sort n l = 
        if l == []
            then n
            else let x = l   

main :: IO ()
main = do
    print $ evenSum [1..4]
    print $ fib 10
    print $ factorial 10