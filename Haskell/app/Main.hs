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

------------------------------------------------------------------------------
-- Однократный прогон перестановок по списку.
-- Возвращает кортеж (n,ps), где n - число совершенных перестановок, 
-- а ps - частично отсортированный список
 
transp :: (Ord a) => [a] -> (Int, [a]) 
transp [x] = (0,[x])
transp (x1:x2:xs) | (x2 < x1) = (n+1, x2: ts1)  
                  | otherwise = (m, x1 :  ts2)  
                    where (n,ts1)=transp (x1:xs)
                          (m,ts2)=transp (x2:xs)
                          
-- Рекурсивное повторение transp до тех пор, 
-- пока число перестановок не станет = 0 (список отсортирован)
 
bSort :: (Ord a) => [a] -> [a]
bSort x | (n==0) = sx
        | otherwise = bSort sx 
          where (n,sx) = transp x

------------------------------------------------------------------------------

main :: IO ()
main = do
    print $ evenSum [1..4]
    print $ fib 10
    print $ factorial 10