module Main where

import Lib
import Data.List

evenSum :: Integral a => [a] -> a
evenSum = accumSum 0
    where 
        accumSum n [] = n
        accumSum n (x:xs) = 
             if even x
                then accumSum (n+x) xs
                else accumSum n xs

fib :: Int -> Int
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
root a b c  = (0 - b) - sqrt (discr)
    where discr = (b^2) - (4 * a * c)

main :: IO ()
main = do
    print $ evenSum [1..4]
    print $ fib 10
    print $ factorial 10
    print $ root 1 (-14) 5
