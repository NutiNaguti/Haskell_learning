{-# LANGUAGE MultiWayIf #-}
module Main where

import Lib
import Data.List
import Data.Char

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
transp (x1:x2:xs) | (x2 < x1) = (n+1, x2:ts1)  
                  | otherwise = (m, x1:ts2)  
                    where (n,ts1)=transp (x1:xs)
                          (m,ts2)=transp (x2:xs)
                          
-- Рекурсивное повторение transp до тех пор, 
-- пока число перестановок не станет = 0 (список отсортирован)
 
bSort :: (Ord a) => [a] -> [a]
bSort x | (n==0) = sx
        | otherwise = bSort sx 
          where (n,sx) = transp x

------------------------------------------------------------------------------

root a b c  = (0 - b) + sqrt (discr)
    where discr = (b^2) - (4 * a * c)

------------------------------------------------------------------------------

--рекурсивное возведение каждого элемента списка в квадрат.

list :: Num a => [a] -> [a]
list [] = []
list (x:xs) = x^2 : list xs

fltr :: (a -> Bool) -> [a] -> [a]
fltr _ [] = []
fltr f (x:xs) = if
    | f x -> x : fltr f xs
    | otherwise -> fltr f xs    


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = 
    if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

-------------------------------------------------------------------------------
-- кортежи

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = 
    abs (sqrt (first + second))
    where 
        first = (fst p2 - fst p1) ^ 2
        second =  (snd p2 - snd p1) ^ 2

doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 0 = 1
doubleFact n = if
    n < 0
    then error "A negative number"
    else
        n * doubleFact (n - 2)
    
doubleFact' :: Integer -> Integer
doubleFact' n   | n == 1 = 1
                | n == 0 = 1
                | n < 0 = error "A negative number"
                | otherwise = n * doubleFact (n - 2)

fib' :: Integer -> Integer
fib' n  | n == 1 = 1
        | n == 0 = 0
        | n > 1 = fib'(n - 1) + fib'(n - 2)
        | otherwise = fib'(n + 2) - fib' (n + 1)

-- хвостовая рекурсия 
fibonacci n = func 0 1 n
        where func a b n    | n == 0   = a
                            | n > 0    = func (a+b) a (n-1)
                            | n < 0    = func b (a-b) (n+1)
-- TODO
-- fibonacci n = func 1 n
-- where func a acc    | a == n = acc
--                     | otherwise = func (a + 1) ((acc - 1) + (acc - 2)) 
---------------------------------------------------------------
--бесконечный ряд чисел Фибоначчи

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

------------------------------------------------------------------------
-- TODO
-- seqA :: Integer -> Integer
-- seqA n = func' 1 2 3 n
--         where func' a b c n | n == 0 = a
--                             | a >= 0 && a <= 2 = a + 1
--                             | otherwise = func'((a-1) + (a-2) - 2 * (a-3)) (n-1) 


-- | n == 0 = 1
-- | n == 1 = 2
-- | n == 2 = 3
-- | otherwise = seqA(n-1) + seqA(n-2) - 2 * seqA(n-3) 

----------------------------------------------------------------------


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x   | x == 0 = (0, 1)
                | otherwise = (a x, b x)
                    where 
                        b x | abs x < 10 = 1 
                            | otherwise = 1 + b (div x 10)
                        a x | abs x < 10 = x
                            | otherwise = a (div (abs x) 10) + mod (abs x) 10

------------------------------------------------------------------

-- интегрирование методом трапеции 
-- TODO

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = undefined

------------------------------------------------------------------
{--Напишите функцию трех аргументов getSecondFrom, 
полиморфную по каждому из них, 
которая полностью игнорирует первый и третий аргумент, 
а возвращает второй. Укажите ее тип.--}

getSecondFrom :: a -> b -> c -> b
getSecondFrom x y z = y

----------------------------------------------------------------

main :: IO ()
main = do
    -- print $ evenSum [1..4]
    -- print $ fib 10
    -- print $ factorial 10
    -- print $ root 1 (-14) 5
    -- print $ list [1..10]
    -- print $ fltr (>3) [1..10]
    -- print $ bSort [1, 4, 6, 2, 3, 8, 9, 0, 5]
    -- print $ twoDigits2Int '3' '4'
    -- print $ dist (1, 0) (1, 0)
    -- print $ doubleFact 10
    -- print $ fib' 10
    -- print $ fibonacci 3
    -- print $ sum'n'count 234
    -- print $ getSecondFrom 2 'g' "wef"
    print $ sum'n'count (1231)
