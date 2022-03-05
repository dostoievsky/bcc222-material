module Aula04 where

import Prelude hiding ( length
                      , replicate
                      , sum
                      , reverse
                      , elem
                      , take
                      , (++)
                      , zip
                      )

length :: [a] -> Int
length []       = 0
length (_ : xs) = 1 + length xs

incAll :: [Int] -> [Int]
incAll xs = [x + 1 | x <- xs]

sumEvens :: [Int] -> Int
sumEvens xs = sum [x | x <- xs, even x]

heads :: [[a]] -> [a]
heads xss = [x | (x : _) <- xss]

qsort :: [Int] -> [Int]
qsort []       = []
qsort (pivot : xs) = smaller ++ [pivot] ++ greater
     where
       smaller = qsort [y | y <- xs, y <= pivot]
       greater = qsort [y | y <- xs, y > pivot]

sum :: Num a => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem x (y : ys) = x == y || elem x ys

(++) :: [a] -> [a] -> [a]
[]       ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

take :: Int -> [a] -> [a]
take 0 _        = []
take _ []       = []
take n (x : xs) = x : take (n - 1) xs

sorted :: Ord a => [a] -> Bool
sorted []           = True
sorted [_]          = True
sorted (x : y : xs) 
     | x <= y       = sorted (y : ys)
     | otherwise    = False

zip :: [a] -> [b] -> [(a,b)]
zip []       _        = []
zip _        []       = []
zip (x : xs) (y : ys) = (x,y) : zip xs ys
