module Aula06 where

import Prelude hiding ( 
  foldr, foldl, sum, concat, 
  and, length, map, reverse, 
  filter, takeWhile, all,
  concatMap)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v []       = v
foldr f v (x : xs) = x `f` foldr f v xs

sum = foldr (+) 0

concat = foldr (++) []

and = foldr (&&) True

length :: [a] -> Int
length = foldr step 0
   where
     step _ ac = 1 + ac

map :: (a -> b) -> [a] -> [b]
map f = foldr step []
   where
     step x ac = f x : ac

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v []       = v
foldl f v (x : xs) = foldl f (f v x) xs

reverse = foldl step []
   where
     step ac x = x : ac

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr step []
   where
     step x ac = if p x then x : ac else ac

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []  = []
takeWhile p (x : xs)
    | p x       = x : takeWhile p xs
    | otherwise = []

takeWhileF :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr step []
  where
    step x ac = if f x then x : ac else ac  

all :: (a -> Bool) -> [a] -> Bool
all _ []      = True
all p (x : xs)
  | p x       = all p xs
  | otherwise = False

allF :: (a -> Bool) -> [a] -> Bool
all f = foldr step True
  where
    step x xs = if f x then xs else False  

-- concatMap :: (a -> [b]) -> [a] -> [b]
-- concatMap _ [] = []
-- concatMap f xs ys

-- concatMapF :: (a -> [b]) -> [a] -> [b]
-- concatMapF f = foldr step []
--   where
--     step   
