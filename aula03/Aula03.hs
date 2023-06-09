-- |
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Aula03 where

foo :: [String] -> String
foo [] = 1
foo (x : xs) = x * foo xs

instance Num String where
  x * y = show $ (length x) * (length y)

bools :: [Bool]
bools = [True, True, False, True, False, False]

nums  :: [[Int]]
nums = [[1,2,3,4], [5,6,7,8]]

add   :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy  :: a -> (a, a)
copy a = (a, a)

-- apply :: (a -> b) -> a -> b
-- apply (a -> b) = a b

swap  :: (a,b) -> (b,a)
swap (a, b) = (b, a)