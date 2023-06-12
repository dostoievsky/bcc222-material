module Aula01 where

sumUpTo :: Int -> Int
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fact :: Int -> Int
fact 0 = 1
fact n = n * factorial (n - 1)

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)