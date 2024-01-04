module Euler where

import Data.Bits
import Data.List

-- Ex 1
sumOfMults :: Int -> Int -> Int -> Int
sumOfMults x y n = sum $ [z | z <- [1 .. n - 1], z `mod` x == 0 || z `mod` y == 0]

-- Ex 2
fib :: Int -> Integer
fib n =
    snd
        . foldl' fib_ (1, 0)
        . dropWhile not
        $ [testBit n k | k <- let s = bitSize n in [s - 1, s - 2 .. 0]]
  where
    fib_ (f, g) p
        | p = (f * (f + 2 * g), ss)
        | otherwise = (ss, g * (2 * f - g))
      where
        ss = f * f + g * g

sumOfFibs :: Integer
sumOfFibs = sum $ filter even $ map fib $ takeWhile ((<= 4 * 10 ^ 6) . fib) [1 ..]
