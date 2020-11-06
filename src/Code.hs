{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Code where

import Data.Numbers.Primes
import Language

type GodelNumber = Int

class GodelEncode a where
  f :: a -> GodelNumber

class GodelDecode a where
  g :: GodelNumber -> a

instance (GodelEncode a, GodelEncode b) => GodelEncode (a, b) where
  f (a, b) = 2 ^ f a * ((2 * f b) + 1) - 1

instance GodelEncode [GodelNumber] where
  f as = product (zipWith (^) as (primes :: [Int]))

instance GodelEncode Variable where
  f (X i) = 2 * (i - 1) + 1
  f Y = 0
  f (Z i) = 2 * (i - 1) + 2

instance GodelEncode Label where
  f (Label 'A' n) = 0 + 5 * pred n
  f (Label 'B' n) = 1 + 5 * pred n
  f (Label 'C' n) = 2 + 5 * pred n
  f (Label 'D' n) = 3 + 5 * pred n
  f (Label 'S' n) = 4 + 5 * pred n

l :: GodelNumber -> GodelNumber
l c = head (filter (\t -> not ((2 ^ (t + 1)) `isDivisor` (c + 1))) [0 .. c])

r :: GodelNumber -> GodelNumber
r x = ((x + 1) `div` 2 ^ l x - 1) `div` 2


(!) :: GodelNumber -> Int -> GodelNumber
(!) x i = (head . filter (\t -> not (((primes !! i) ^ (t + 1)) `isDivisor` x))) [0..x]

long :: GodelNumber -> Int
long x = (head . filter (\i -> x ! i /= 0 || all (\j -> x ! j == 0) [1..(x + 1)])) [1..x]

isDivisor :: Integral a => a -> a -> Bool
isDivisor a b = b `rem` a == 0
