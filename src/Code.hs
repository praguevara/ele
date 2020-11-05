module Code where

import Language

encodeVariable :: Variable -> Int
encodeVariable v = case v of
  Y -> 0
  X i -> 2 * (i - 1) + 1
  Z i -> 2 * (i - 1) + 2

-- encodeLabel :: Label -> Int
-- encodeLabel (Label l i) = undefined
--   where f l = case l of
--     'A' -> 0
--     'B' -> 1
--     'C' -> 2
--     'D' -> 3
--     'S' -> 4

l :: Int -> Int
l c = head (filter (\t -> not ((2 ^ (t + 1)) `isDivisor` (c + 1))) [0..c])
  where
    isDivisor a b = b `rem` a == 0

r :: Int -> Int
r x = ((x + 1) `div` 2 ^ l x - 1) `div` 2

