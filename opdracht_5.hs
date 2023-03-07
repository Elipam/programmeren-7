-- 5a
import Data.List
pow :: Integer -> Integer -> Integer
pow x p
    | p == 0 = 1
    | otherwise = x * pow x (p - 1)

-- ghci> pow 2 10000000
-- Exception: stack overflow

-- 5b
fastpow::Integer->Integer->Integer
fastpow x p