-- 5a
import Data.List
pow :: Integer -> Integer -> Integer
pow x p
    | p == 0 = 1
    | otherwise = x * pow x (p - 1)

-- 5b
