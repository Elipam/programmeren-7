-- 1a
euclid :: Integer -> Integer -> Integer
euclid x y
  | y == 0    = x
  | otherwise = euclid y (x `mod` y)
-- 1b
-- 2
--3a
--3b
-- 4
-- 5
-- 6