-- 1a
euclid :: Integer -> Integer -> Integer
euclid x y
  | y == 0    = x
  | otherwise = euclid y (mod x y)

-- 1b
egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (mod b a) a
    in (g, t - (div b a) * s, s)
--
-- 2
--3a
--3b
-- 4
-- 5
-- 6