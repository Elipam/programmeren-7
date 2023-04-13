import Text.XHtml (base)
import Distribution.ModuleName (main)
-- 1a
euclid :: Integer -> Integer -> Integer
euclid x y
  | y == 0    = x
  | otherwise = euclid y (x `mod` y)

-- 1b
egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (mod b a) a
    in (neg g modulo, neg(t- div b a * s)modulo, neg s modulo)
    where modulo = mod a b

neg :: Integer -> Integer -> Integer
neg x m
  | x < 0 = x + m
  | otherwise = x


-- 2
--3a
--3b
-- 4
-- 5
-- 6