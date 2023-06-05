import Data.List

-- 1a
differentieer :: (Double -> Double) -> Double -> Double -> Double
differentieer f p x = (f (x + p) - f x) / p

-- 1b
integreer :: (Double -> Double) -> Double -> Double -> Double -> Double
integreer f a b p = sum [f (a + (b - a)) * (b - a)]

-- 2
dubbelen :: Eq a => [a] -> [a]
dubbelen s = nub [x | x <- s, count x > 1]
  where
    count x = length (filter (== x) s)

-- 3
