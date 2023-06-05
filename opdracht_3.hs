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
faca :: Float -> Float
faca 0 = 1
faca n = n * faca ( n - 1 )

permutaties :: Float
permutaties = faca 5

permutatiesGelijk :: Float -> Float
permutatiesGelijk x = permutaties / faca x

permutaties2Gelijk :: Float -> Float -> Float
permutaties2Gelijk x y = permutaties / (faca x * faca y)

kans :: String -> Float
kans a
    | a == "One pair" = permutatiesGelijk 2 / permutaties
    | a == "Two pair" = permutaties2Gelijk 2 2 / permutaties

select :: [Float] -> String
select xs 
    | aantal == 5 = select2 xs
    | aantal == 4 = "One pair"
    | aantal == 3 = "Two pair or Three of a kind"
    | aantal == 2 = "Full house or Four of a kind"
    | aantal == 1 = "Poker"
    where aantal = length(group xs)

select2 :: [Float] -> String
select2 xs
    | xs == xs = "ok"

game :: [Float] -> Float
game a = kans(select a)
