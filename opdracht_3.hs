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
select [1, 2, 3, 4, 5] = "Straight"
select [2, 3, 4, 5, 6] = "Straight"
select [a]
    | heeftWaarde [a] 1 && heeftWaarde [a] 6 = "Bust"
    | aantal == 4 = "One pair"
    | aantal == 3 && aantalPairs [a] == 2 = "Two pairs"
    | aantal == 3 = "Three of a kind"
    | aantal == 2 = "Full house or Four of a kind"
    | aantal == 1 = "Poker"
    where
    aantal = length (group [a])

heeftWaarde :: [Float] -> Float -> Bool
heeftWaarde xs x = x `elem` xs

aantalPairs :: [Float] -> Int
aantalPairs xs = length (filter (\x -> length x == 2) (group xs))

select2 :: [Float] -> String
select2 xs
    | xs == xs = "ok"

game :: [Float] -> Float
game a = kans(select a)
