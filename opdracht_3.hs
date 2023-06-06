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
    | a == "Straight" = (6/6)*(5/6)*(4/6)*(3/6)*(2/6)*2
    | a == "One pair" = (permutatiesGelijk 2 / permutaties) - kans "Two pair" - kans "Three of a kind"
    | a == "Two pair" = (permutaties2Gelijk 2 2 / permutaties) - kans "Four of a kind"
    | a == "Three of a kind" = (permutatiesGelijk 3 / permutaties) - kans "Full house"
    | a == "Full house" = (permutaties2Gelijk 2 3 / permutaties) - kans "Poker"
    | a == "Four of a kind" = (permutatiesGelijk 4 / permutaties) - kans "Poker"
    | a == "Poker" = permutatiesGelijk 5 / permutaties
    | a == "Bust" = 1 - (kans "Straight" + kans "One pair" + kans "Two pair" + kans "Three of a kind" + kans "Full house" + kans "Four of a kind" + kans "Poker")

select :: [Float] -> String
select xs
    | aantal == 1 = "Poker"
    | aantal == 2 && max == 4 = "Four of a kind"
    | aantal == 2 = "Full house"
    | aantal == 3 && max == 2 = "Two pair"
    | aantal == 3 = "Three of a kind"
    | aantal == 4 = "One pair"
    | aantal == 5 && elem 1 xs && elem 6 xs = "Bust"
    | aantal == 5 = "Straight"
  where aantal = length list
        list = group (sort xs)
        max = maximum (map length list)

game :: [Float] -> (Float, String)
game a = (kans (select a), select a)
