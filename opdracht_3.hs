import Data.List

-- 1a
-- differentieer (\x -> x^2) 0.01 2.0
differentieer :: (Double -> Double) -> Double -> Double -> Double
differentieer f p x = (f (x + p) - f x) / p

-- 1b
--  integreer (\x -> x^3) 1.0 3.0 0.001
integreer :: (Double -> Double) -> Double -> Double -> Double -> Double
integreer f a b p = sum [f n * p | n <- [a+0*p, a+1*p..b]]

-- 2
dubbelen :: (Eq a, Ord a) => [a] -> [a]
dubbelen xs = nub . concat $ [ x | x <- group $ sort xs, length x > 1]

-- 3
factorial :: Float -> Float
factorial n = product [1..n]

combinaties :: Float -> Float -> Float
combinaties x y = factorial x / (factorial y * factorial (x - y))

kans :: String -> Float
kans a
    | a == "Straight" = 2 * (5 * 4 * 3 * 2) /7776 
    | a == "One pair" = factorial 3 * combinaties 6 1 * combinaties 5 2 * combinaties 5 3 / 7776 
    | a == "Two pair" = combinaties 6 2 * 4 * combinaties 5 3 * combinaties 3 2 / 7776 
    | a == "Three of a kind" = 6 * combinaties 5 2 * combinaties 5 3 * combinaties 2 1  / 7776 
    | a == "Four of a kind" = 6 * 5 * 5 / 7776 
    | a == "Full house" = 6 * 5 * combinaties 5 3 / 7776 
    | a == "Poker" = 6 / 7776 
    | a == "Bust" = 1 - (kans "Straight" + kans "One pair" + kans "Two pair" + kans "Three of a kind" + kans "Four of a kind" + kans "Full house" + kans "Poker")

select :: [Float] -> String
select xs
    | wrong  /= 5 = "Wrong input"
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
        wrong = length xs

game :: [Float] -> (Float, String)
game a = (kans (select a), select a)
