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
floatLength :: [a] -> Float
floatLength [] = 0
floatLength (_:xs) = 1 + floatLength xs

verbeterdeDubbelen :: (Eq a, Ord a) => [a] -> (Int, Int)
verbeterdeDubbelen xs = (berekenLengte, berekenSets)
    where   berekenLengte = length $ concat [x | x <- group $ sort xs, length x >= 2]
            berekenSets = length [x | x <- group $ sort xs, length x >= 2]

lengteSetVergelijking :: Ord a => (Int, Int) -> [[a]] -> Float
lengteSetVergelijking _ [] = 0
lengteSetVergelijking (a,b) (x:xs)
    | verbeterdeDubbelen x == (a,b) = 1 + lengteSetVergelijking (a,b) xs
    | otherwise = lengteSetVergelijking (a,b) xs

straightUitzondering :: [[Int]] -> Float
straightUitzondering [] = 0
straightUitzondering (x:xs)
    | sort x == [1..5] || sort x == [2..6] = 1 + straightUitzondering xs
    | otherwise = straightUitzondering xs

kans :: String -> Float
kans a
    | a == "Poker" = lengteSetVergelijking (5,1) alleMogelijkheden / floatLength alleMogelijkheden
    | a == "Four of a kind" = lengteSetVergelijking (4,1) alleMogelijkheden / floatLength alleMogelijkheden 
    | a == "Full house" = lengteSetVergelijking (5,2) alleMogelijkheden / floatLength alleMogelijkheden
    | a == "Three of a kind" = lengteSetVergelijking (3,1) alleMogelijkheden / floatLength alleMogelijkheden
    | a == "One pair" = lengteSetVergelijking (2,1) alleMogelijkheden / floatLength alleMogelijkheden
    | a == "Two pair" = lengteSetVergelijking (4,2) alleMogelijkheden / floatLength alleMogelijkheden
    | a == "Straight" = straightUitzondering alleMogelijkheden / floatLength alleMogelijkheden
    | a == "Bust" = 1 - (kans "Straight" + kans "One pair" + kans "Two pair" + kans "Three of a kind" + kans "Four of a kind" + kans "Full house" + kans "Poker")
    where mogelijkheden = 6^5
                
alleMogelijkheden :: [[Int]]
alleMogelijkheden = [[a, b, c, d, e] | a <- [1..6], b <- [1..6], c <- [1..6], d <- [1..6], e <- [1..6]]