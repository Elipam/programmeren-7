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
-- verbeterdeDubbelen :: (Eq a, Ord a) => Int -> [a] -> [a]
-- verbeterdeDubbelen a xs = nub . concat $ [ x | x <- group $ sort xs, length x == a]

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
    | a == "Poker" = lengteSetVergelijking (5,1) allRolls / mogelijkheden
    | a == "Four of a kind" = lengteSetVergelijking (4,1) allRolls / mogelijkheden 
    | a == "Full house" = lengteSetVergelijking (5,2) allRolls / mogelijkheden
    | a == "Three of a kind" = lengteSetVergelijking (3,1) allRolls / mogelijkheden
    | a == "One pair" = lengteSetVergelijking (2,1) allRolls / mogelijkheden
    | a == "Two pair" = lengteSetVergelijking (4,2) allRolls / mogelijkheden
    | a == "Straight" = straightUitzondering allRolls / mogelijkheden
    | a == "Bust" = 1 - (kans "Straight" + kans "One pair" + kans "Two pair" + kans "Three of a kind" + kans "Four of a kind" + kans "Full house" + kans "Poker")
    where mogelijkheden = 6^5
                
allRolls :: [[Int]]
allRolls = [[a, b, c, d, e] | a <- [1..6], b <- [1..6], c <- [1..6], d <- [1..6], e <- [1..6]]

-- nogbetereDubbelen :: (Eq a, Ord a) => [a] -> Float
-- nogbetereDubbelen xs
--     | bereken == 5 = 
--     where bereken = length(concat [x | x <- group $ sort xs, length x >= 2])

-- welOfNietDubbelen :: Ord a => Int -> [a] -> Bool
-- welOfNietDubbelen a xs
--     | null (verbeterdeDubbelen a xs) = False
--     | otherwise = True

-- telDubbelen :: Ord a => Int -> [[a]] -> Float
-- telDubbelen _ [] = 0
-- telDubbelen a (x:xs)
--     | welOfNietDubbelen a x = 1 + telDubbelen a xs
--     | otherwise = telDubbelen a xs


-- Definieer een dobbelsteen als een getal van 1 tot en met 6
type Die = Int

-- Een worp van 5 dobbelstenen
type Roll = [Die]

-- Functie om te controleren of een worp een poker is
isPoker :: Roll -> Bool
isPoker roll = any (\x -> length x == 5) $ group $ sort roll

-- Functie om te controleren of een worp een four of a kind is
isFourOfAKind :: Roll -> Bool
isFourOfAKind roll = any (\x -> length x == 4) $ group $ sort roll

-- Functie om te controleren of een worp een three of a kind is
isThreeOfAKind :: Roll -> Bool
isThreeOfAKind roll = any (\x -> length x == 3) $ group $ sort roll

-- Functie om te controleren of een worp een full house is
isFullHouse :: Roll -> Bool
isFullHouse roll = isThreeOfAKind roll && isOnePair roll

-- Functie om te controleren of een worp two pairs heeft
isTwoPair :: Roll -> Bool
isTwoPair roll = length (filter (\x -> length x == 2) $ group $ sort roll) == 2

-- Functie om te controleren of een worp one pair heeft
isOnePair :: Roll -> Bool
isOnePair roll = any (\x -> length x == 2) $ group $ sort roll

-- Functie om te controleren of een worp een straight is
isStraight :: Roll -> Bool
isStraight roll = sort roll == [1,2,3,4,5] || sort roll == [2,3,4,5,6]

-- Functie om de overgebleven dobbelstenen te tonen (bust)
bust :: Roll -> [Die]
bust roll = roll \\ (nub roll)

-- Functie om de kans te berekenen voor een specifieke combinatie
-- Deze functie neemt een predicaat voor de combinatie en een lijst van worpen
-- en retourneert de kans voor die combinatie
calculateProbability :: (Roll -> Bool) -> [Roll] -> Double
calculateProbability predicate rolls = fromIntegral (length $ filter predicate rolls) / fromIntegral (length rolls)

-- Lijst van alle mogelijke worpen met 5 dobbelstenen

-- Functie om alle kansen voor de verschillende combinaties te berekenen
calculateAllProbabilities :: [Roll] -> [(String, Double)]
calculateAllProbabilities rolls =
    [ ("Poker", calculateProbability isPoker rolls)
    , ("Four of a Kind", calculateProbability isFourOfAKind rolls)
    , ("Full House", calculateProbability isFullHouse rolls)
    , ("Straight", calculateProbability isStraight rolls)
    , ("Three of a Kind", calculateProbability isThreeOfAKind rolls)
    , ("Two Pair", calculateProbability isTwoPair rolls)
    , ("One Pair", calculateProbability isOnePair rolls)
    , ("Bust", calculateProbability (\_ -> True) rolls) -- Alle worpen zijn Bust
    ]

