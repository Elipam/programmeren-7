import Data.Bits (Bits (shiftL, shiftR))

-- 1a
-- Berekent de faculteit dmv pattern matching
faca :: Integer -> Integer
faca 0 = 1
faca x = x * faca (x - 1)

-- 1b
-- Berekent de faculteit dmv guards
facb :: Integer -> Integer
facb x
  | x == 0 = 1
  | x > 0 = x * facb (x - 1)

-- 2a
-- Berekent de nulpunten van tweedegraadsfunctie mbv where functie
nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c =
  [(-b + discriminant) / (2 * a), (-b - discriminant) / (2 * a)]
  where
    discriminant = sqrt (b * b - 4 * a * c)

-- 2b
-- Berekent de nulpunten van tweedegraadsfunctie dmv guards en where keyword
nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
  | d < 0 = []
  | d == 0 = [-b / (2 * a)]
  | d > 0 = [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
  where
    d = b * b - 4 * a * c

-- 2c
-- Gooit dobbelsteen a, b en c en geeft de combinaties die opgeteld een veelvoud van 5 zijn terug door som van a,b en c door 5 te delen en modulus met 0 te verrgelijken
dobbelstenen :: [(Integer, Integer, Integer)]
dobbelstenen = [(a, b, c) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], (a + b + c) `mod` 5 == 0]

-- 2d
-- Zelfde functie, maar nu kan je zelf de deler bepalen
dobbelstenen2 :: Integer -> [(Integer, Integer, Integer)]
dobbelstenen2 n = [(a, b, c) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], (a + b + c) `mod` n == 0]

-- 3
-- Maakt permutaties met alle getalen tussen de -100 en 100, stelt daarna de voorwaarden
opg3 :: [(Integer, Integer, Integer)]
opg3 = [(a, b, c) | a <- [-100 .. 100], b <- [-100 .. 100], c <- [-100 .. 100], a == 2 * (b - c), b == a * c, c * 2 == a + b]

-- 4a
-- Vermenigvuldigt dmv optellen
mult :: Integer -> Integer -> Integer
mult a b
  | b <= 0 = 0
  | otherwise = a + mult (b - 1) a

{-
De functie mult geeft een stackoverflow bij mult 1000000000000 4500000.
Zo lang een van de twee cijfers 45????? of daaronder blijft is er geen stackoverflow.
-}

-- 4b
-- Vermenigvuldigt dmv bitshifting en restanten bij oneven getallen op te tellen.
fastmult :: Integer -> Integer -> Integer
fastmult a b
  | a == 0 || b == 0 = 0
  | even b = fastmult (shiftL a 1) (shiftR b 1)
  | otherwise = a + fastmult (shiftL a 1) (shiftR b 1)

{-
fastmult kan met de getallen die bij de mult functie een stackoverflow veroorzaken wel de berkening doen.
-}

-- 5a
-- Zet x in macht van p dmv recursie.
pow :: Integer -> Integer -> Integer
pow x p
  | p == 0 = 1
  | otherwise = x * pow x (p - 1)

{-
pow 1 9100000 haalde het programma nog wel
pow 1 9200000 leverde een stack overflow op
-}

-- 5b
-- Zet x in de macht van p mbv bitshifting.
fastpow :: Integer -> Integer -> Integer
fastpow _ 0 = 1
fastpow 0 _ = 0
fastpow x p
  | even p = fastpow (x * x) (shiftR p 1)
  | otherwise = x * fastpow (x * x) (shiftR p 1)

{-
Het duurt een tijdje maar het programma kan uiteindelijk wel de values die de mult functie een stackoverflow geven aan
-}
