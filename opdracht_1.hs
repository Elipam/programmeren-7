import Data.Bits ( Bits(shiftL, shiftR) )

-- 1a 
faca :: Integer -> Integer
faca 0 = 1
faca n = n * faca ( n - 1 )

-- 1b
facb :: Integer -> Integer
facb n | n == 0 = 1
       | n /= 0 = n * facb (n-1)



-- 2a
nulpuntena :: Double->Double->Double->[Double]
nulpuntena a b c = [(-b + sqrt (b * b - 4 * a * c))/ (2 * a), (-b - sqrt (b * b - 4 * a * c))/ (2 * a)]

-- 2b
nulpuntenb :: Double->Double->Double->[Double]
nulpuntenb a b c
       | d < 0 = []
       | d == 0 = [-b / (2 * a)]
       | d > 0 = [(-b + sqrt d)/ (2 * a), (-b - sqrt d)/ (2 * a)]
       where d = b * b - 4 * a * c

-- 2c
dobbelstenen :: [(Integer, Integer, Integer)]
dobbelstenen = [ (a,b,c) | a <- [1..6], b <- [1..6], c <- [1..6],   (a + b + c) `mod` 5 == 0 ]

-- 2d
dobbelstenen2 :: Integral c => c -> [(c, c, c)]
dobbelstenen2 n = [ (a,b,c) | a <- [1..6], b <- [1..6], c <- [1..6], (a + b + c) `mod` n == 0 ]



-- 3
opg3 :: [(Integer, Integer, Integer)]
opg3 = [(a, b, c)|a<-[-100..100],b<-[-100..100],c<-[-100..100],a==2*(b-c),b==a*c,c*2==a+b]



-- 4a
mult :: Integer->Integer->Integer
mult a b
    | b <= 0    = 0
    | otherwise = a + mult (b-1) a

{-
De functie mult geeft een stackoverflow bij mult 10000000 10000000.
1 nul minder kan nog wel, dan kan je ook zoveel cijfers toevoegen als je wel.
Het gaat erom dat als beide values boven 7 cijfers hebben dan geeft het programma een stackoverflow. 
-}

-- 4b
fastmult :: Integer -> Integer -> Integer
fastmult a b
    | a == 0 || b == 0 = 0
    | even b    = fastmult (shiftL a 1) (shiftR b 1)
    | otherwise = a + fastmult (shiftL a 1) (shiftR b 1)
{-
fastmult kan met de getallen die bij de mult functie een stackoverflow veroorzaken wel de berkening doen. 
-}



-- 5a
pow :: Integer -> Integer -> Integer
pow x p
    | p <= 0 = 1
    | otherwise = x * pow x (p - 1)
  {- 
  pow 1 9100000 haalde het programma nog wel 
  pow 1 9200000 leverde een stack overflow op
  -}

-- 5b
fastpow :: Integer -> Integer -> Integer
fastpow x 0 = 1
fastpow x p
    | x == 0 = 0 
    | even p    = fastpow (x * x) (shiftR p 1)
    | otherwise = x * fastpow (x * x) (shiftR p 1)
{-
Het duurt een tijdje maar het programma kan uiteindelijk wel de values die de mult functie een stackoverflow geven aan
-}
