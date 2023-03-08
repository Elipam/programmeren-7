import Data.Bits ( Bits(shiftL, shiftR) )
-- 1a 
faca :: Integer -> Integer 
faca 0 = 1 
faca n = n * faca ( n - 1 ) 

-- 1b
facb :: Integer -> Integer 
facb n | n == 0 = 1 
       | n /= 0 = n * facb (n-1) 



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



opg3 :: [(Integer, Integer, Integer)]
opg3 = [(x, y, z)|x<-[-100..100],y<-[-100..100],z<-[-100..100],x==2*(y-z),y==x*z,z*2==x+y]



-- 4a
mult :: Integer->Integer->Integer 
mult a b  
    | a <= 0    = 0  
    | otherwise = b + mult (a-1) b  

-- 4b



-- 5a
pow :: Integer -> Integer -> Integer
pow x p
    | p == 0 = 1
    | otherwise = x * pow x (p - 1)

  {- 
  pow 1 9100000 kon die nog wel maar
  pow 1 9200000 leverde een stack overflow op
  -}


-- 5b
fastpow :: Integer -> Integer -> Integer
fastpow x 0 = 1
fastpow x p
    | even p    = fastpow (x * x) (shiftR p 1)
    | otherwise = x * fastpow (x * x) (shiftR p 1)