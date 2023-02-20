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
dobbelstenen = [ (a,b,c) | c <- [1..6], b <- [1..6], a <- [1..6], (a + b + c) `mod` 5 == 0 ]

-- 2d
dobbelstenen2 n = [ (a,b,c) | c <- [1..6], b <- [1..6], a <- [1..6], (a + b + c) `mod` n == 0 ]