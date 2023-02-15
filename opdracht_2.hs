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

-- 2d
    