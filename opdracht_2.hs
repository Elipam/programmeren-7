-- 2a
{-nulpuntena :: Double->Double->Double->[Double]
nupuntena a b c
    |d < 0 = []
    |d == 0 = [-b/(2 * a)]
    |d > 0 = [(-b + sqrt d) / (2* a), (-b - sqrt d) / (2* a)]
    d = b * b - 4 * a * c  -}

nulpuntena :: Double->Double->Double->[Double]  
nulpuntena a b c  
    | d < 0 = []  
    | d == 0 = [1,2] 
    | d > 0 = [1,2,3,d]
    where d = b * b - 4 * a * c
-- 2b

-- 2c

-- 2d
    