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
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

doubleSmallNumber x = if x > 100  
                    then x  
                    else x*2 
premutaties :: [Int]
take 10 (5) 
-- 2d
    