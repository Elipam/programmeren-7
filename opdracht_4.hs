-- 4a
mult :: Integer->Integer->Integer 
mult a b  
    | a <= 0    = 0  
    | otherwise = b + mult (a-1) b  

-- 4b
