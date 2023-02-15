-- 1a 
faca :: Integer -> Integer 
faca 0 = 1 
faca n = n * faca ( n - 1 ) 

-- 1b
facb :: Integer -> Integer 
facb n | n == 0 = 1 
       | n /= 0 = n * facb (n-1) 

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  