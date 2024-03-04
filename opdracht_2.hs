import Data.Char

-- 1a
euclid :: Integer -> Integer -> Integer
euclid x y
  | y == 0    = x
  | otherwise = euclid y (mod x y)

-- 1b
egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (mod b a) a
        modulo = mod a b
    in (neg g modulo, neg(t - div b a * s) modulo, neg s modulo)


neg :: Integer -> Integer -> Integer
neg x m
  | x < 0 = x + m
  | otherwise = x
 
-- Neem 2 priemgetallen p en q
-- m  = p * q
-- Phi(m) = (p-1)(q-1)
-- Kies een getal dat kleiner is dan phi en waarvan de modulus van dat getal e mod phi 1 is
-- public key: (e, m)
-- (d * e)mod phi = 1, hiervoor kan d = (phi * n > 0) - (phi - e)
-- private key: (d, m)

-- priemgetallen: 2 en 7
-- m = 2 * 7 = 14 
-- phi = 1 * 6 = 6
-- (e < 6 en e mod phi = 1) = 5 is encrypt 
-- d = 6 * 2 - 1 = 11 is decrypt

-- 2
modulus::Integer -> Integer -> Integer
modulus p q = p*q

phi::Integer -> Integer -> Integer
phi p q = (p-1)*(q-1)

eBereken::Integer -> Integer -> Integer
eBereken phi1 phi2
  | eCalc == 1    = phi1
  | otherwise = eBereken (phi1 - 1) phi2
  where eCalc = mod phi2 phi1

dBereken::Integer -> Integer -> Integer -> Integer
dBereken e phi randNum = (phi * randNum) - (phi - e)

generateKey::Integer -> Integer -> Integer -> [(Integer, Integer)]
generateKey p q r = [(e,m),(d,m)]
  where
    m = modulus p q
    m' = phi p q
    e = eBereken m' m'
    d = dBereken e m' r
        
--3a
rsaEncrypt :: (Integer, Integer) -> Integer -> Integer
rsaEncrypt (e,m) x = (x^e) `mod` m

--3b
rsaDecrypt :: (Integer, Integer) -> Integer -> Integer
rsaDecrypt (d,m) x = x^d `mod` m

-- 4
versleutel :: Char -> Int
versleutel = ord

ontsleutel :: Int -> Char
ontsleutel = chr

-- 5
-- Bob moet ook een public en private key aanmaken zodat Alice ook berichten naar Bob kan sturen

-- 6
-- Bob stuurt bericht naar Alice met de public key.
-- Alice kijkt naar bericht met de private key.
-- Man in the middle ziet het bericht ook, omodat hij de private key van Alice weet.