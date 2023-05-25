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
    in (g, neg(t - div b a * s)modulo, neg s modulo)
    where modulo = mod a b

neg :: Integer -> Integer -> Integer
neg x m
  | x < 0 = x + m
  | otherwise = x
 
-- Neem 2 priemgetallen p en q
-- m  = p * q
-- Phi(m) = (p-1)(q-1)
-- Kies een getal dat kleiner is dan phi en waarvan de modulus van e mod m 1 is
-- public key: (e, m)
-- (d * e)mod phi = 1, hiervoor is d = (phi * n > 0) - 1
-- private key: (d, m)

-- priemgetallen: 2 en 7
-- m = 2 * 7 = 14 
-- phi = 1 * 6 = 6
-- (e < 6 en e mod phi = 1) = 5 is encrypt 
-- d = 6 * 2 - 1 = 11 is decrypt

-- 2
modulus::Integer -> Integer -> Integer
modulus p q = p*q

modulusAccent::Integer -> Integer -> Integer
modulusAccent p q = (p-1)*(q-1)

privateKey:: Integer -> Integer
privateKey ma = head [a |a<-[100..ma-1], euclid a ma == 1]

publicKey:: Integer -> Integer -> Integer
publicKey e ma = head [d | d <- [100..], (e * d) mod ma == 1]

generateKey::Integer ->Integer -> (Integer, Integer, Integer)
generateKey p q = (e,d,m)
  where
    ma = modulusAccent p q
    e = privateKey ma
    d = publicKey e ma
    m = modulus p q

--3a
rsaencrypt :: (Integer, Integer) -> Integer -> Integer
rsaencrypt (e,m) x = (x^e) `mod` m

--3b
rsadecrypt :: (Integer, Integer) -> Integer -> Integer
rsadecrypt (d,m) x = x^d `mod` m

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