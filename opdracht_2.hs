import Text.XHtml (base)
import Distribution.ModuleName (main)
import Data.Sequence.Internal.Sorting (popMinQ)
import System.Win32 (xBUTTON1)
-- 1a
euclid :: Integer -> Integer -> Integer
euclid x y
  | y == 0    = x
  | otherwise = euclid y (x `mod` y)

-- 1b
egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (mod b a) a
<<<<<<< HEAD
    in do
      (g, t - div b a * s, s)
=======
    in (neg g modulo, neg(t- div b a * s)modulo, neg s modulo)
    where modulo = mod a b

neg :: Integer -> Integer -> Integer
neg x m
  | x < 0 = x + m
  | otherwise = x

>>>>>>> c9bbd7059f9adee950e893a5f83c5c3a8969de91
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

-- 5

-- 6