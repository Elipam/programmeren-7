-- 5a
import Data.List
pow :: Integer -> Integer -> Integer
pow x p
    | p == 0 = 1
    | otherwise = x * pow x (p - 1)

  {- 
  pow 1 9100000 kon die nog wel maar
  pow 1 9200000 leverde een stack overflow op
  -}
-- 5b
