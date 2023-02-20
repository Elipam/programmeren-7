opg3 ::(Num a, Fractional a) => (a, a, a)
opg3 = (x, y, z)
    where
        x = 2 * (y - z)
        y = x * z
        z = (x + y) / 2

-- UITLEG

-- "opg3" is de naam van de functie waarop de handtekening van toepassing is.

-- "(Num a, Fractional a)"" is een typebeperking die aangeeft dat de functie een type "a" vereist dat een 
-- instantie is van zowel de klassen "Num" als "Fractionele". Dit betekent dat de functie alleen 
-- numerieke typen kan accepteren die zich zowel als gehele getallen als als breukgetallen kunnen gedragen, 
-- zoals "Float" of "Double".

-- (a, a, a) specificeert het type invoer voor de functie. Het geeft aan dat de functie een tupel van 
-- drie waarden verwacht, elk van het type "a".

-- Samengevat geeft de typehandtekening "name :: (Num a, Fractional a) => (a, a, a)" 
-- aan dat de functie "name" een tuple van drie numerieke waarden heeft, elk van het type "a " 
-- die zich kan gedragen als zowel gehele als gebroken getallen, en retourneert een waarde 
-- waarvan het type wordt afgeleid uit de bewerkingen die in de functie worden uitgevoerd.