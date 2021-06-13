{- VEREM SZÁMOLÓGÉP -}
{-
   Ez a modul egy olyan számológép implementációját tartalmazza, amely egy
   veremmel dolgozik és a kiszámítandó aritmetikai kifejezést az ún. lengyel
   formában kapja meg. Ennek az a lényege, hogy a először az operandusokat,
   majd a műveletet adjuk meg, vagyis a kifejezést zárójelek nélkül, postfix
   alakban írjuk fel.
-}
module StackCalculator
( isDecimalDigit
, isSign
, isInteger
, strToInteger
, Stack
, push
, pop
, pop2
, IFun1
, IFun2
, findFunction
, step
, eval
, rpn
) where

-- | Decimális számjegyek vizsgálata
isDecimalDigit :: Char -> Bool
isDecimalDigit c = '0' <= c && c <= '9'

-- | Előjel-e?
isSign :: Char -> Bool
isSign c = c `elem` "+-"

-- | Szöveg egész számmá alakíthatósága
isInteger :: String -> Bool
isInteger [] = False
isInteger [x] = isDecimalDigit x
isInteger (x:xs) = isSign x || isDecimalDigit x && all isDecimalDigit xs

-- | Konvertálás egész számmá
strToInteger :: String -> Integer
strToInteger ('+':xs) = read xs :: Integer
strToInteger xs = read xs :: Integer

-- | A verem definíciója
type Stack a = [a]

-- | Érték lerakása a verembe
push :: Stack a -> a -> Stack a
push s a = a:s

-- | Érték kivétele a veremből
pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x, xs)

-- | A legutolsó két érték kivétele a veremből
pop2 :: Stack a -> (a, a, Stack a)
pop2 (x1:x2:xs) = (x1, x2, xs)

-- | A műveletek típusa:
-- A típusok egyszerűsítése végett megadjuk azokat a típusokat, amelyek egy
-- egyparaméteres (IFun1) és egy kétparaméteres (IFun2), egész számokkal dolgozó
-- függvényeket írnak le, amelyekhez hozzárendeltük a nevüket.

type IFun1 = (String, Integer -> Integer)
type IFun2 = (String, Integer -> Integer -> Integer)

-- | Keresés a függvények közt
findFunction :: [(String,f)] -> String -> Maybe f
findFunction [] _ = Nothing
findFunction ((s, f):xs) key
  | s == key = Just f
  | otherwise = findFunction xs key

-- | Elem feldolgozása verem használatával
step :: ([IFun1],[IFun2]) -> Stack Integer -> String -> Stack Integer
step (fs1, fs2) stack str
  | isInteger str = push stack num
  | otherwise = case (find1, find2) of -- talált művelet elvégzése
    (Just f , _) -> solve1 f stack
    (_, Just f ) -> solve2 f stack
    _ -> error "unknown operator"
  where
    num = strToInteger str -- sztring számmá alakítása
    find1 = findFunction fs1 str -- egyelemű függvények keresése
    find2 = findFunction fs2 str -- kételemű függvények keresése
    err = error "not enough parameters" -- közös hibaüzenet

    -- | Egy értéket tartalmazó műveletek elvégzése, majd elhelyezése a veremben
    solve1 :: (Integer -> Integer) -> Stack Integer -> Stack Integer
    solve1 f [] = err
    solve1 f stack = push xs $ f x -- eredmény visszatevése a verembe
      where (x, xs) = pop stack -- egy elem kivétele a veremből

    -- | Két értéket tartalmazó műveletek elvégzése, majd elhelyezése a veremben
    solve2 :: (Integer -> Integer -> Integer) -> Stack Integer -> Stack Integer
    solve2 f [] = err
    solve2 f [_] = err
    solve2 f stack =  push xs $ x2 `f` x1 -- eredmény visszatevése a verembe
      where (x1, x2, xs) = pop2 stack -- egy elem kivétele a veremből

-- | Postfix kifejezés kiértékelése adott műveletekkel
eval :: ([IFun1],[IFun2]) -> String -> Integer
eval fs s = eval' fs (words s) []
  where
    -- | Segédfüggvény
    eval' :: ([IFun1],[IFun2]) -> [String] -> Stack Integer -> Integer
    eval' fs (x:xs) stack
      | null xs = top
      | otherwise = eval' fs xs stack'
      where
        top = fst $ pop stack'
        stack' = step fs stack x

-- | Számológép szabványos műveletekkel
rpn :: String -> Integer
rpn = eval (xs1, xs2)
  where -- felhasználható műveletek (xs1 - egyelemű, xs2 - kételemű)
    xs1 = [("abs", abs), ("inc", (+ 1)), ("dec", \x -> x - 1)]
    xs2 = [("+", (+)), ("*", (*)), ("-", (-)), ("/", div), ("%", mod)]
