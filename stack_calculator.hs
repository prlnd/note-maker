{- VEREM SZÁMOLÓGÉP -}
import Data.Maybe

-- Decimális számjegyek vizsgálata (1 pont)
isDecimalDigit :: Char -> Bool
isDecimalDigit c = '0' <= c && c <= '9'

-- Előjel-e? (1 pont)
isSign :: Char -> Bool
isSign c = c `elem` "+-"

-- Szöveg egész számmá alakíthatósága (2 pont)
isInteger :: String -> Bool
isInteger [] = False
isInteger xxs@(x:xs)
  | isSign x && (not . null) xs = f xs
  | otherwise = f xxs
  where
    f = all isDecimalDigit

-- Konvertálás egész számmá (2 pont)
strToInteger :: String -> Integer
strToInteger xxs@(x:xs) =
  let xs' = if x == '+' then xs else xxs
  in read xs' :: Integer

-- A verem definíciója (1 pont)
type Stack a = [a]

-- Érték lerakása a verembe (1 pont)
push :: Stack a -> a -> Stack a
push s a = a:s

-- Érték kivétele a veremből (1 pont)
pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x, xs)

-- A legutolsó két érték kivétele a veremből (1 pont)
pop2 :: Stack a -> (a, a, Stack a)
pop2 (x1:x2:xs) = (x1, x2, xs)

-- A műveletek típusa
-- A típusok egyszerűsítése végett megadjuk azokat a típusokat, amelyek egy
-- egyparaméteres (IFun1) és egy kétparaméteres (IFun2), egész számokkal dolgozó
-- függvényeket írnak le, amelyekhez hozzárendeltük a nevüket.

type IFun1 = (String, (Integer -> Integer))
type IFun2 = (String, (Integer -> Integer -> Integer))

-- Keresés a függvények közt (2 pont)
findFunction :: [(String,f)] -> String -> Maybe f
findFunction [] _ = Nothing
findFunction ((s, f):xs) key
  | s == key = Just f
  | otherwise = findFunction xs key

-- Elem feldolgozása verem használatával (4 pont)
step :: ([IFun1],[IFun2]) -> Stack Integer -> String -> Stack Integer
step (fs1, fs2) stack str
  | isInteger str = push stack num
  | otherwise = case (find1, find2) of
    (Just f , _) -> eval1 f stack
    (_, Just f ) -> eval2 f stack
    _ -> error "unknown operator"
  where
    num = strToInteger str
    find1 = findFunction fs1 str
    find2 = findFunction fs2 str
    err = error "not enough parameters"

    eval1 :: (Integer -> Integer) -> Stack Integer -> Stack Integer
    eval1 f [] = err
    eval1 f stack =
      let (x, xs) = pop stack
          res = f x
      in push xs res
    
    eval2 :: (Integer -> Integer -> Integer) -> Stack Integer -> Stack Integer
    eval2 f [] = err
    eval2 f [x] = err
    eval2 f stack =
      let (x1, x2, xs) = pop2 stack
          res = x2 `f` x1
      in push xs res

-- Postfix kifejezés kiértékelése adott műveletekkel (3 pont)
evaluate :: ([IFun1],[IFun2]) -> String -> Integer
evaluate fs s =
  let evaluate' :: ([IFun1],[IFun2]) -> [String] -> Stack Integer -> Integer
      evaluate' fs (x:xs) stack
        | null xs = top
        | otherwise = evaluate' fs xs stack'
        where
          top = fst $ pop stack'
          stack' = step fs stack x  
  in evaluate' fs (words s) []

-- -- Számológép szabványos műveletekkel (1 pont)
rpn :: String -> Integer
rpn s =
  let xs1 = [("abs", abs), ("inc", (+ 1)), ("dec", \x -> x - 1)]
      xs2 = [("+", (+)), ("*", (*)), ("-", (-)), ("/", div), ("%", mod)]
  in evaluate (xs1, xs2) s
