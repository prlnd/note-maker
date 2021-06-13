{- Szövegformázás -}
{-
   Ez a modul egy egyszerű szövegformázó függvényt tartalmaz, amely egyébként a
   UNIX fmt parancsának is az alapja. Ennek az a feladata, hogy a kapott
   szöveget az adott hosszúságú sorokra tördelje, valamint a mondatvégi
   írásjelek után két szóközzel válassza el egymástól a mondatokat.
   (Ez a konvenciót az Emacs szövegszerkesztő is alkalmazza, így ismeri fel a
   mondatokat a szövegben.)
-}
module TextFormatter
( isEndMark
, unwords'
, d
, fit
, cut
, format
) where

import Data.List.Split
import Data.Char

-- | Mondatjel felismerése
isEndMark :: Char -> Bool
isEndMark c = c `elem` ".?!"

-- | Szavak összefűzése, írásjelek esetén két szóközzel, máskép eggyel
unwords' :: [String] -> String
unwords' [] = []
unwords' [x] = x
unwords' (x:xs) = x ++ sep ++ unwords' xs
  where
    sep = if isEndMark $ last x then "  " else " "

-- | Két szám távolsága
d :: (Ord a, Num a) => a -> a -> a
d x y
  | x < y = 5 * (y - x)
  | otherwise = x - y

-- | Egy sorba beférő szavak,
-- ha csak egy szóból áll és az meghaladja a határt, nem lehet tovább bontani
fit :: (Int -> Int -> Int) -> Int -> [String] -> [String]
fit f n xxs@(x:_) = if null res then [x] else res
  where
    fit' :: (Int -> Int -> Int) -> Int -> [String] -> [String]
    fit' _ _ [] = []
    fit' f n (x:xs)
      | last x == '\n' = [x]
      | n < n' = []
      | otherwise = x : fit' f n'' xs
      where
        sep = if isEndMark $ last x then 2 else 1
        n' = n `f` length x
        n'' = n' - sep
    res = fit' f n xxs

-- | A szöveg feldarabolása
cut :: (Int -> Int -> Int) -> Int -> [String] -> [[String]]
cut f n [] = []
cut f n xs = x : cut f n xs'
  where
    x = fit f n xs
    len = length x
    xs' = drop len xs

-- | Formázás
format :: Int -> String -> String
format n s = format' xs
  where
    format' :: [[String]] -> String
    format' [] = ""
    format' (x:xs) = unwords' x ++ (if last x' == '\n' then "" else "\n") ++ format' xs
      where
        x' = unwords' x
    xs = cut d n $ wordsBy (\x -> isSpace x && (x /= '\n')) s
