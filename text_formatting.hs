{- Szövegformázás -}
-- Mondatjel felismerése (1 pont)
isEndMark :: Char -> Bool
isEndMark c = c `elem` ".?!"

-- Szavak összefűzése (2 pont)
unwords' :: [String] -> String
unwords' [] = []
unwords' [x] = x
unwords' (x:xs)
  | isEndMark $ last x = x ++ "  " ++ unwords' xs
  | otherwise = x ++ " " ++ unwords' xs

-- Távolság (1 pont)
d :: (Ord a, Num a) => a -> a -> a
d x y
  | x < y = 5 * (y - x)
  | otherwise = x - y

-- Egy sorba beférő szavak (4 pont)
fit :: (Int -> Int -> Int) -> Int -> [String] -> [String]
fit f n xxs@(x:_) =
  let fit' :: (Int -> Int -> Int) -> Int -> [String] -> [String]
      fit' _ _ [] = []
      fit' f n (x:xs)
        | n < n' = []
        | otherwise = x : fit' f n'' xs
        where
          sep = if isEndMark $ last x then 2 else 1
          n' = n `f` length x
          n'' = n' - sep
      res = fit' f n xxs
  in if null res then [x] else res

-- A szöveg feldarabolása (3 pont)
cut :: (Int -> Int -> Int) -> Int -> [String] -> [[String]]
cut f n [] = []
cut f n xs =
  let x = fit f n xs
      len = length x
      xs' = drop len xs
  in x : cut f n xs'

-- -- Formázás (1 pont)
format :: Int -> String -> String
format n s =
  let format' :: [[String]] -> String
      format' [] = ""
      format' (x:xs) = unwords' x ++ "\n" ++ format' xs
      xs = cut d n $ words s
  in format' xs