{- MARKUP ÉRTELMEZŐ -}
{-
   Ez a modul egy markup értelmező/elemző függvény implementációját tartalmazza.
   A függvény a megadott címkék és entitások alapján kigenerál egy szöveget.
-}

module MarkupParser
( toUpperMap
, toLowerMap
, capitalize
, capitalizeAll
, apart
, spacing
, paragraphize
, hr
, hlen
, h1
, takeValue
, takeTag
, parseTag
, editTags
, applyTags
, parseEntity
, applyEntities
, parse
, markup
, parse'
) where
import Data.Char
import Data.List
import Data.List.Split
import StackCalculator

-- | Szöveg nagybetűssé alakítása
toUpperMap :: String -> String
toUpperMap = map toUpper

-- | Szöveg kisbetűssé alakítása
toLowerMap :: String -> String
toLowerMap = map toLower

-- | Szó nagykezdőbetűsítése
capitalize :: String -> String
capitalize (x:xs) = toUpper x : toLowerMap xs

-- | Szavak nagykezdőbetűsítése egy szövegben a fehérkarakterek elvesztése nélkül
capitalizeAll :: String -> String
capitalizeAll = concatMap capitalize . groupBy (\a b -> isSpace a == isSpace b)

-- | Karakterek szétválasztása szóközzel
apart :: String -> String
apart [] = []
-- apart [x] = [x]
apart (x:xs)
  | x /= '\n' = x : ' ' : apart xs
  | otherwise = x : apart xs

-- | Szavak között egy szóköz hagyása
spacing :: String -> String
spacing = unwords . words

-- | Paragrafusok HTML szerű generálása
paragraphize :: String -> String
paragraphize s = '\n' : spacing s ++ "\n"

-- | Horizontális vonal, egy adott sztring ismétlése a sor végéig
hr :: Int -> String -> String
hr n s = take n $ cycle s

hlen :: Int -> String -> String -> Int
hlen n pad s = (n - (length s + 2) - 2 * length pad) `div` 2

-- | Fejléc, nagykezdőbetűsítés és sor kitöltése két felől
h1 :: Int -> String -> String
h1 n s = paragraphize $ unwords [ln, toUpperMap s, ln]
  where
    pad = "*" -- kitöltő szöveg
    len = hlen n pad s -- kitöltő szöveg hossza
    ln = pad ++ hr len pad -- egy kitöltött oldal (a pad legalább egyszer megjelenik)

h2 :: Int -> String -> String
h2 n s = paragraphize $ unwords [ln, capitalizeAll s, ln]
  where
    pad = "~" -- kitöltő szöveg
    len = hlen n pad s `div` 2 -- kitöltő szöveg hossza
    ln = pad ++ hr len pad -- egy kitöltött oldal (a pad legalább egyszer megjelenik)

-- | Sztring-függvény pár,
-- a címke jelöl egy függvényt, ami sztringen dolgozik
type TagFun1 = (String, String -> String)
type TagFun2 = (String, Int -> String -> String)

-- | Markup entity,
-- egy címkén belül tudunk a segítségével tiltott karaktereket használni
-- (&, <, >)
type Entity = (String, String)

-- kezdő és befejező karakterek cimkékhez és entitásokhoz
tagStart :: Char
tagStart = '<'

tagEnd :: Char
tagEnd = '>'

entityStart :: Char
entityStart = '&'

entityEnd :: Char
entityEnd = ';'

-- | Felbontja a szöveget az első c karakter mentén, megtartva a c-t
takeValue :: Char -> String -> (String, String)
takeValue c s = (value, rest)
  where
    value = takeWhile (/= c) s
    rest = dropWhile (/= c) s

-- | Felbontja a szöveget az első c karakter mentén, eldobva az első karaktert
takeTag :: Char -> String -> (String, String)
takeTag c s = (tag, rest)
  where
    tag = tail $ takeWhile (/= c) s
    rest = tail $ dropWhile (/= c) s

-- | Az első címke előtti szöveg, a címke és a címke utáni szöveg meghatározása
parseTag :: String -> (String, String, String)
parseTag s = (value, tag, rest')
  where
    (value, rest) = takeValue tagStart s
    (tag, rest') = if null rest then ("", "") else takeTag tagEnd rest

-- | Címke hozzáadása vagy törlése a kezdeti '/' karakter meglététől függően
editTags :: String -> [String] -> [String]
editTags "" = id
editTags xxs@(x:xs) = if x /= '/' then (xxs:) else delete xs

-- | Címke alkalmazása adott szövegre
applyTags :: [TagFun1] -> [TagFun2] -> [String] -> Int -> String -> String
applyTags _ _ [] _ s = s
applyTags fs1 fs2 (x:xs) len s = case (findFunction fs1 x, findFunction fs2 x) of
  (Just f, _) -> applyTags fs1 fs2 xs len $ f s
  (_, Just f) -> applyTags fs1 fs2 xs len $ len `f` s
  _ -> error ("unknown tag: " ++ x)

-- | Az első entitás előtti szöveg, az entitás és az entitás utáni szöveg meghatározása
parseEntity :: String -> (String, String, String)
parseEntity s = (begin, entity, end')
  where
    (begin, end) = takeValue entityStart s
    (entity, end') = if null end then ("", "") else takeTag entityEnd end

-- | Entitások kicserélése a nekik megfelelő szövegre
applyEntities :: [Entity] -> String -> String
applyEntities _ "" = ""
applyEntities ents s
  | null entity = s
  | otherwise = case findFunction ents entity of
    Just ent -> begin ++ ent ++ applyEntities ents end
    _ -> error ("unknown entity: " ++ entity)
    where
      (begin, entity, end) = parseEntity s

-- | Markup szöveg értelmezése a megadott címkék és entitások alapján.
-- Visszatérül a generált szöveg és a be nem zárt címkék tömbje
parse :: [TagFun1] -> [TagFun2] -> [Entity] -> [String] -> Int -> String -> (String, [String])
parse _ _ _ tags _ "" = ("", tags)
parse fs1 fs2 ents tags len s = (value'' ++ resS, resTags)
  where
    (value, tag, rest) = parseTag s
    tags' = editTags tag tags
    value' = applyEntities ents value
    value'' = applyTags fs1 fs2 tags len value'
    (resS, resTags) = parse fs1 fs2 ents tags' len rest

-- | Markup szöveg értelmezése a megadott címkék és entitások alapján (segédfüggvény)
parse' :: Int -> String -> (String, [String])
parse' = parse fs1 fs2 ents []
  where
    fs1 = [
      ("up", toUpperMap), ("low", toLowerMap), ("cap", capitalizeAll),
      ("apart", apart), ("p", paragraphize), ("rpn", show . rpn),
      ("rev", reverse), ("--", const "")
      ]
    fs2 = [("hr", hr), ("h1", h1), ("h2", h2)]
    ents = [("amp", "&"), ("lt", "<"), ("gt", ">")]

-- | Két elemű lista konvertálása tuple-lé
tuplify :: [a] -> (a, a)
tuplify [x] = (x, x)
tuplify [x, y] = (x, y)

-- | Címke attribútumainak lekérése
parseAttrs :: String -> [(String, String)]
parseAttrs tag = map (tuplify . wordsBy (== '=')) (words tag)

-- | Előfeldolgozás, ha 'format' címkével kezdődik a szöveg,
-- átállítjuk a sorhosszt a megadott értékre
preParse :: String -> (Maybe Int, String)
preParse "" = (Nothing, "")
preParse s@(x:xs)
  | x /= tagStart, a /= "format" = (Nothing, s)
  | otherwise = (len, rest)
  where
    (tag, rest) = takeTag tagEnd s
    ((a, _):as) = parseAttrs tag
    (attr, value) = head as
    len = if attr == "length"
      then Just . fromIntegral $ strToInteger value
      else Nothing

-- | Főfüggvény, meghívja az előfeldolgozást és a teljes feldolgozást is
markup :: Int -> String -> (String, [String], Int)
markup def s = (parsed, tags, len')
  where
    (len, s') = preParse s
    len' = case len of
      Just n -> n
      _ -> def
    (parsed, tags) = parse' len' s'

-- | Szöveg felbontása címkékre és értékekre egy listában (nem használt)
parseList :: [(String, Bool)] -> String -> [(String, Bool)]
parseList pairs "" = pairs
parseList pairs s = parseList pairs' rest
  where
    (value, tag, rest) = parseTag s
    valPair = (value, False)
    tagPair = (tag, True)
    pairs' = case (value, tag) of
      ([], _) -> tagPair:pairs
      (_, []) -> valPair:pairs
      (_, _) -> tagPair:valPair:pairs
