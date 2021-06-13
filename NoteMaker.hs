module Main where
import MarkupParser
import TextFormatter

defLen :: Int
defLen = 80

addSpace :: String -> String
addSpace "" = ""
addSpace (x:xs)
  | x == '\n' = x : ' ' : addSpace xs
  | otherwise = x : addSpace xs

parseFile :: String -> String -> IO ()
parseFile ifname ofname = do
  s <- readFile ifname
  let (parsed, _, len) = markup defLen s
  let result = format len $ addSpace parsed
  writeFile ofname result

main :: IO ()
main = parseFile ".\\examples\\input.txt" ".\\examples\\output.txt"
