module Day3(day3) where
import Data.List(intersect)
import UtilityFunctions(chunk)
import Data.Char(ord, isUpper)

halfLength :: [Char] -> Int
halfLength li= div (length li) 2

splitInTwo :: [Char] -> ([Char], [Char])
splitInTwo li = splitAt (halfLength li) li

findCommon :: ([Char], [Char]) -> Char
findCommon (a,b)=  head (a `intersect` b)

charToNumber :: Char -> Int
charToNumber c 
  | isUpper c = ord c - 38
  | otherwise = ord c - 96

day3a :: [[Char]] -> Int
day3a li = sum $ map (charToNumber . findCommon . splitInTwo) li

inAll :: [[Char]] -> [Char]
inAll [] = ""
inAll [x] = x
inAll (x:xs) =  x `intersect` inAll xs

day3b :: [[Char]] -> Int
day3b li = sum $ map (charToNumber . head . inAll) (chunk 3 li)

day3::IO()
day3 = do
  putStrLn "day3"
  contents <- readFile "input/day3.txt"
  let input = lines contents
  print $ day3a input
  print $ day3b input