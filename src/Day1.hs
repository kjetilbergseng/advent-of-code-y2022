module Day1(day1) where
import UtilityFunctions(readInt, split)
import Data.List(sort)

prepareList :: [[String]] -> [Int]
prepareList li= reverse . sort $ map (sum . map readInt) li

day1a :: [[String]] -> Int
day1a = maximum . prepareList

day1b :: [[String]] -> Int
day1b li = sum $ take 3 (prepareList li)

day1::IO()
day1 = do
  putStrLn "day1"
  contents <- readFile "input/day1.txt"
  let input =  split "" . lines $ contents
  print $ day1a input
  print $ day1b input