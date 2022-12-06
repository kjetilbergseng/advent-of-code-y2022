module Day6(day6) where

solveDay6 :: Eq a => Int -> Int -> [a] -> Int
solveDay6 counter size li
    | allUnique (take size li) = counter + size
    | otherwise = solveDay6 (counter+1) size (tail li)

allUnique :: Eq a => [a] -> Bool
allUnique (x:xs)
    | x `elem` xs = False
    | otherwise = allUnique xs
allUnique []=True  


day6 :: IO ()
day6 = do
  putStrLn "day6"
  contents <- readFile "input/day6.txt"
  print $ solveDay6 0 4 contents
  print $ solveDay6 0 14 contents