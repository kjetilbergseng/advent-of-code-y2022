module Day2(day2) where
import UtilityFunctions(rotate, pairMap)

prepareInput :: [[Char]] -> [([Char], [Char])]
prepareInput=map (span (/= ' '))

data Choise = Rock | Paper | Scissors deriving (Eq)   

getResultsScore :: (Choise, Choise) -> [Choise] -> Integer
getResultsScore (x,y) choises
  | x==y = 3
  | x==head choises && y == choises !! 1 = 6
  | y==head choises && x == choises !! 1 = 0 
  | otherwise = getResultsScore (x,y) (rotate 1 choises)

getChoiseScore :: (Choise, Choise) -> Integer
getChoiseScore (_,Rock) = 1
getChoiseScore (_,Paper) = 2
getChoiseScore (_,Scissors) = 3

toChoiseType :: Char -> Choise
toChoiseType 'A'= Rock
toChoiseType 'B'= Paper
toChoiseType 'X'= Rock
toChoiseType 'Y'= Paper
toChoiseType _ = Scissors

getScore :: (Choise, Choise) -> Integer
getScore c = getResultsScore c [Rock,Paper,Scissors] + getChoiseScore c

findPartBChoise ::  [Choise] -> (Char, Char) -> (Choise, Choise)
findPartBChoise li (first,second)
  | toChoiseType first == head li = findPartBChoiseHelper (toChoiseType first,second) li
  | otherwise = findPartBChoise (rotate 1 li) (first,second)

findPartBChoiseHelper :: (Choise, Char) -> [Choise] -> (Choise, Choise)
findPartBChoiseHelper (first,'X') li = (first, last li)
findPartBChoiseHelper (first,'Y') _ = (first, first)
findPartBChoiseHelper (first,_) li = (first, li!!1)

day2a :: [([Char], [Char])] -> Integer
day2a = sum . map (getScore . pairMap (toChoiseType . last))

day2b :: [([Char], [Char])] -> Integer
day2b = sum . map (getScore . findPartBChoise [Rock,Paper, Scissors] . pairMap last)

day2::IO()
day2 = do
  putStrLn "day2"
  contents <- readFile "input/day2.txt"

  let input =  prepareInput $ lines contents
  print $ day2a input
  print $ day2b input