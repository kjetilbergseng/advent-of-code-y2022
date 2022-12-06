module Day5(day5) where
import UtilityFunctions(chunk, remove, split, readInt)
import Data.List ( transpose )

getCratesOnly :: [[Char]] -> [[[Char]]]
getCratesOnly li= map (remove [""]) (transpose $ takeWhile (\x -> head x /= ['1']) (map divideIntoCreates li))

divideIntoCreates :: [Char] -> [[Char]]
divideIntoCreates li= map ( remove " []") (chunk 4 (' ':li))

removeCreates :: [String] -> [String]
removeCreates=tail . dropWhile (/= "")

getInstructions :: [String] -> [[Int]]
getInstructions li= map (map readInt . remove [""] . split ' ' . remove "movefromto") (removeCreates li)

executeInstruction :: ([[Char]] -> [[Char]]) -> [[[Char]]] -> [Int] -> [[[Char]]]
executeInstruction fn creates [x,y,z] = dropCrates x y (prependCreates fn z (getCreatesToMove x y creates) creates)
executeInstruction _ _ _ = [[[' ']]]

getCreatesToMove :: Int -> Int -> [[a]] -> [a]
getCreatesToMove number pos creates = take number (creates !! (pos-1))

prependCreates :: (t -> [a]) -> Int -> t -> [[a]] -> [[a]]
prependCreates fn pos createsToPrepend creates = 
    take (pos-1) creates ++ [fn createsToPrepend ++ creates !! (pos-1)]  ++ drop pos creates

dropCrates :: Int -> Int -> [[a]] -> [[a]]
dropCrates number pos creates = 
    take (pos-1) creates ++ [drop number (creates !! (pos-1))]  ++ drop pos creates

solveDay5 :: ([[Char]] -> [[Char]]) -> [[[Char]]] -> [[Int]] -> [Char]
solveDay5 fn creates instructions = concatMap head (foldl (executeInstruction fn) creates instructions)

day5 :: IO ()
day5 = do
  putStrLn "day5"
  contents <- readFile "input/day5.txt"
  let creates = getCratesOnly (lines contents)
  let instructions = getInstructions (lines contents)
  print $ solveDay5 reverse creates instructions
  print $ solveDay5 id creates instructions
