module Day4(day4) where
import UtilityFunctions(splitPair, pairMap, readInt)
import Data.List(intersect)

prepareInput :: [[Char]] -> [(([Char], [Char]), ([Char], [Char]))]
prepareInput = map (pairMap (splitPair '-') . splitPair ',') 

inputToInt :: ((String, String), (String, String)) -> ((Int, Int), (Int, Int))
inputToInt = pairMap (pairMap readInt)

fullyOverlaps :: (Ord a1, Ord a2, Num a3) => ((a1, a2), (a1, a2)) -> a3
fullyOverlaps ((a,b),(x,y))
    | a>=x && b<=y = 1
    | x>=a && y<=b = 1
    | otherwise = 0

overlaps :: (Eq a1, Enum a1, Num a2) => ((a1, a1), (a1, a1)) -> a2
overlaps ((a,b),(x,y)) 
    | null ([a..b] `intersect` [x..y]) = 0
    | otherwise = 1

solveDay4 :: (((Int, Int), (Int, Int)) -> Integer) -> [((String, String), (String, String))] -> Integer
solveDay4 f = sum . map (f . inputToInt)

day4::IO()
day4 = do
    putStrLn "day4"
    contents <- readFile "input/day4.txt"
    let input = lines contents
    print $ solveDay4 fullyOverlaps (prepareInput input)
    print $ solveDay4 overlaps (prepareInput input)