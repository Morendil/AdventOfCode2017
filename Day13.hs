import Data.Char (isDigit, isSpace)
import Data.Maybe (fromJust, isJust)

type Firewall = [(Int, Int)]

toPair :: String -> (Int, Int)
toPair = pairUp . map read . words . filter (\c -> isDigit c || isSpace c)
    where pairUp (x:y:_) = (x,y)

severities :: Firewall -> [Int]
severities firewall = map severity [0..maxDepth]
    where maxDepth = maximum $ map fst firewall
          severity depth = if caught then depth * range else 0
            where scanner = lookup depth firewall
                  range = fromJust scanner
                  caught = isJust scanner && (depth `mod` ((2*range)-2)) == 0

part1 :: Firewall -> Int
part1 = sum . severities

main = do
    firewall <- map toPair . lines <$> readFile "day13.txt"
    print $ part1 firewall