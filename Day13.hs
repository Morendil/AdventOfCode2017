import Data.Char (isDigit, isSpace)
import Data.Maybe (fromJust, isJust)

type Firewall = [(Int, Int)]

toPair :: String -> (Int, Int)
toPair = pairUp . map read . words . filter (\c -> isDigit c || isSpace c)
    where pairUp (x:y:_) = (x,y)

severities :: Int -> Firewall -> [(Int, Int)]
severities delay firewall = map severity [0..maxDepth]
    where maxDepth = maximum $ map fst firewall
          severity depth = if caught then (depth, range) else (0,0)
            where scanner = lookup depth firewall
                  range = fromJust scanner
                  caught = isJust scanner && (time `mod` ((2*range)-2)) == 0
                  time = delay + depth

getCaught :: Int -> Firewall -> Bool
getCaught delay firewall = any ((0,0) /=) (severities delay firewall)

part1 :: Firewall -> Int
part1 = sum . map (uncurry (*)) . severities 0

part2 :: Firewall -> Int
part2 firewall = length $ takeWhile id $ map (`getCaught` firewall) [0..]

main = do
    firewall <- map toPair . lines <$> readFile "day13.txt"
    print $ part1 firewall
    print $ part2 firewall