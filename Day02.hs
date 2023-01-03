import Data.List

part1 :: [[Int]] -> Int
part1 = sum . map check
    where check list = maximum list - minimum list

part2 :: [[Int]] -> Int
part2 = sum . map check
    where check list = head [quot | a <- list, b <- list \\ [a], a `mod` b == 0, let quot = a `div` b]

main = do
    rows <- map (map read . words) . lines <$> readFile "day02.txt"
    print $ part1 rows
    print $ part2 rows