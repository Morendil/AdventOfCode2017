import Data.List

part1 :: [[String]] -> Int
part1 = length . filter valid
    where valid words = words == nub words

part2 :: [[String]] -> Int
part2 = length . filter valid
    where valid words = (map sort words) == (nub $ map sort words)

main = do
    phrases <- map words . lines <$> readFile "day04.txt"
    print $ part1 phrases
    print $ part2 phrases    