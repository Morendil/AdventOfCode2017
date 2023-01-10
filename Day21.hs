import Data.List.Split ( chunksOf, splitOn )
import Data.List ( transpose )
import Data.Maybe (mapMaybe)

type Rule = ([String],[String])

transforms :: [[[a]] -> [[a]]]
transforms = [id, map reverse, transpose, reverse . transpose, reverse, map reverse . reverse, map reverse . transpose, reverse . map reverse . transpose]

assemble :: [[String]] -> [String]
assemble flat = concatMap (foldr1 (zipWith (++))) $ chunksOf side flat
    where side = floor $ sqrt $ fromIntegral $ length flat

cutUp :: Int -> [String] -> [[String]]
cutUp n = concatMap (chunksOf n) . transpose . map (chunksOf n)

evolve :: [Rule] -> [String] -> [String]
evolve rules grid | even (length grid) = assemble $ map (match rules) $ cutUp 2 grid
evolve rules grid | length grid `mod` 3 == 0 = assemble $ map (match rules) $ cutUp 3 grid

match :: [Rule] -> [String] -> [String]
match rules square = head $ mapMaybe (\transform -> lookup (transform square) rules) transforms

glider :: [String]
glider = [".#.","..#","###"]

part1 :: [Rule] -> Int
part1 rules = length $ filter (=='#') $ concat $ last $ take 6 $ iterate (evolve rules) glider

main = do
    rules <- map ((\[x,y] -> (x,y)) . map (splitOn "/") . splitOn " => ") . lines <$> readFile "day21.txt"
    print $ part1 rules