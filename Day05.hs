import qualified Data.IntMap as M
import Data.Maybe ( fromJust )

part1 :: [Int] -> Int
part1 = solve succ

part2 :: [Int] -> Int
part2 = solve rule2
    where rule2 cur = if cur >= 3 then pred cur else succ cur

solve :: (Int -> Int) -> [Int] -> Int
solve fn offsets = length $ takeWhile ((>)n . fst) $ iterate (step fn) (0, mapped)
    where n = length offsets
          mapped = M.fromList $ zip [0..] offsets

step :: (Int -> Int) -> (Int, M.IntMap Int) -> (Int, M.IntMap Int)
step fn (cur, offsets) = (next, M.adjust fn cur offsets)
    where next = cur + fromJust (M.lookup cur offsets)

main = do
    offsets <- map read . lines <$> readFile "day05.txt"
    print $ part1 offsets
    print $ part2 offsets