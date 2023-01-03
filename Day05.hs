import qualified Data.IntMap as M
import Data.Maybe ( fromJust )

part1 :: [Int] -> Int
part1 offsets = length $ takeWhile ((>)n . fst) $ iterate step (0, mapped)
    where n = length offsets
          mapped = M.fromList $ zip [0..] offsets

step :: (Int, M.IntMap Int) -> (Int, M.IntMap Int)
step (cur, offsets) = (next, M.adjust succ cur offsets)
    where next = cur + fromJust (M.lookup cur offsets)

main = do
    offsets <- map read . lines <$> readFile "day05.txt"
    print $ part1 offsets