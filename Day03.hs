import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List.HT (takeUntil)

-- | Day 03 part 1
--
-- >>> steps 1
-- 0
-- >>> steps 12
-- 3
-- >>> steps 23
-- 2
-- >>> steps 1024
-- 31

steps :: Int -> Int
steps = dist . (!!) coords . pred

coords :: [(Int, Int)]
coords = scanl add (0,0) $ concatMap shell [0..]

-- | Day 03 part 1
--
-- >>> shell 0
-- [(1,0)]
-- >>> shell 1
-- [(0,-1),(-1,0),(-1,0),(0,1),(0,1),(1,0),(1,0),(1,0)]

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
dist :: (Int, Int) -> Int
dist (x,y) = abs x + abs y

shell :: Int -> [(Int, Int)]
shell 0 = [(1,0)]
shell n = replicate ((n*2)-1) (0,-1) ++ replicate (n*2) (-1,0) ++ replicate (n*2) (0,1) ++ replicate ((n*2)+1) (1,0)

stress :: (Int, M.Map (Int, Int) Int) -> (Int, Int) -> (Int, M.Map(Int, Int) Int)
stress (cur, prev) here = (nearSum, M.insertWith (const id) here nearSum prev)
    where neighbours = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
          near pos = map (add pos) neighbours
          nearSum = sum $ mapMaybe (`M.lookup` prev) $ near here

part2 :: Int -> Int
part2 input = fst $ last $ takeUntil ((<) input . fst) $ scanl stress initial coords
    where initial = (0, M.insert (0,0) 1 M.empty)

part1 :: Int -> Int
part1 = steps

main = do
    input <- read <$> readFile "day03.txt"
    print $ part1 input
    print $ part2 input
