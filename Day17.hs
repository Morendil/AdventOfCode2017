import qualified Data.Sequence as S
import Data.Maybe (fromJust)

type Ring = (S.Seq Int, Int, Int)

insertion :: Int -> Ring -> Ring
insertion step (prev, pos, val) = (S.insertAt newPos val prev, newPos, val+1)
    where newPos = 1 + (pos+step) `mod` S.length prev

initial :: Ring
initial = (S.fromList [0], 0, 1)

part1 :: Int -> Int
part1 step = fromJust $ ring S.!? (pos + 1)
    where (ring, pos, val) = last $ take 2018 $ iterate (insertion step) initial

part2 :: Int -> Int -> Int
part2 count step = go count (0,1) 0
    where go 0 _ found = found
          go n (pos, val) lastFound = go (n-1) (newPos,val+1) maybeFound
                where newPos = 1 + (pos+step) `mod` val
                      maybeFound = if newPos == 1 then val else lastFound


main = do
    print $ part1 376
    print $ part2 50000000 376