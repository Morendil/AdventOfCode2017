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

main = do
    print $ part1 376