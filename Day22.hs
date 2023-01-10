import qualified Data.Set as S
import Data.Bifunctor (first)
import Data.Maybe (fromJust)

type Point = (Int, Int)
type Dir = Int
type Carrier = (Point, Dir)
type Cluster = S.Set Point
type State = (Cluster, Carrier, Int)

dirs :: [Point]
dirs = [(0,-1),(1,0),(0,1),(-1,0)]
add :: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
step :: Carrier -> Carrier
step (pos, dir) = (add pos (dirs !! dir), dir)

scan :: [String] -> Cluster
scan lines = S.fromList [(x,y) | x <- [0..length (head lines)-1], y <- [0..length lines-1], let cell = (lines !! y) !! x, cell == '#']

display :: State -> [String]
display (cluster,(pos,dir),_) = [[if (x,y) == pos then arrow else if S.member (x,y) cluster then '#' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]]
    where minX = minimum $ S.map fst cluster
          maxX = maximum $ S.map fst cluster
          minY = minimum $ S.map snd cluster
          maxY = maximum $ S.map snd cluster
          arrow = fromJust $ lookup dir [(0,'^'),(1,'>'),(2,'v'),(3,'<')]

burst :: State -> State
burst (cluster, (pos, dir), infections) = (cluster', step turned, infections')
    where turned = if infected then turnRight else turnLeft
          infected = S.member pos cluster
          turnRight = (pos, (dir+1)`mod`4)
          turnLeft = (pos, (dir-1)`mod`4)
          cluster' = if infected then S.delete pos cluster else S.insert pos cluster
          infections' = if not infected then infections + 1 else infections

part1 :: [String] -> Int
part1 grid = (\(_,_,infections) -> infections) $ last states
    where cluster = scan grid
          start = (length (head grid) `div` 2, length grid `div` 2)
          states = take 10001 $ iterate burst (cluster, (start,0), 0)

main = do
    grid <- lines <$> readFile "day22.txt"
    print $ part1 grid
