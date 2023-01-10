import qualified Data.Map as M
import Data.Bifunctor (first)
import Data.Maybe (fromJust)

type Point = (Int, Int)
type Dir = Int
type Carrier = (Point, Dir)
type Cluster = M.Map Point Char
type State = (Cluster, Carrier, Int)

dirs :: [Point]
dirs = [(0,-1),(1,0),(0,1),(-1,0)]
add :: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
step :: Carrier -> Carrier
step (pos, dir) = (add pos (dirs !! dir), dir)

scan :: [String] -> Cluster
scan lines = M.fromList [((x,y),cell) | x <- [0..length (head lines)-1], y <- [0..length lines-1], let cell = (lines !! y) !! x, cell == '#']

display :: State -> [String]
display (cluster,(pos,dir),_) = [[if (x,y) == pos then arrow else M.findWithDefault '.' (x,y) cluster| x <- [minX..maxX]] | y <- [minY..maxY]]
    where minX = minimum $ map fst $ M.keys cluster
          maxX = maximum $ map fst $ M.keys cluster
          minY = minimum $ map snd $ M.keys cluster
          maxY = maximum $ map snd $ M.keys cluster
          arrow = fromJust $ lookup dir [(0,'^'),(1,'>'),(2,'v'),(3,'<')]

burst :: State -> State
burst (cluster, (pos, dir), infections) = (cluster', step turned, infections')
    where turned = if infected then turnRight else turnLeft
          infected = M.findWithDefault '.' pos cluster == '#'
          turnRight = (pos, (dir+1)`mod`4)
          turnLeft = (pos, (dir-1)`mod`4)
          cluster' = if infected then M.delete pos cluster else M.insert pos '#' cluster
          infections' = if not infected then infections + 1 else infections

burst2 :: State -> State
burst2 (cluster, carrier@(pos, dir), infections) = (cluster', step newDirection, infections')
    where newDirection
            | weakened = carrier
            | infected = turnRight
            | flagged = reverse
            | otherwise = turnLeft
          newState
            | flagged = '.'
            | infected = 'F'
            | weakened = '#'
            | otherwise = 'W'
          here = M.findWithDefault '.' pos cluster
          infected = here == '#'
          weakened = here == 'W'
          flagged = here == 'F'
          turnRight = (pos, (dir+1)`mod`4)
          turnLeft = (pos, (dir-1)`mod`4)
          reverse = (pos, (dir+2)`mod`4)
          cluster' = if flagged then M.delete pos cluster else M.insert pos newState cluster
          infections' = if newState == '#' then infections + 1 else infections

part1 :: [String] -> Int
part1 = solve burst 10000

part2 :: [String] -> Int
part2 = solve burst2 10000000

solve :: (State -> State) -> Int -> [String] -> Int
solve fn turns grid = (\(_,_,infections) -> infections) $ last states
    where cluster = scan grid
          start = (length (head grid) `div` 2, length grid `div` 2)
          states = take (turns+1) $ iterate fn (cluster, (start,0), 0)

main = do
    grid <- lines <$> readFile "day22.txt"
    print $ part1 grid
    print $ part2 grid
