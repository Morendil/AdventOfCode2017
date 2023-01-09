import qualified Data.Map as M
import Data.List
import Data.List.HT
import Data.Char (isAlpha)

type Point = (Int, Int)
type Direction = Point
type Packet = (Point, Direction)
type Network = M.Map Point Char

neighbours :: [Direction]
neighbours = [(-1,0),(1,0),(0,-1),(0,1)]

anti :: Direction -> Direction
anti (x,y) = (-x,-y)

start :: Network -> Point
start = head . filter (\(x,y) -> y == 0) . M.keys

collect :: Network -> Packet -> String
collect network = go "" . move network
    where cell (pos,_) = M.findWithDefault ' ' pos network 
          go acc packet | cell packet == ' ' = acc
          go acc packet | cell packet == '+' = go acc (move network $ turn network packet)
          go acc packet | isAlpha (cell packet) = go (acc++[cell packet]) (move network packet)
          go _ packet = error $ "Packet stopped at unexpected point, " ++ show packet

turn :: Network -> Packet -> Packet
turn network (pos,dir) = (pos, head (directions \\ [anti dir]))
    where directions = filter (\offset -> M.findWithDefault ' ' (add pos offset) network /= ' ') neighbours

move :: Network -> Packet -> Packet
move network packet = last $ takeUntil atStop $ drop 1 $ iterate step packet
    where atStop p = cell p == '+' || cell p == ' ' || isAlpha (cell p)
          cell (pos,_) = M.findWithDefault ' ' pos network

step :: Packet -> Packet
step (pos,dir) = (add dir pos, dir)

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

scan :: [String] -> Network
scan lines = M.fromList [((x,y), cell) | x <- [0..length (head lines)-1], y <- [0..length lines-1], let cell = (lines !! y) !! x, cell /= ' ']

part1 :: Network -> String
part1 network = collect network (start network,(0,1))

main = do
    diagram <- scan . lines <$> readFile "day19.txt"
    print $ part1 diagram