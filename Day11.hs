import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Hex = (Int, Int, Int)

add :: Hex -> Hex -> Hex
add (q1,r1,s1) (q2,r2,s2) = (q1+q2,r1+r2,s1+s2)

offsets :: [(String, Hex)]
offsets = [("n",(0,-1,1)),("ne",(1,-1,0)),("se",(1,0,-1)),("s",(0,1,-1)),("sw",(-1,1,0)),("nw",(-1,0,1))]

move :: Hex -> String -> Hex
move from dir = add from $ fromJust $ lookup dir offsets

moveAlong :: Hex -> [String] -> Hex
moveAlong = foldl move

dist :: Hex -> Int
dist (q,r,s) = (abs q + abs r + abs s) `div` 2

main = do
    paths <- map (splitOn ",") . lines <$> readFile "day11.txt"
    print $ map (dist . moveAlong (0,0,0)) paths
    print $ map (maximum . map dist . scanl move (0,0,0)) paths