import Data.Char (isNumber)
import Data.List ( elemIndex, sortBy )
import Data.List.Extra (minimumBy)
import Data.Ord (comparing)

parse :: String -> [[Int]]
parse = map (map read . words . clean) . words
    where clean = map unCommas . filter (\c -> isNumber c || c == '-' || c == ',')
          unCommas c = if c==',' then ' ' else c

accelMagnitude :: [[Int]] -> Int
accelMagnitude [_,_,[ax,ay,az]] = abs ax + abs ay + abs az
accelMagnitude _ = error "Strange particle"

velMagnitude :: [[Int]] -> Int
velMagnitude [_,[vx,vy,vz],_] = abs vx + abs vy + abs vz
velMagnitude _ = error "Strange particle"

main = do
    particles <- map parse . lines <$> readFile "day20.txt"
    let andThen=mappend
    print $ elemIndex (minimumBy (comparing accelMagnitude `andThen` comparing velMagnitude) particles) particles