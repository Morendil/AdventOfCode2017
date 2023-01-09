import Data.Char (isNumber)
import Data.List ( elemIndex, sortBy, sort, groupBy, nub, transpose )
import Data.List.Extra (minimumBy)
import Data.Ord (comparing)
import Data.List (intersect)
import Data.Maybe (fromJust)

type Particle = [[Int]]

parse :: String -> Particle
parse = map (map read . words . clean) . words
    where clean = map unCommas . filter (\c -> isNumber c || c == '-' || c == ',')
          unCommas c = if c==',' then ' ' else c

accelMagnitude :: Particle -> Int
accelMagnitude [_,_,[ax,ay,az]] = abs ax + abs ay + abs az
accelMagnitude _ = error "Strange particle"

velMagnitude :: Particle -> Int
velMagnitude [_,[vx,vy,vz],_] = abs vx + abs vy + abs vz
velMagnitude _ = error "Strange particle"

diff :: Particle -> Particle -> Particle
diff = zipWith (zipWith (-))

solve :: [Int] -> [Int]
solve [pos, vel, acc]
  | vel == 0 && pos == 0 = [0]
  | disc >= 0 && isNat root = map floor $ filter isNat solutions
  | otherwise = []
  where
      a = acc
      b = 2 * vel + acc
      c = 2 * pos
      disc = (b ^ 2) - (4 * a * c)
      root :: Float
      root = sqrt $ fromIntegral disc
      isNat x = x == fromInteger (floor x) && x >= 0
      solutions :: [Float]
      solutions = if abs a > 0 then roots else [fromIntegral (- pos) / fromIntegral vel]
      roots
        = [fromIntegral (- b + floor root) / (2 * fromIntegral a),
           fromIntegral (- b - floor root) / (2 * fromIntegral a)]

part1 :: [Particle] -> Int
part1 particles = fromJust $ elemIndex (minimumBy (comparing accelMagnitude `andThen` comparing velMagnitude) particles) particles
    where andThen=mappend

part2 :: [Particle] -> Int
part2 particles = length particles - exploded particles
    where exploded = length . nub . concat . collisions
          collisions particles = [[a,b] | a <- [0..length particles-1], b <- [a+1..length particles-1], let hits=collision particles a b, common hits]
          collision particles a b = map solve $ transpose $ diff (particles !! a) (particles !! b)
          common = not . null . foldr1 intersect

main = do
    particles <- map parse . lines <$> readFile "day20.txt"
    print $ part1 particles
    print $ part2 particles
