import Data.List ( elemIndex )
import Data.Maybe ( fromJust )
import Data.Sequence (chunksOf, fromList)
import Data.Foldable (toList)

-- | Day 06 part 1
--
-- >>> step [0,2,7,0]
-- [2,4,1,2]

step :: [Int] -> [Int]
step banks = foldl (zipWith (+)) zeroOut (map (take (length banks) . (++ repeat 0)) added)
    where redist = fromJust $ elemIndex amount banks
          amount = maximum banks
          zeroOut = take redist banks ++ [0] ++ drop (redist+1) banks
          chunksOfL n = map toList . toList . chunksOf n . fromList
          ones = replicate amount 1
          added = chunksOfL (length banks) (replicate (redist+1) 0 ++ ones)

findCycle :: Eq a => [a] -> ([a],[a])
findCycle xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x == y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys

part1 :: [Int] -> Int
part1 banks = length (fst cycle) + length (snd cycle)
    where cycle = findCycle $ iterate step banks

part2 :: [Int] -> Int
part2 banks = length (snd cycle)
    where cycle = findCycle $ iterate step banks

main = do
    banks <- map read . words <$> readFile "day06.txt"
    print $ part1 banks
    print $ part2 banks