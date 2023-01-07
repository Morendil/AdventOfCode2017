import Data.Char (isNumber)
import Data.Bits ((.&.))

part1 :: (Int, Int) -> Int
part1 = go 40000001 0
    where go 0 accum prev = accum
          go n accum prev = go (n-1) (if agree prev then accum+1 else accum) (step prev)

step :: (Int, Int) -> (Int, Int)
step (a, b) = (a * 16807 `mod` 2147483647, b * 48271 `mod` 2147483647)

agree :: (Int, Int) -> Bool
agree (a,b) = a .&. 65535 == b .&. 65535

main = do
    (genA:genB:_) <- map (read . filter isNumber) . lines <$> readFile "day15.txt"
    print $ part1 (genA,genB)