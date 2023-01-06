import Data.List.Split
import Data.Char (ord, chr)
import Data.Bits (Bits(xor))
import Text.Printf (printf)

type Knots = ([Int], (Int, Int))

part1 :: [Int] -> Int -> Int
part1 lengths size = product $ take 2 scrambled
    where scrambled = fst $ foldl (twist size) ([0..size-1], (0,0)) lengths

part2 :: String -> String
part2 input = concatMap (printf "%02x" . foldl1 xor) $ chunksOf 16 sparseHash
    where lengths = pre input ++ suffix
          sparseHash = sparse lengths

sparse :: [Int] -> [Int]
sparse lengths = fst $ last $ take 65 $ iterate knotHash initial
    where initial = ([0..255], (0,0))
          knotHash state = foldl (twist 256) state lengths

twist :: Int -> Knots -> Int -> Knots
twist size (list, (current, skipSize)) len = (offset, ((current+len+skipSize) `mod` size, skipSize+1))
    where toReverse = take len $ drop current $ cycle list
          spliced = reverse toReverse ++ take (size-len) (drop (current+len) (cycle list))
          offset = take size $ drop (size-current) $ cycle spliced

suffix :: [Int]
suffix = [17, 31, 73, 47, 23]

pre :: String -> [Int]
pre = map ord

main = do
    raw <- lines <$> readFile "day10.txt"
    let [[size], lengths] = map (map read . splitOn ",") raw
    print $ part1 lengths size
    print $ part2 (raw !! 1)