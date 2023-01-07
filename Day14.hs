import Day10 (knotHash)
import Numeric (readHex)
import Data.Bits (popCount)

part1 :: String -> Int
part1 key = sum $ map (popCount . fst . head . (readHex :: String -> [(Integer,String)]) . knotHash) keys
    where keys = map rowKey [0..127]
          rowKey n = key ++ "-" ++ show n

main = do
    keyString <- readFile "day14.txt"
    print $ part1 keyString