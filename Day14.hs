import Day10 (knotHash)
import Numeric (readHex)
import Data.Bits (popCount, Bits (testBit))
import Data.Graph (components, graphFromEdges)
import Data.Sequence (fromList, (!?))
import Data.Maybe (fromJust)

part1 :: [Integer] -> Int
part1 = sum . map popCount

part2  :: [Integer] -> Int
part2 hashes = length $ components graph
    where (graph, nodeFromVertex, vertexFromKey) = graphFromEdges relations
          relations = [((x,y),(x,y),set) | x <- [0..127], y <- [0..127], isSet (x,y), let set=filter isSet (neighbours (x,y))]
          neighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
          isSet (x,y) | x < 0 || x > 127 || y < 0 || y > 127 = False
          isSet (x,y) = testBit (fromJust $ hashSeq !? y) x
          hashSeq = fromList hashes

hashes :: String -> [Integer]
hashes key = map (fst . head . (readHex :: String -> [(Integer,String)]) . knotHash) keys
    where keys = map rowKey [0..127]
          rowKey n = key ++ "-" ++ show n

main = do
    keyString <- readFile "day14.txt"
    let keyHashes = hashes keyString
    print $ part1 keyHashes
    print $ part2 keyHashes