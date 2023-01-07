import Data.Sequence (Seq, elemIndexL, adjust', fromList, (!?))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Foldable (toList)

type DanceLine = (Seq Char, Int)

move :: DanceLine -> String -> DanceLine
move (line, pos) ('s':spin) = (line, (pos-read spin) `mod` length line)
move (line, pos) ('x':rest) = (adjust' (const atTwo) one $ adjust' (const atOne) two line, pos)
    where (one:two:_) = map ((`mod` length line) . (+)pos . read) $ splitOn "/" rest
          atOne = fromJust (line !? one)
          atTwo = fromJust (line !? two)
move (line, pos) ('p':rest) = (adjust' (const two) whereOne $ adjust' (const one) whereTwo line, pos)
    where (one:two:_) = map head $ splitOn "/" rest
          whereOne = fromJust $ elemIndexL one line
          whereTwo = fromJust $ elemIndexL two line

final :: DanceLine -> String
final (line, pos) = take (length line) $ drop pos $ cycle (toList line)

part1 :: DanceLine -> [String] -> String
part1 line moves = final $ foldl move line moves

main = do
    -- let (file, size) = ("day16_sample.txt", 5)
    let (file, size) = ("day16.txt", 16)
        initial = (fromList $ take size ['a'..], 0)
    moves <- splitOn "," <$> readFile file
    print $ part1 initial moves