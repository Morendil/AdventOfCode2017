import Data.Sequence (Seq, elemIndexL, adjust', fromList, (!?))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Data.List (nub, elemIndices, unfoldr)

type DanceLine = (Seq Char, Int)
data Dance = Dance { size::Int, spin::Int, exchanges::Seq Int, partners::Seq Char}
    deriving (Show, Eq)

norm :: Ord a => (a, a) -> (a, a)
norm (a,b) = if a > b then (b,a) else (a,b)

addMove :: Dance -> String -> Dance
addMove dance ('s':spinStr) = dance {spin=(spin dance + read spinStr)`mod` size dance}
addMove dance ('x':rest) = dance {exchanges=swapIdx (exchanges dance) (one,two)}
    where (one:two:_) = map ((`mod` size dance) . flip (-) (spin dance) . read) $ splitOn "/" rest
addMove dance ('p':rest) = dance {partners=swapVal (partners dance) (one,two)}
    where (one:two:_) = map head $ splitOn "/" rest

swapVal :: Eq a => Seq a -> (a,a) -> Seq a
swapVal values (one,two) = adjust' (const two) whereOne $ adjust' (const one) whereTwo values
    where whereOne = fromJust $ elemIndexL one values
          whereTwo = fromJust $ elemIndexL two values

swapIdx :: Seq a -> (Int,Int) -> Seq a
swapIdx values (one,two) = adjust' (const atTwo) one $ adjust' (const atOne) two values
    where atOne = fromJust (values !? one)
          atTwo = fromJust (values !? two)

combine :: Dance -> Dance -> Dance
combine dance1 dance2 = Dance {size=size dance1, spin=0, exchanges=exchanges', partners=partners'}
    where partners' = fromList $ map (fromJust . flip lookup (zip starting (toList (partners dance2)))) (toList $ partners dance1)
          exchanges' = fromList $ map (unspin2 !!) unspin1
          starting = take dsize ['a'..]
          spins = spin dance1 + spin dance2
          dsize = size dance1
          unspin1 = take dsize $ drop (dsize-spin dance1) $ cycle $ toList $ exchanges dance1
          unspin2 = take dsize $ drop (dsize-spin dance2) $ cycle $ toList $ exchanges dance2

nullDance :: Int -> Dance
nullDance size = Dance {size, spin=0, exchanges=fromList $ take size [0..], partners=fromList $ take size ['a'..]}

apply :: Dance -> String
apply Dance {size,spin,exchanges,partners} = take size $ drop (size-spin) $ cycle swappedTwice
    where swappedOnce = map (fromJust . flip lookup (zip starting (toList partners))) starting
          swappedTwice = map (swappedOnce !!) $ toList exchanges
          starting = take size ['a'..]

part1 :: Int -> [String] -> String
part1 size moves = apply $ foldl addMove (nullDance size) moves

part2 :: Int -> [String] -> String
part2 size moves = apply $ pow (foldl addMove (nullDance size) moves) 1000000000

pow :: Dance -> Int -> Dance
pow base n = foldr1 combine $ map (powers (len+1) base !!) $ elemIndices 1 binary
  where binary = toBinary n
        len = length binary

toBinary :: Int -> [Int]
toBinary = unfoldr toBinary'
  where toBinary' 0 = Nothing
        toBinary' n = Just (n `mod` 2, n `div` 2)

powers :: Int -> Dance -> [Dance]
powers n m = take n $ iterate (\m -> combine m m) m

main = do
    -- let (file, size) = ("day16_sample.txt", 5)
    let (file, size) = ("day16.txt", 16)
        initial = (fromList $ take size ['a'..], 0)
    moves <- splitOn "," <$> readFile file
    print $ part1 size moves
    print $ part2 size moves