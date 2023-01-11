import Data.List.Split ( splitOn )
import qualified Data.Set as S
import Data.Foldable
import Data.Tuple (swap)
import Algorithm.Search
import Control.Monad.State
import Data.Ord (comparing)
import Data.List (maximumBy)

type Connector = (Int, Int)
type Bridge = [Connector]
type Partial = (Bridge, S.Set Connector)

neighbours :: Partial -> [Partial]
neighbours (partial, available) = straight ++ reversed
    where straight = [(component:partial, S.delete component available) | component <- toList $ S.filter ((==bridgeHead).fst) available]
          reversed = [(swap component:partial, S.delete component available) | component@(a,b) <- toList $ S.filter ((==bridgeHead).snd) available, a/=b]
          bridgeHead = if null partial then 0 else snd $ head partial

score :: Partial -> Int
score (bridge, _) = sum $ map (uncurry (+)) bridge

neighboursM :: Partial -> State Partial [Partial]
neighboursM partial = do
    let next = neighbours partial
    winner <- get
    put $ maximumBy (comparing score) (winner:next)
    return next

part1 :: [Connector] -> Int
part1 connectors = score $ snd result
    where search = dfsM neighboursM (const $ return False) initial
          result = runState search initial
          initial = ([], S.fromList connectors)

toPair :: [Int] -> (Int, Int)
toPair [x,y] = (x,y)

main = do
    connectors <- map (toPair . map read . splitOn "/") . lines <$> readFile "day24.txt"
    print $ part1 connectors
