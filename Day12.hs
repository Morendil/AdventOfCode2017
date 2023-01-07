import Data.Char (isDigit, isSpace)
import Data.Graph (graphFromEdges, reachable, components, Graph)

part1 :: [[Int]] -> Int
part1 = length . (`reachable` 0) . toGraph

part2 :: [[Int]] -> Int
part2 = length . components . toGraph

toGraph :: [[Int]] -> Graph
toGraph pipes = graph
    where (graph, nodeFromVertex, vertexFromKey) = graphFromEdges relations
          relations = map rel pipes
          rel (x:rest) = (x,x,rest)


main = do
    pipes <- map (map read . words . filter (\c -> isDigit c || isSpace c)) . lines <$> readFile "day12.txt"
    print $ part1 pipes
    print $ part2 pipes