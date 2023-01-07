import Data.Char (isDigit, isSpace)
import Data.Graph (graphFromEdges, reachable)

part1 :: [[Int]] -> Int
part1 pipes = length $ reachable graph 0
    where (graph, nodeFromVertex, vertexFromKey) = graphFromEdges relations
          relations = map rel pipes
          rel (x:rest) = (x,x,rest)

main = do
    pipes <- map (map read . words . filter (\c -> isDigit c || isSpace c)) . lines <$> readFile "day12.txt"
    print $ part1 pipes