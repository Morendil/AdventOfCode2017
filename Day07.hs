import Text.ParserCombinators.ReadP
import Data.Char ( isNumber, isAlpha )
import Data.Maybe ( fromJust )
import Data.Graph

type Program = (Int, String, [String])

programs :: ReadP [Program]
programs = sepBy1 program (string "\n")
program :: ReadP Program
program = do
    name <- many1 (satisfy isAlpha)
    weight <- string " (" >> (number <* string ")")
    dependencies <- option [] (string " -> " >> sepBy (many1 (satisfy isAlpha)) (string ", "))
    return (weight, name, dependencies)
number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

part1 :: [Program] -> String
part1 relations = nameOf $ nodeFromVertex $ head $ topSort graph
    where (graph, nodeFromVertex, vertexFromKey) = graphFromEdges relations
          nameOf (_,name,_) = name

main = do
    relations <- fromJust . parseMaybe programs <$> readFile "day07.txt"
    putStrLn $ part1 relations

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result