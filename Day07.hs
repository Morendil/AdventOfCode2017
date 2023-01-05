import Text.ParserCombinators.ReadP
import Data.Char ( isNumber, isAlpha )
import Data.Maybe ( fromJust, mapMaybe, isNothing )
import Data.Graph
import Data.List (find, groupBy, sort, partition)
import Data.Eq.HT (equating)

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

-- | Day 07 part 2
--
-- >>> weigh [(66,"foo",[])] "foo"
-- 66
-- >>> weigh [(66,"foo",["bar"]),(11,"baz",[]),(33,"bar",[])] "foo"
-- 99
-- >>> oddOneOut [(11,"root",["foo","bar","baz"]),(66,"foo",[]),(66,"bar",[]),(66,"baz",[])] "root"
-- Nothing
-- >>> oddOneOut [(11,"root",["foo","bar","baz"]),(66,"foo",[]),(99,"bar",[]),(66,"baz",[])] "root"
-- Just ("bar",33)

weigh :: [Program] -> String -> Int
weigh programs target = weight + sum (map (weigh programs) deps)
    where (weight, _, deps) = fromJust $ find (\(_,name,_) -> name == target) programs

oddOneOut :: [Program] -> String -> Maybe (String, Int)
oddOneOut programs target = if length grouped == 1 then Nothing else Just (snd $ head $ head lone, fst (head $ head lone) - fst (head $ head rest))
    where (lone, rest) = partition (\l -> length l == 1) grouped
          grouped = groupBy (equating fst) $ sort $ map (\t -> (weigh programs t, t)) deps
          findByName key = find (\(_,name,_) -> name == key) programs
          (weight, _, deps) = fromJust $ findByName target

adjustment :: [Program] -> String -> (String, Int)
adjustment programs target = if balancedAbove then supporting else adjustment programs (fst supporting)
    where supporting = fromJust $ oddOneOut programs target
          findByName key = find (\(_,name,_) -> name == key) programs
          balancedAbove = isNothing $ oddOneOut programs (fst supporting)

part2 :: [Program] -> String -> (String, Int)
part2 programs target = (anomalous, originalWeight - change)
    where (originalWeight, _, _) = fromJust $ findByName anomalous
          findByName key = find (\(_,name,_) -> name == key) programs
          (anomalous, change) = adjustment programs target

nameOf (_,name,_) = name
weightOf (weight,_,_) = weight

main = do
    relations <- fromJust . parseMaybe programs <$> readFile "day07.txt"
    let root = part1 relations
    putStrLn root
    print $ part2 relations root

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result