import Text.ParserCombinators.ReadP
import Data.Maybe (catMaybes, fromJust)

data Group = Garbage String | Nested [Group]
    deriving (Eq, Show)

score :: Group -> Int
score = go 1
    where go outer (Garbage s) = 0
          go outer (Nested groups) = outer + sum (map (go (outer+1)) groups)

garbageCount :: Group -> Int
garbageCount (Garbage content) = length content
garbageCount (Nested groups) = sum $ map garbageCount groups

cancellable :: ReadP (Maybe Char)
cancellable = (char '!' >> get >> return Nothing) +++ (Just <$> satisfy (`notElem` "!>"))

garbage :: ReadP Group
garbage = Garbage <$> between (string "<") (string ">") (catMaybes <$> many cancellable)

group :: ReadP Group
group = Nested <$> between (string "{") (string "}") (sepBy (group +++ garbage) (string ","))

main = do
    groups <- fromJust . parseMaybe (sepBy1 group (string "\n")) <$> readFile "day09.txt"
    print $ map score groups
    print $ map garbageCount groups

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result