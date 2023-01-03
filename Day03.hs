-- | Day 03 part 1
--
-- >>> steps 1
-- 0
-- >>> steps 12
-- 3
-- >>> steps 23
-- 2
-- >>> steps 1024
-- 31

steps :: Int -> Int
steps = dist . (!!) coords . pred
    where coords = scanl add (0,0) $ concatMap shell [0..]

-- | Day 03 part 1
--
-- >>> shell 0
-- [(1,0)]
-- >>> shell 1
-- [(0,-1),(-1,0),(-1,0),(0,1),(0,1),(1,0),(1,0),(1,0)]

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
dist :: (Int, Int) -> Int
dist (x,y) = abs x + abs y

shell :: Int -> [(Int, Int)]
shell 0 = [(1,0)]
shell n = replicate ((n*2)-1) (0,-1) ++ replicate (n*2) (-1,0) ++ replicate (n*2) (0,1) ++ replicate ((n*2)+1) (1,0)

main = do
    input <- read <$> readFile "day03.txt"
    print $ steps input