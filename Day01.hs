import Data.Char ( digitToInt )

-- | Day 01
--
-- >>> captcha "1122"
-- 3
-- >>> captcha "1111"
-- 4
-- >>> captcha "1234"
-- 0
-- >>> captcha "91212129"
-- 9

captcha :: String -> Int
captcha digits = check digits
    where check = sum . map fst . filter (uncurry (==)) . pairs . map digitToInt
          pairs digits = zip digits (tail $ cycle digits)

main = do
    input <- readFile "day01.txt"
    print $ captcha input
