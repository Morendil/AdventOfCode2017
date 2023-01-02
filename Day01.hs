import Data.Char ( digitToInt )
import Data.Function.HT

-- | Day 01 part 1
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

-- | Day 01 part 2
--
-- >>> captcha2 "1212"
-- 6
-- >>> captcha2 "1221"
-- 0
-- >>> captcha2 "123425"
-- 4
-- >>> captcha2 "123123"
-- 12
-- >>> captcha2 "12131415"
-- 4

captcha2 :: String -> Int
captcha2 digits = check digits
    where check = sum . map fst . filter (uncurry (==)) . pairs . map digitToInt
          pairs digits = zip digits (nest halfway tail $ cycle digits)
            where halfway = length digits `div` 2

main = do
    input <- readFile "day01.txt"
    print $ captcha input
    print $ captcha2 input
