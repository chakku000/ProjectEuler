import Debug.Trace

digit :: Int -> Int
digit x = length $ show x

parse :: Int -> String
parse 0 = ""
parse 1 = "one"
parse 2 = "two"
parse 3 = "three"
parse 4 = "four"
parse 5 = "five"
parse 6 = "six"
parse 7 = "seven"
parse 8 = "eight"
parse 9 = "nine"
parse 10 = "ten"
parse 11 = "eleven"
parse 12 = "twelve"
parse 13 = "thirteen"
parse 14 = "fourteen"
parse 15 = "fifteen"
parse 16 = "sixteen"
parse 17 = "seventeen"
parse 18 = "eighteen"
parse 19 = "nineteen"
parse 20 = "twenty"
parse 30 = "thirty"
parse 40 = "forty"
parse 50 = "fifty"
parse 60 = "sixty"
parse 70 = "seventy"
parse 80 = "eighty"
parse 90 = "ninety"
parse 1000 = "onethousand"
parse n 
  | digit n == 2 = (parse b) ++ (parse a)
--  | digit n == 2 = trace ("n is " ++ show n ++ " a=" ++ show a ++ " b=" ++ show b ++ " c=" ++ show c ) ((parse b) ++ (parse a))
  | digit n == 3 && (n `mod` 100) > 0 = (parse c) ++ "hundred" ++ "and" ++ (parse $ n `mod` 100)
  | digit n == 3 && ((n `mod` 100) == 0) = (parse c) ++ "hundred"
  | otherwise = ""
  where
    a = n `mod` 10
    b = ((n `div` 10) `mod` 10)*10
    c = (n `div` 100) `mod` 10

main = print $ sum $ map length $ map parse [1..1000]
