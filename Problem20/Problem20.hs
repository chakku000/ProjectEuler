import Data.Char

fact :: Integer -> Integer 
fact 0 = 1
fact n = n *  fact (n-1)

main = print $ sum $ map digitToInt $ show $ fact 100
