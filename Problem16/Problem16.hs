digitsum :: Integer -> Integer
digitsum 0 = 0
digitsum n = (mod n 10) + digitsum (n `div` 10)

main = print $ digitsum (2^1000)
