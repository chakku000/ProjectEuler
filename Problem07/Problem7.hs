sieve (x:xs) = x : sieve [y | y <- xs , mod y x /= 0]
prime = sieve [2..]

main = print $ prime !! 10000
