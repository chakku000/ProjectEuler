fib 0 = 1
fib 1 = 2
fib n = fib (n-1) + fib (n-2)

main = print $ sum $ filter (\x -> mod x 2 == 0 ) $ takeWhile (< 4*10^6) $ map fib [0..]
