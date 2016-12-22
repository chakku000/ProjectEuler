calc :: Int -> Int
calc n = if even n then n `div` 2 else 3*n+1

collatz n len = if n==1 then len+1 else collatz (calc n) len+1


main = print $ snd $  maximum $ map (\x -> (collatz x 0 , x)) [333333..1000000-1]
