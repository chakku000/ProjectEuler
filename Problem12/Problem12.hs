triangle :: [Integer]
triangle = [x * (x+1) `div` 2 | x <- [1..]]

--素因数分解  [(素因数,個数)]
factorization :: Integer -> [(Integer,Integer)]
factorization n = factor n 2
  where
    divcount :: Integer -> Integer -> Integer
    divcount n m = if (mod n m) == 0 then 1+(divcount (n `div` m) m) else 0   -- nをmで割り切れる回数
    factor :: Integer -> Integer -> [(Integer,Integer)]
    factor n m
      | n == 1  = []
      | m*m > n = [(n,1)]
      | otherwise  = if divc==0 then factor divn (m+1) else (m,divc) : factor divn (m+1)
          where divc = divcount n m
                divn = n `div` (m^divc)

count_divisor :: Integer -> Integer
count_divisor n = foldl (\acc (_,x) -> acc * (x+1)) 1 $ factorization n

main = print $ fst $ head $ filter (\ (_,x) -> x > 500) $ map (\x -> (x,count_divisor x)) triangle
