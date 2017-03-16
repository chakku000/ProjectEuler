isUru :: Int -> Bool
isUru n
  | n `mod` 400 == 0 = True
  | n `mod` 100 == 0 = False
  | n `mod` 4 == 0 = True
  | otherwise = False

days :: Int -> Int -> Int
days y m
  | m `elem` [1,3,5,7,8,10,12] = 31
  | m `elem` [4,6,9,11] = 30
  | isUru y = 29
  | otherwise = 28


-- その年の最初の曜日
startWeek :: Int -> Int
startWeek y = (1+ (sum dlist)) `mod` 7
  where
    dlist = [if isUru yy then 366 else 365 | yy <- [1900 .. y-1]]

-- その年の日曜日から始まる月の数
sundaynum :: Int -> Int
sundaynum y = length $ filter (== 0) wlist
  where
    w = startWeek y     -- w曜日から始まる
    dlist = [days y m | m <- [1..11]]
    wlist = w : [(w+(sum $ take m dlist)) `mod` 7 | m <- [1..11] ]

main = print $ sum $ map sundaynum [1901 .. 2000]
