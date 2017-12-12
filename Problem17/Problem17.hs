import qualified Data.Map as Map

str1= ["zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty"]
str2=["thirty","forty","fifty","sixty","seventy","eighty","ninety"]

dict = Map.fromList $ zip [0..20] str1 ++ zip [30,40..90] str2 ++ zip [1000] ["onethousand"]

parse :: Integer -> String
parse n
  | Map.member n dict = dict Map.! n
  | n >= 100 = parse3 n
  | n >= 10 ~ parse2 n
  | otherwise = parse1 n

parse3 :: Integer -> String
parse3 n = (parse h) ++ (parse2 m)
  where
    h = n `div` 100
    m = mod n 100

parse2 :: Integer -> String
parse2 n = (parse t) ++ (parse1 m)
  where
    t = n `div` 10
    m = mod n 10
