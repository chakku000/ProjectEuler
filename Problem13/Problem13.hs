
--main = print $ take 50 $ show num
main = do
  str <- readFile "input"
  let
    num = map (\x -> (read x :: Integer)) $ lines str
    in
    print $ take 10 $ show $ sum num
