import Data.Set as Set

factor :: Integer -> Set Integer -> Set Integer
factor 1 fct = fct
factor n fct = factor (n `div` d) (insert d fct)
  where
    d = (Prelude.filter (\x -> mod n x == 0) [2..]) !! 0

main = do
  print $ last $ toList (factor 600851475143 $ fromList [])
