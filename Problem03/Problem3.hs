import qualified Data.Set as Set

factor :: Integer -> Set.Set Integer -> Set.Set Integer
factor 1 fct = fct
factor n fct = factor (n `div` d) (Set.insert d fct)
  where
    d = (filter (\x -> mod n x == 0) [2..]) !! 0

main = do
  print $ last $ Set.toList (factor 600851475143 $ Set.fromList [])
