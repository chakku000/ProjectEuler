import qualified Data.Set as Set 
import Debug.Trace

seive :: Int -> Set.Set Int -> Set.Set Int
seive n set
  | n * n > (Set.findMax set) = set
  | otherwise = Set.insert (Set.findMin set) (seive (Set.findMin set) removedset)
      where nextset = Set.fromAscList $ takeWhile (<= Set.findMax set) [n*n,(n+1)*n..]
            removedset = Set.delete (Set.findMin set) (Set.foldl (\acc x -> Set.delete x acc) set nextset)

main = do
  print $ sum $ Set.toList $ Set.insert 2 $ seive 2 (Set.fromAscList [3..2000000])
