import Control.Applicative
import Data.List
import Data.Ord (comparing)

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = let (f,b) = splitAt (n-1) xs in f ++ x : b

insertAt' :: a -> [a] -> Int -> [a]
insertAt' x ys     1 = x : ys
insertAt' x (y:ys) n = y : insertAt x ys (n-1)

range :: (Ord a, Eq a, Enum a) => a -> a -> [a]
range x y
  | x < y     = x : range (succ x) y
  | x > y     = x : range (pred x) y
  | otherwise = [x]

range' x y = [x..y]

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

lsort' :: [[a]] -> [[a]]
lsort' = sortBy (\xs ys -> compare (length xs) (length ys))
