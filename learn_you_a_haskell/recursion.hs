maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)
-- maximum'' [3,6,7,5] => max 3 (max 6 (max 7 (5)))
--                              7      7      7
-- maximum'' [3,4,7,5] == 7

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x
-- replicate' 3 5 => x:x:x:[]
-- replicate' 3 5 == [5,5,5]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-- reverse' [4,3,2,1] => [] ++ [1] ++ [2] ++ [3] ++ [4]
-- reverse' [4,3,2,1] == [1,2,3,4]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger  = quicksort [a | a <- xs, a > x]
    in  smaller ++ [x] ++ bigger
