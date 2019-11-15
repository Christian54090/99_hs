import Data.List

myLast :: [a] -> a
myLast [] = error "No end for empty lists"
myLast [a] = a
myLast a = myLast $ tail a

--myLast = head . reverse

myButLast :: [a] -> a
myButLast [] = error "No but last for empty lists"
myButLast [a] = error "No but last for single item lists"
myButLast a = reverse a !! 1

--myButLast = head . tail . reverse

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Element doesn't exist"
elementAt a x = if x == 1 then (head a) else elementAt (tail a) (x-1)

--elementAt list i = list !! (i-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--myLength = foldl (const . (+1)) 0
--myLength = foldl (\n _ -> n + 1) 0

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ (x : [])

--myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a == (reverse a)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List a) = concatMap flatten a

compress :: (Eq a) => [a] -> [a]
compress = map head . group

--compress (x:xs) = x : (compress $ dropWhile (==x) xs)

pack :: (Eq a) => [a] -> [[a]]
pack = group

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

--encode xs = map (length &&& head) $ group xs