data ListItem = Multiple Int Char | Single Char deriving (Show)

encodeModified :: String -> [ListItem]
encodeModified "" = []
encodeModified s
  | i == 1    = Single c : encodeModified (drop i s)
  | otherwise = Multiple i c : encodeModified (drop i s)
  where c = head s
        i = length (takeWhile (\e -> e == c) s)

decodeModified :: [ListItem] -> String
decodeModified [] = ""
decodeModified (x:xs) =
  case x of
    Single c     -> c : decodeModified xs
    Multiple i c -> (replicate i c) ++ decodeModified xs

encodeDirect :: String -> [ListItem]
encodeDirect [] = []
encodeDirect (x:xs)
  | i == 1    = Single x : encodeDirect xs
  | otherwise = Multiple i x : encodeDirect rest
  where (matched, rest) = span (==x) xs
        i = 1 + (length matched)

dupli :: Eq a => [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli :: Eq a => [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) i =
  replicated ++ repli xs i
  where
    repliHelper :: Eq a => Int -> a -> [a]
    repliHelper 0 a = []
    repliHelper n a = a : repliHelper (n-1) a
    replicated = repliHelper i x

repli' :: Eq a => [a] -> Int -> [a]
repli' xs n = concatMap (take n . repeat) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery l i  = take (i-1) l ++ dropEvery (drop i l) i

split :: [a] -> Int -> ([a],[a])
split xs n = ((take n xs), (drop n xs))

split' = flip splitAt

slice :: [a] -> Int -> Int -> [a]
slice a x y = take (y-x+1) $ drop (x-1) a

rotate :: [a] -> Int -> [a]
rotate xs n = back ++ front
  where fitN :: Int -> Int -> Int
        fitN x k
          | k > 0 = k
          | otherwise = x+k
        (front, back) = splitAt (fitN (length xs) n) xs

rotate' :: [a] -> Int -> [a]
rotate' xs n
  | n < 0     = rotate xs (n+(length xs))
  | otherwise = let (f,b) = splitAt n xs in b ++ f

removeAt :: Int -> [a] -> (a, [a])
removeAt _ []     = error "nothing to remove"
removeAt 0 (x:xs) = (x, xs)
removeAt n xs     = (e, rest)
  where e = xs !! (n-1)
        rest = take (n-1) xs ++ drop n xs