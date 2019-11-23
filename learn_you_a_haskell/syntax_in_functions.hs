
--PATTERN MATCHING

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
-- ghci> charName 'h'
-- "*** Exception: (...): Non-exhaustive patterns in function charName

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "Can't call tail on an empty list"
tail' (_:xs) = xs

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. the first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--GUARDS

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're normal"
    | bmi <= 30.0 = "You're overweight"
    | otherwise   = "You're obese"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight"
    | bmi <= normal = "You're normal"
    | bmi <= fat    = "You're overweight"
    | otherwise     = "You're obese"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
          --(skinny, normal, fat) = (18.5, 20.0, 30.0)

calcBMIs :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in  sideArea + 2 * topArea

calcBMIs' :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists"
                       (x:_) -> x
