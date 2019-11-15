myLast :: [a] -> a
myLast [] = error "No end for empty lists"
myLast [a] = a
myLast a = myLast $ tail a
