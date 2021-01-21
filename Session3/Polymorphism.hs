module Polymorphism where

f1, f2, f3 :: (Int,Int) -> (Int,Int)

f1 a = a
f2 (a,b) = (b,a)
f3 (a,b) = (a,0)

g1, g2, g3,g4 :: (a,a) -> (a,a)
g1 (a,b) = (a,a)
g2 (a,b) = (b,b)
g3 (a,b) = (b,a)
g4 (a,b) = (a,b)


h :: (a,b) -> (b,a) --want only what is on input. single possibility.
h (a,b) = (b,a)

-- map :: (a -> b) -> [a] -> [b]

-- map f [] = []

-- map (\x -> 0) xs = ... -- resulting lost can only contiainnig 0s