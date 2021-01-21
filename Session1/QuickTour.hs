import Prelude hiding (elem,(||),not)

five = 2 + 3

double x = x + x

test = double 7
-- these two lines have to appear together.
-- elem number []                         = False 
-- elem number (firstNumber:otherNumbers) = (number == firstNumber) || elem number otherNumbers

-- elem x [] = []
-- elem x (y:ys) = _ -- (x==y) || elem x ys


elem :: Eq  a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys) = (x == y) || elem x ys

-- elem 3 [4,3,7]
-- step by step internally looks lik elem 3 (4 : 3 : 7 :[])
-- checks which of the definitons of the 2 lines match.
-- temporairly assign number:3 fnumber 4 othernumbers [3,7]
-- (3 == 4) || elem 3 (3 : 7 : []) (defn of (||))
-- False ||elem 3 (3 : 7 :[])
-- elem 3 (3:7:[]) {second case of (||)}
-- (3==3) || elem 3 (7 : [])
-- True || elem 3 (7 : [])
-- True 
-- inbuilt recursive approach/ thats why no nedd of inc or dec through traversal/ --- CALEED Equationa;l reasoning.;/


-- defn of || we are explcitly defining here.
True || y=True
False || y=y

not True = False 
not False = True 

question x = x : x: []


hello :: String --[Char]
hello = "Hello"