--------------------------------------------------------------------------
-- Datatypes.hs
--
-- (c) 2017-2021 Andres Loeh, Well-Typed LLP

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module Datatypes where

-- We hide functions we are going to redefine.
import Prelude hiding ((++), or, reverse, filter, replicate, repeat,uncurry)
import Control.Applicative ((<|>), liftA2)

-- Task Datatypes-1.
--
-- Define logical implication, which is given
-- by the following truth table (first argument
-- to the left, second argument on the top)
--
--       | False | True
-- ------+-------+------
-- False | True  | True
-- True  | False | True
--
-- Try to make the definition as lazy as possible.
-- Do not use other functions

implies :: Bool -> Bool -> Bool
implies True False = False 
implies  _ _ = True

-- Task Datatypes-2.
--
-- Define logical implication again, this time in terms
-- of 'not' and '||', both of which are predefined.

implies' :: Bool -> Bool -> Bool
implies' a b = not (True || False)

-- Task Datatypes-3.
--
-- Reimplement 'orelse' from the slides.

orelse :: Maybe a -> Maybe a -> Maybe a
orelse Nothing y = y
orelse (Just x) _ = Just x

-- Task Datatypes-4.
--
-- Reimplement 'mapMaybe' from the slides.

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing 
mapMaybe x (Just y) = Just (x y) 

-- Task Datatypes-5.
--
-- In GHCi, check the existing operator (<|>), actually
-- imported from Control.Applicative in the module header.
--
-- Observe what type it has. Observe that it behaves
-- exactly the same on the examples from the slides
-- as the 'orelse' function.

-- Task Datatypes-6.
--
-- In GHCi, check the existing operator (<$>).
--
-- Observe what type it has. Observe that it behaves
-- exactly the same on the examples from the slides
-- as the 'mapMaybe' function.

-- Task Datatypes-7.
--
-- Implement a function that evaluates two 'Maybe's,
-- and if both are 'Just', returns their elements
-- as a pair. Otherwise, it returns 'Nothing'.
--
-- Define the function without using other functions.

pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe (Just a) (Just b) = Just (a,b)
pairMaybe _ _ = Nothing 


-- Task Datatypes-8.
--
-- Reimplement the function 'liftMaybe' from the slides.

liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe op (Just x) (Just y) = Just (op x y)
liftMaybe _ _ _ = Nothing 

-- Task Datatypes-9.
--
-- Reimplement 'pairMaybe' using 'liftMaybe'.

pairMaybe' :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe' = liftMaybe (,)

-- Task Datatypes-10.
--
-- Reimplement 'addMaybes' from the slides, but
-- by using 'liftMaybe'.

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes = liftMaybe (+)

-- Task Datatypes-11.
--
-- Comment out the type signature of 'addMaybes'
-- above. Then reload the file into GHCi and let
-- GHCi infer the type of 'addMaybes'.
--
-- See if it works if you use the function with
-- fractional numbers.

-- Task Datatypes-12.
--
-- Reimplement 'addMaybes' from the slides, this
-- time using 'pairMaybe', 'uncurry', and
-- 'mapMaybe'.

addMaybes' :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes' x y = fmap (\ (x,y) -> x + y) (pairMaybe' x y)

-- uncurry :: (Int -> Int -> Int) -> (Int->Int) -> Int
-- uncurry f (x,y) = f x

-- Task Datatypes-13.
--
-- In GHCi, check the existing function 'liftA2'.
--
-- Observe what type it has. Observe that it behaves
-- exactly the same on 'Maybe' as 'liftMaybe'; for
-- example, by replacing 'liftMaybe' by 'liftA2'
-- in the definition of the 'addMaybe'.

-- Task Datatypes-14.
--
-- Define a function that applies both the given
-- functions to the given value and returns the
-- results as a pair.

split :: (a -> b) -> (a -> c) -> a -> (b, c)
split a b x = (a x , b x)

-- Task Datatypes-15.
--
-- Reimplement '++' from the slides.

(++) :: [a] -> [a] -> [a]
(++) [] xs = xs
(++) (y : ys) xs = y : ((++) ys xs)

-- Task Datatypes-16.
--
-- Implement a function 'or' that checks whether
-- a least one element in the given list of 'Bool's
-- is 'True'.

or :: [Bool] -> Bool
or [] = False 
or (x:xs) = x == True || or xs

-- Task Datatypes-17.
--
-- Reimplement the function 'reverse' from the slides.

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- Task Datatypes-18.
--
-- Implement a function that takes two lists and
-- returns the reversed second list concatenated with
-- the first list.
--
-- Do NOT use 'reverse' and '++' to define this function.
-- Do it directly, by applying the standard design
-- pattern on lists.
--

reverseAcc :: [a] -> [a] -> [a]
reverseAcc a [] = a
reverseAcc a (x : xs) = reverseAcc(x : a) xs 

-- Task Datatypes-19.
--
-- One way to look at the previous task is that the first
-- argument is the accumulator, that is initially empty,
-- and while traversing the list, contains the reversed
-- list that we have seen so far.
--
-- Observe that the reimplemented reverse below indeed
-- reverses a list.

reverse' :: [a] -> [a]
reverse' = reverseAcc []

-- Task Datatypes-20.
--
-- Observe that '[1 .. 10]' produces a list containing
-- the numbers from '1' up to '10'.
--
-- Use this notation to generate lists of various lengths,
-- and then use the two versions of 'reverse' to reverse
-- them.
--
-- Do you observe one of the two versions to be faster
-- than the other?

-- Task Datatypes-21.
--
-- Reimplement 'filter' from the slides.

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter a (y : ys)
    |a y = y : filter a ys
    |otherwise  = filter a ys


-- Task Datatypes-22.
--
-- The predefined function 'mod' performs an integer
-- division and returns the remainder of the division.
--
-- In particular, 'mod' returns 0 if and only if the
-- first argument is divisible by the second.
--
-- Use 'mod', 'filter', and the '[1 .. n]' construction
-- to compute all the divisors of a given integer.

divisors :: Integral a => a -> [a]
divisors k = rest 2 k
  where
    rest n k | n*n > k = [k]
                  | n*n == k = [n, k]
                  | k `mod` n == 0 = (n:(k `div` n):b)
                  | otherwise = b
      where b = rest (n+1) k

  -- divisors' :: Integral a => a -> [a]
  -- divisors' i = filter (\ d -> i `mod` d == 0)[1 .. i]

-- Task Datatypes-23.
--
-- A call to 'replicate n x' should produce a list
-- containing n copies of x.

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate a b = b : replicate (a-1) b

-- Task Datatypes-24.
--
-- We can also create an infinite list of copies of
-- one element using 'repeat'.
--
-- Once you defined 'repeat', try to rewrite 'replicate'
-- as a one-liner that uses 'take' and 'repeat'.
--
-- (If you have not seen 'take' before, use GHCi to
-- figure out what it does.)

repeat :: a -> [a]
repeat a = a : repeat a

-- Task Datatypes-25.
--
-- The type class 'Integral' contains both 'Int',
-- a 64-bit integer type (assuming a 64-bit architecture),
-- and 'Integer', a memory-bounded 'Integer' type.
--
-- Use divisors on a really large number, such as
--
-- 1000000000000000000000
--
-- and verify that in principle, it works, and that lazy
-- evaluation produces parts of the list, even though you
-- probably cannot wait for the computation to finish.
--
-- You can interrupt the computation using Ctrl-C.

-- Task Datatypes-26.
--
-- A naive check whether a number is prime checks whether
-- its divisors are just 1 and the number itself.
-- Implement this check.

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isprimerec n (n-1)

isprimerec _ 1 = True
isprimerec n t = if (n `rem` t) == 0 then False else isprimerec n (t-1)

-- Task Datatypes-27.
--
-- What happens if you type in '[1 ..]' into GHCi?
-- Infinite  Loop of digits 1 to
-- Task Datatypes-28.
--
-- Compute the first 1000 prime numbers.

thousandPrimes :: [Int]
thousandPrimes = take 1000 (filter isPrime [1 ..])

-- After computing 'thousandPrimes' in GHCi once, compute
-- it a second time. What do you observe?


-- REASON : Top level constants are shared in  Haskell.


-- Task Datatypes-29.
--
-- Draw a picture (on paper) of 'tree4'.

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

tree1 :: Tree Int
tree1 = Leaf 1

tree2 :: Tree Int
tree2 = Node (Leaf 2) (Leaf 4)

tree3 :: Tree Int
tree3 = Node tree2 tree1

tree4 :: Tree Int
tree4 = Node tree2 tree3

-- Task Datatypes-30.
--
-- Re-implement 'height' from the slides.

height :: Tree a -> Int
height (Leaf _) = 0
height (Node left right) = 1 + max (height left) (height right)


-- Task Datatypes-31.
--
-- Implement a map function on trees. It
-- should produce a tree of the same shape
-- as the original tree, where the elements
-- in the new tree have been transformed by
-- the function.

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree fn (Leaf a)   = Leaf (fn a)
mapTree fn (Node l r) = Node (mapTree fn l) (mapTree fn r)

-- Task Datatypes-32.
--
-- Check whether two trees have the same
-- shape. Implement this directly, without
-- using other functions.

sameShape :: Tree a -> Tree b -> Bool
sameShape (Leaf _) (Leaf _) = True 
sameShape (Node l r) (Node l' r') = sameShape l l' && sameShape r r'
sameShape _ _  = False 
-- Task Datatypes-33.
--
-- Re-implement 'sameShape', but this time,
-- use the derived equality on trees and
-- 'mapTree'. Also use the pre-defined
-- "unit" datatype, which is a datatype with
-- just one value. It has special syntax and
-- can be thought to be defined via
--
--   data () = ()
--
-- The single constructor '()' is called
-- "unit" as well:
--
--   () :: ()

sameShape' :: Tree a -> Tree b -> Bool
sameShape' t1 t2 = mapTree (\_x -> ()) t1 == mapTree (\_x -> ()) t2 

-- Task Datatypes-34.
--
-- Build a complete tree of the given height
-- (i.e., a tree where all leaves are at the
-- same depth from the root).
--
-- If the given integer is zero or negative,
-- return just a single leaf.

buildTree :: Int -> Tree ()
buildTree d 
  | (d <= 0) = Leaf ()
  | otherwise    = Node (buildTree (d - 1)) (buildTree (d - 1))


-- Task Datatypes-35.
--
-- Write a function that takes a tree of
-- trees and turns it into a single tree
-- by replacing each leaf with the tree
-- contained in that leaf.

graft :: Tree (Tree a) -> Tree a
graft (Leaf x)   = x
graft (Node l r) = Node (graft l) (graft r)

-- Task Datatypes-36.
--
-- Explain in words what the following
-- function does.

function :: Tree Int -> Tree ()
function t = graft (mapTree buildTree t)

-- Task Datatypes-37.
--
-- Re-implement the 'eval' function on expressions
-- from the slides.

data Expr =
    Lit Int
  | Add Expr Expr
  | Neg Expr
  | IfZero Expr Expr Expr
  deriving (Eq, Show)

expr1 :: Expr
expr1 = Neg (Add (Lit 3) (Lit 5))

expr2 :: Expr
expr2 = IfZero expr1 (Lit 1) (Lit 0)

eval :: Expr -> Int 
eval (Lit i)  = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Neg e) = - (eval e) -- or negate
eval (IfZero e1 e2 e3) = if eval e1 == 0 then eval expr2 else eval e3

prop_eval1 :: Bool
prop_eval1 = eval expr1 == 8

prop_eval2 :: Bool
prop_eval2 = eval expr2 == 0

-- Task Datatypes-38.
--
-- Implement a function that constructs
-- an expression that subtracts one expression
-- from another.

sub :: Expr -> Expr -> Expr
sub = error "TODO: implement sub"

-- Task Datatypes-39.
--
-- Implement a function that counts the number
-- of operations in an expression. All of 'Add',
-- 'Neg', and 'IfZero' count as one operation.

countOps :: Expr -> Int
countOps = error "TODO: implement countOps"

-- Task Datatypes-40.
--
-- Implement a function that produces a textual
-- representation of an expression in a suitable
-- concrete syntax.
--
-- For example, the representation of expr2 should
-- be something like
--
--   ifzero - (3 + 5) then 1 else 0
--
-- Use as many parentheses as you need or want.
-- You do not have to try to minimize the number
-- of parentheses needed.

-- Task Datatypes-41.
--
-- Add a constructor 'Mul' for multiplication to
-- the expression language and adapt all functions
-- accordingly.

-- Task Datatypes-42.
--
-- Let's re-visit the 'Tree' datatype from above.
--
-- Now define a new datatype 'Path' that
-- represents the sequence of steps one can walk
-- through such a binary tree of type 'Tree', from
-- the root to a potential leaf.
--
-- A path can either end, or we can go down to
-- the left subtree and continue, or we can go
-- down to the right subtree and continue -- so
-- there should be three constructors.

data Path =
    GoLeft Path
    | GoRight Path
    | End -- placeholder; replace with definition
    deriving (Eq, Show)

-- Task Datatypes-43.
--
-- Define a function 'follow' that takes a path
-- and a tree and tries to look up the element
-- at the position described by the path:
examplePath :: Path
examplePath = GoLeft (GoRight End)

follow :: Path -> Tree a -> Maybe a
follow (GoLeft path) (Node l _) = follow path l
follow (GoRight path) (Node _ r) = follow path r
follow End            (Leaf x) = Just x
follow _ _ = Nothing 

-- Task Datatypes-44.
--
-- Define a function 'search' that tries to find
-- an element in a tree and returns the path.

search :: Eq a => a -> Tree a -> Maybe Path
search = error "TODO: implement search"

