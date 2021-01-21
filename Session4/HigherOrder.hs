---------------------------------------------------------------------
-- HigherOrder.hs
--
-- (c) 2017-2021 Andres Loeh, Well-Typed LLP

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module HigherOrder where

import Data.List (foldl',sortBy)
import Prelude hiding (take, product, reverse)

-- These are binary trees with labels in their nodes.

data BinTree a =
    Bin (BinTree a) a (BinTree a)
  | Empty
  deriving (Eq, Show)

-- Task HigherOrder-1.
--
-- Define 'product' both using an accumulator explicitly,
-- and using (strict) foldl'.

product :: Num a => [a] -> a
product xs = foldl' (*) 1 xs

-- Task HigherOrder-2.
--
-- Define 'reverse' using 'foldl'.

reverse :: [a] -> [a]
reverse ys = foldl (\ acc y -> y : acc) [] ys

-- Task HigherOrder-3.
--
-- Define a Functor instance for binary trees. For
-- this, we have to define a map function on binary
-- trees and then define the class instance.
--
-- The instance is actually given below. You just
-- have to uncomment it.

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _ Empty = Empty
mapBinTree f (Bin l x r) = Bin (mapBinTree f l) (f x) (mapBinTree f r)



instance Functor BinTree where
  fmap = mapBinTree


-- Task HigherOrder-4.
--
-- The 'BinTree' type is suitable for representing
-- "binary search trees".
--
-- Binary search trees are trees that store their elements
-- in order, so that we can efficiently find elements by comparing
-- the element we are looking for with the current node, and
-- descending either left or right.
--
-- Define a function 'isBST' that checks if a given 'BinTree'
-- is a binary search tree.

isBST :: Ord a => BinTree a -> Bool
-- as explained in 21/01 session. Using a combination of flattening and checking for sorting it out.
isBST = isSorted . flatten

flatten :: Ord a => BinTree a -> [a]
flatten Empty = []
flatten (Bin l x r) = flatten l ++ [x] ++ flatten r

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [single_element] = True
isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2: xs) 

-- Task HigherOrder-5.
--
-- Define a function 'search' that looks up a value in a BST.
--
-- From now on, we use a type synonym to signal that a certain
-- binary tree should in fact be a binary search tree, even if
-- the type system does not actively enforce this.

type BST a = BinTree a

-- search :: a -> BST a -> Bool redefining.
search :: Ord a => a -> BST a -> Bool
search _ Empty = False
search v (Bin l x r)
  | v == x = True
  | otherwise = (search x l) && (search x r)

-- Task HigherOrder-6.
--
-- Define a function 'insert' that inserts a value into a BST
-- while maintaining the BST property. (Don't worry about balancing
-- the tree. That's not important for now. But do make sure you
-- maintain the BST property itself.)

-- insert :: a -> BST a -> BST a
insert :: Ord a => a -> BST a -> BST a
insert a Empty = Bin Empty a Empty
insert a (Bin l x r)
  | a > x = Bin l x (insert a r)
  | otherwise = Bin (insert a l) x r 



-- Task HigherOrder-7.
--
-- Define the function 'all' (as in the Prelude) using 'foldr'.
-- Hide the original binding from the Prelude by exluding it in
-- the module header. Provide the type signature yourself.

all'  :: (a->Bool) -> [a] ->Bool
all' f [] = True
all' f (x:xs) = ( f x ) && ( all' f  xs )

-- Task HigherOrder-8.
--
-- Import the function 'sortyBy' from the 'Data.List' module.
-- Then use this function to define a function that sorts a
-- list in descending rather than ascending order.

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy (flip compare)

-- Task HigherOrder-9.
--
-- Use 'insert' and 'foldr' to create a BST from a list.

fromListBST :: Ord a => [a] -> BST a
fromListBST = error "TODO: define fromListBST"

-- Task HigherOrder-10 (very difficult but important).
--
-- We want to attach unique numbers to each node in a binary
-- tree, so that all the numbers from left to right are labelled
-- in ascending order.
--
-- NOTE: You may have to define a more general helper function
-- in order to succeed here. Think about what information you
-- need to know in general before and after labelling a subtree,
-- and then try to define a function that computes the necessary
-- outputs from the necessary inputs.
--
-- There is a HINT at the end of this file if you need a bit
-- more guidance.

labelTree :: BinTree a -> BinTree (a, Int)
labelTree = error "TODO: define labelTree"

-- Task HigherOrder-11.
--
-- Another form of tree labeling does not use an integer, but
-- a label supply that is given as a list. So write a variant
-- of 'labelTree' that takes the labels from a list, but uses
-- every label only once. You may assume in this function that
-- the list contains infinitely many (or at least sufficiently
-- many) labels, so you don't have to return a 'Maybe' if the
-- list is too short, but can just crash.

labelTree' :: BinTree a -> [b] -> BinTree (a, b)
labelTree' = error "TODO: define labelTree'"

-- Task HigherOrder-12 (somewhat difficult).
--
-- The function 'foldr' is also called the catamorphism on
-- lists. It systematically replaces constructors by functions,
-- and recurses where the datatype itself is recursive, thereby
-- implementing the standard design pattern on lists.
--
-- Try to do the same for 'BinTree'. Look at functions operating
-- on 'BinTree' for inspiration, such as 'mapBinTree', 'search'
-- and 'insert' above, and try to abstract out the commonalities.
-- Introduce an argument corresponding to each of the constructors
-- of 'BinTree', and recurse naturally.
--
-- There is a HINT at the end of this file if you need a bit
-- more guidance.
--


-- Task HigherOrder-13 (very difficult).
--
-- Try to implement the function 'take' on lists using 'foldr'.
--
-- Consider the type signature and a possible definition of
-- take, and note that not just the list is being traversed,
-- but also the number changes.
--
-- There is a HINT at the end of this file if you need a bit
-- more guidance.
--

take :: Int -> [a] -> [a]
take _ []       = []
take n (x : xs)
  | n >= 0      = x : take (n - 1) xs
  | otherwise   = []

-- Task HigherOrder-14.
--
-- If you succeeded in defining 'take' in terms of 'foldr',
-- then perhaps it will not surprise you all that much that
-- even 'foldl' can be written in terms of 'foldr'.
--
-- Try to do this. The approach required is similar.

-- Task HigherOrder-15 (difficult).
--
-- Define a Foldable instance for binary trees.
-- For this, there are several methods, but it
-- seems easiest to implement a list-like 'foldr'
-- function on trees.

foldrBinTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrBinTree = error "TODO: implement foldrBinTree"

{-
instance Foldable BinTree where
  foldr = foldrBinTree
-}




-- HINTs for some of the exercises.
--
-- Only continue here if you need the extra information.
--

-- HINT for HigherOrder-10.
--
-- In general, you will need to know for each subtree what
-- the first label you can use is. But to obtain this, you
-- will also need to know for each subtree after labelling
-- it what the next label you can use is.
--
-- So try to define a helper function:
--
-- labelTreeAux :: Int -> BinTree a -> (BinTree (a, Int), Int)
--
-- The first argument indicates the next "free" number.
-- The returned integer should indicate the next "free" number
-- after labelling of the given tree is complete.
--
-- With this, you should be able to apply the standard
-- design pattern on binary trees, but you will still have
-- to use let or case constructs to deconstruct the
-- pairs resulting from the recursive calls.
--
--
-- HINT for HigherOrder-12.
--
-- The type signature of the catamorphism for binary trees
-- is:
--
-- cataBinTree :: (r -> a -> r -> r) -> r -> BinTree a -> r
--
-- The first argument corresponds to the 'Bin' case. It's
-- a function that gets three arguments: the results of the
-- two recursive calls and the label stored in the node.
--
-- The second argument corresponds to the 'Empty' case. As
-- leaves carry no additional information, this is just a
-- plain element of the result type (similar to how the
-- case for the empty list is treated in 'foldr').
--
--
-- HINT for HigherOrder-13.
--
-- The key insight here is that you can use 'foldr' in such
-- a way that the result is a function.
--
-- If the above hint is not enough for you, go on for more
-- detail:
--
-- You can imagine 'take' with its arguments flipped:
--
-- takeAux :: [a] -> (Int -> [a])
--
-- Now you can try to define 'take' in terms of foldr as
-- follows:
--
-- takeAux xs = foldr _1 _2 xs
--
-- The two holes now must have the following types:
--
-- _2 :: Int -> [a]
--
-- This corresponds to the case for the empty list. In dependence
-- of the numeric argument to 'take', you have to say which list
-- to return if the input list is empty. That should be easy.
--
-- _1 :: a -> (Int -> [a]) -> (Int -> [a])
--
-- Here you get two arguments, corresponding to the cons-case:
-- the next list element in question, and a partially applied
-- recursive case, where you know you'll take from the tail, but
-- you can still specify how many elements you want to take.
--
-- Finally, you also have to produce a function that will
-- receive the number of elements desired and has to produce the
-- correct list.
--
