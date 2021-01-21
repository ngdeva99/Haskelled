module Paths where

import Prelude hiding ((++), or, reverse, filter, replicate, repeat)
import Control.Applicative ((<|>), liftA2)

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



data Path =
      GoLeft Path
    | GoRight Path
    | End 
    deriving (Eq, Show)

examplePath :: Path
examplePath = GoLeft (GoRight End)


follow :: Path -> Tree a -> Maybe a
follow (GoLeft path) (Node l _) = follow path l
follow (GoRight path) (Node _ r) = follow path r
follow End            (Leaf x) = Just x
follow _ _ = Nothing 


search :: Eq a => a -> Tree a 
search x (Leaf y)
    | x == y = Just End
    | otherwise = Nothing 
search  x (Node l r) = 
        fmap GoLeft  (search x l) BST 


fst (length [getLine , getLine ], putStrLn "Hello")