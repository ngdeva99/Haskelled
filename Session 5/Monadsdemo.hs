module Monadsdemo where

import qualified Data.Map as M

table :: M.Map Int Int

table =
    M.fromList
    [
        (2,4)
    ,   (3,6)
    ,   (4,8)
    ,   (5,8)
    ,   (6,8)
    ,   (8,10)
    ,   (10,20)
    ,   (16,32)
    ,   (32,64)

    ]


-- given a  num,ber lookup the no in the table a total of 3 times, and if successful, return final number and incremented by one.
--  if any of lookups fail, the whole function should fail.


-- horrible definition/.
{-
lookupThreeTimes :: Int -> Maybe Int
lookupThreeTimes i =
    case M.lookup i table of
        Nothing -> Nothing
        Just j  -> case M.lookup j table of
            Nothing -> Nothing
            Just k -> case M.lookup k table of
                Nothing -> Nothing
                Just l-> Just (l + 1)
-}
-- recursive solution.

lookupThreeTimes :: Int -> Maybe Int
lookupThreeTimes i =
    M.lookup i table >>>= \ j ->
    M.lookup j table >>>= \ k ->
    M.lookup k table >>>=  \l  ->
    rreturn (l + 1)
{-
lookupThreeTimes :: Int -> Maybe Int
lookThreeTimes i = do
    j <- M.lookup i table
    M.lookup j table
    M.lookup k table
    return (l + 1)
-}

(>>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>>=) op cont =
    case op of
        Nothing -> Nothing
        Just x -> cont x

rreturn :: a -> Maybe a
rreturn x  = Just x


data BinTree a =
     Bin (BinTree a) a (BinTree a)
    | Empty
    deriving (Eq,Show)

tree :: BinTree Char
tree = Bin (Bin Empty 'a' Empty) 'b' (Bin Empty 'c' Empty)


tree2 :: BinTree Char
tree2 = Bin tree 'x' tree 

labelTree :: BinTree a  -> BinTree (a, Int)
labelTree tree = fst (labelTreeAux tree 0)

labelTreeAux :: BinTree a -> Int -> (BinTree (a,Int)  -> Int)
labelTreeAux Empty initial = (Empty, initial)
labelTreeAux (Bin l x r) initial =
    -- recursively label left tree,
    case labelTreeAux l initial of
        (l', counterAfterleft) -> 
            let x' = (x, counterAfterleft) 
            in case labelTreeAux r (counterAfterleft + 1) of
                (r',counterAfterright) -> 
                    (Bin l' x' r', counterAfterright)

 