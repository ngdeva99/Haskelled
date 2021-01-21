
import Prelude hiding (null,lookup,map,fst,filter,zip)

null :: [a] -> Bool 
null []       = True 
null (x : xs) = False 

null2 :: Eq a =>[a] -> Bool  -- more restricted type
null2 xs = xs == []

null3 :: [a] -> Bool 
null3 xs = length xs == 0 -- less efficiient

map ::  (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

fst :: (a,b) -> a
fst (firstComponent, _secondComponent) = firstComponent

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

zip :: [a] -> [b] -> [(a,b)]
-- zip [] [] = [] -- both empty lists
-- zip [] ys = []
-- zip (x : xs) [] = []
zip (x : xs) (y : ys) = (x,y) : zip xs ys 
zip _ _ = [] 

-- bad version with crash
lookupBad :: Eq k => [(k,v)] -> k -> v
lookupBad [] key = error "key not found"                                                                                                                                                
lookupBad ((key', val) : otherEntries) key
    | key == key' = val
    | otherwise   = lookupBad otherEntries key



lookup :: Eq k => [(k,v)] -> k -> Maybe v
lookup [] key = Nothing                                                                                                                                                 
lookup ((key', val) : otherEntries) key
    | key == key' = Just val
    | otherwise   = lookup otherEntries key
-- constructors of Maybe Type
-- Nothing :: Maybe a
-- Just :: a -> Maybe a

-- better version to find the head
safehead :: [a] -> Maybe a
safehead [] = Nothing 
safehead (x : _xs) = Just x



fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _def (Just x) = x


-- email directory example
orelse :: Maybe a-> Maybe a -> Maybe a
orelse Nothing y = y
orelse (Just x) _ = Just x


-- propagate a failure/
addMaybes :: Maybe Int  -> Maybe Int -> Maybe Int
addMaybes (Just x) (Just y) = Just (x + y)
addMaybes _ _ = Nothing 

multMaybes :: Maybe Int -> Maybe Int -> Maybe Int 
-- multMaybes (Just x) (Just y) = (Just x * y)
multMaybes _ _ = Nothing 


liftMaybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c 
liftMaybes op (Just x) (Just y) = Just (op x y)
liftMaybes _ _ _ = Nothing 

--- NEW DATA TYPE TREE
data Tree a = Leaf a | Node (Tree a) (Tree a)

exampleTree :: Tree Int 
exampleTree = 
    Node
        (Node (Leaf 1) (Leaf 2))
        (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))


countLeaves :: Tree a -> Int 
countLeaves (Leaf _)= 1
countLeaves (Node l r) = countLeaves l + countLeaves r 