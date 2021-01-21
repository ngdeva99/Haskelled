class MyEq a where 
    equals :: a -> a -> Bool

instance MyEq Int where 
    equals x y = abs (x - y) < 5

instance MyEq a => MyEq [a] where
    equals [] [] = True 
    equals (x : xs) (y : ys) = equals x y && equals xs ys
    equals _ _ = False 

data Foo = Bar Int | Baz Char Foo
    deriving Eq