
-- x == True
-- x == False   (not x)
-- if x then True else False     x || y
-- if x then False else y        not x && y


-- g True = False 
-- g False = y

-- Better implementation of Q1

-- because it does not matter on undefined property on argument 2.
implies False _ = True
implies True y = y


-- STEP FUCNTIONS

data Expr =
      Lit Int 
    | Add Expr Expr
    | Neg Expr
    | IfZero Expr Expr Expr

-- 3 + (-7)

expr1 :: Expr
expr1 = Add (Lit 3) (Neg (Lit 7))

-- ifzero (1 + 1) then 5 else 7
expr2 :: Expr
expr2 = IfZero (Add (Lit 1) (Lit 1)) (Lit 5) (Lit 7)
-- expr2 = Neg(IfZero (Add (Lit 1) (Lit 1)) (Lit 5) (Lit 7))

eval :: Expr -> Int 
eval (Lit i)  = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Neg e) = - (eval e) -- or negate
eval (IfZero e1 e2 e3) = if eval e1 == 0 then eval expr2 else eval e3


-- removeNeg :: Expr -> Expr


-- count no of additons
countAdds :: Expr -> Int
countAdds (Lit i) = 0
countAdds (Add e1 e2) = 1 + countAdds e1 + countAdds e2
countAdds (Neg e) = countAdds e
countAdds (IfZero e1 e2 e3) = countAdds e1 + countAdds e2 +  countAdds e3

