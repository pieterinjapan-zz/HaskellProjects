{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2019-06-24

Abstract Calculating Machine
A simple calculating machine that performs addition and multiplication.
-}

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- data type for addition of expressions :
data Expr = Val Int        -- evaluated value
          | Add Expr Expr  -- addition
          | Mul Expr Expr  -- multiplication
  deriving (Show)

-- control stack for defining order of expression evaluation :
data Op   = EVAL_A Expr -- evaluating over addition
          | EVAL_M Expr -- evaluating over multiplication
          | ADD Int     -- add integer
          | MUL Int     -- multiply by integer
type Cont = [Op] -- control stack

------------------------------------------------
-- Functions :
------------------------------------------------

-- evaluating expression according to control stack :
eval :: Expr -> Cont -> Int
eval (Val n) c   = exec c n -- execute control stack on value n
eval (Add x y) c = eval x (EVAL_A y : c) -- evaluate x, then add addition evaluation of y to the stack
eval (Mul x y) c = eval x (EVAL_M y : c) -- evaluate y, then add multiplication evaluation of y to the stack

-- executing control stack :
exec :: Cont -> Int -> Int
exec [] n = n -- no more opperations to perform. return evaluated result
exec (ADD m : c ) n = exec c (m+n) -- execute addition operation
exec (MUL m : c ) n = exec c (m*n) -- execute multiplication operation
exec (EVAL_A x : c) n = eval x (ADD n : c)
exec (EVAL_M x : c) n = eval x (MUL n : c)

-- evaluating addition expression :
value :: Expr -> Int
value e = eval e []

-- test expressions :
p1 = Val 3
p2 = Val 4
p3 = Add p1 p2 -- > 7
p4 = Add (Add (Val 2) (Val 3)) (Val 5) -- > 10
p5 = Mul p4 p2 -- > 40
p6 = Mul p3 p4 -- > 70
