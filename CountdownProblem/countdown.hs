{-
Pieter van Wyk
Created : 2019-05-27
Updated : 2019-06-24

Countdown Problem :

Given a list of integers, and a target integer, generate all
the possible arithmetic expressions, using integers from the
list, that evaluate to the given target.
-}

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- data types for arithmetic operations :
data Op = Add -- addition
        | Sub -- subtraction
        | Mul -- multiplication
        | Div -- division
        | Pow -- exponentiation (to the power to)
        deriving(Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

-- data type for arithmetic expressions :
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)      = show n
  show (App op l r) = brak l ++ show op ++ brak r
                    where -- put brackets arount nested expressions
                      brak (Val n) = show n
                      brak e       = "(" ++ show e ++ ")"

-- type for succesfully evaluated expressions, and their results :
type Result = (Expr,Int)

------------------------------------------------
--  Functions :
------------------------------------------------

-- check whether the operation is valid (within game rules) :
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x >  y
valid Mul x y = x < y && x /= 1 && y /= 1 -- x = y case is covered by Pow
valid Div x y = mod x y == 0 && y /= 1 && y /= 0
valid Pow x y = y /= 1 && x /= 1 -- remove x^1 = 1 and 1^y = 1

-- aplication of operation to integers :
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = div x y
apply Pow x y = x^y

-- return list of values in an expression :
values :: Expr -> [Int]
values (Val n)       = [n]
values (App _ el er) = values el ++ values er

-- evaluates arithmetic expression :
eval :: Expr -> [Int]
eval (Val n)        = [ n | n > 0 ]
eval (App op el er) = [ apply op l r | l <- (eval el), r <- (eval er), valid op l r ]

-- return all subsequences of a list (power-set of list) :
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
            where
              yss = subs xs

-- possible ways of inserting new element to a list :
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- all permutations of elements of a list :
perms :: [a] -> [[a]]
perms []      = [[]]
perms (x:xs)  = concat $ map (interleave x) (perms xs)

-- all possible choices of zero or more elements from a list :
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- checks if an expression is a solution for a given list of integers :
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n | eval e == [n] = elem (values e) (choices ns)
                | otherwise     = False

-- ways to split list into two non-empty lists that append to give original list :
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : map (\(ls,rs) -> (x:ls,rs)) (split xs)

-- return all possible Results for which the list of values equals a given list and value :
results :: [Int] -> [Result]
results []  = []
results [n] = [ (Val n,n) | n > 0 ] -- only accept positive integers
results ns  = [ res | (ls,rs) <- split ns,
                       lx     <- results ls,
                       ry     <- results rs,
                       res    <- combine_R lx ry ]

combine_R :: Result -> Result -> [Result] -- combine function for Results
combine_R (e_l,v_l) (e_r,v_r) = [ (App o e_l e_r, apply o v_l v_r) | o <- [Add,Sub,Mul,Div,Pow], valid o v_l v_r ]

-- generates list of Results that solve the countdown problem for a given list and value :
solutions_R :: [Int] -> Int -> [(Expr,Int)]
solutions_R ns n = sortedPairLs $ filter ( \pr -> apx_n == snd pr ) ps
   where
     ps = [ (e,v_e) | ns' <- choices ns, (e,v_e) <- results ns' ]
     apx_n = nearSol n ps

-- finds closest solution for given integer in list of expressions :
nearSol :: Int -> [(Expr,Int)] -> Int 
nearSol n = snd . foldr f_n (n,n)
   where
     f_n p p_apx | abs ( n - snd p) < fst p_apx = ( abs ( n - snd p ) , snd p )
                 | otherwise = p_apx

-- sorting expressions in solution in order ofcomplexity :
{-
Here we define the complexity of an expression as
com = (#additions + #subtractions) 2*(#multiplications + #devisions) + 4*(#exponents)
-}
complexity :: Expr -> Int
complexity (Val _ ) = 0
complexity (App o e1 e2) | o == Add || o == Sub = 1 + complexity e1 + complexity e2
                         | o == Mul || o == Div = 2 + complexity e1 + complexity e2
                         | o == Pow  = 4 + complexity e1 + complexity e2
                         | otherwise = complexity e1 + complexity e2

-- sorts list of results in order of complexity :
sortedPairLs :: [(Expr,Int)] -> [(Expr,Int)]
sortedPairLs []     = []
sortedPairLs (p:ps) = sortedPairLs small ++ [p] ++ sortedPairLs large
                      where
                        small = [ p_sm | p_sm <- ps, c_fst p_sm <= c_fst p ]
                        large = [ p_la | p_la <- ps, c_fst p_la >  c_fst p ]
                        c_fst = complexity . fst
