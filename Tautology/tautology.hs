{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2019-07-02

Tautology Checker :

Given a proposition, the program checks whether it is a Tautology 
(true regardless of the truth values of its constituent propositions) 
or not. 
-}

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- data type for logical propositions, and operations on them :
data Prop = Const Bool       -- truth value
          | Var Char         -- name of proposition
          | Not Prop         -- negation
          | And Prop Prop    -- conjucntion
          | Or  Prop Prop    -- disjunction
          | Imply Prop Prop  -- implication
          | Eq Prop Prop     -- equivalence
  deriving (Show)

-- making Prop an intance of Eq :
{- here we declare two propositions to be
   equivalent if they share the same truth
   table -}
instance Eq Prop where
  p == q = truthTab p == truthTab q
  
-- associating boolean values to propositions :
type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool -- ex [('A',True)] assigns True to 'A'  
  
------------------------------------------------
-- Functions :
------------------------------------------------  

-- function returning the first value v for an associated key k in Assoc k v :
find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k',v) <- t, k' == k ]

-- function for evaluating propositions :
eval :: Subst -> Prop -> Bool
eval _ ( Const b )   = b
eval s ( Var x )     = find x s
eval s ( Not p )     = not ( eval s p )
eval s ( And p q )   = ( eval s p ) && ( eval s q )
eval s ( Or  p q )   = ( eval s p ) || ( eval s q )
eval s ( Imply p q ) = ( not ( eval s p ) ) || ( eval s q )
eval s ( Eq  p q )   = ( eval s p ) == ( eval s q )

-- function for extracting atomic proposition names from a composite proposition :
vars :: Prop -> [Char]
vars ( Const _ )   = []
vars ( Var x )     = [x]
vars ( Not p )     = vars p
vars ( And p q )   = vars p ++ vars q
vars ( Or  p q )   = vars p ++ vars q
vars ( Imply p q ) = vars p ++ vars q
vars ( Eq p q )    = vars p ++ vars q

-- remove duplicates from a list :
rmdups [] = []
rmdups (x:ls) = x : ( filter (/= x) (rmdups ls) )

-- produce all the possible configurations of n booleans :
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
        where
          bss = bools (n-1)

-- substitute possible truth values for a given proposition :
subst :: Prop -> [Subst]
subst p = map (zip vp) bss
        where
          vp  = ( rmdups . vars ) p
          bss = bools ( length vp )

-- generate truth table for proposition :
truthTab :: Prop -> [Bool]
truthTab p = ( map (\s -> eval s p) . subst ) p

-- test whether given proposition is a tautology :
isTaut :: Prop -> Bool
--isTaut p = and (map (\s -> eval s p) (subst p) )
isTaut = and . truthTab

-- test propositions :
p1 :: Prop
p1 = And ( Var 'A' ) ( Not ( Var 'A' ) )

p2 :: Prop
p2 = Imply ( And ( Var 'A' ) ( Var 'B' ) ) ( Var 'A' )

p3 :: Prop
p3 = Imply ( Var 'A' ) ( And ( Var 'A' ) ( Var 'B' ) )

p4 :: Prop
p4 = Imply ( And ( Var 'A' ) ( Imply ( Var 'A' ) ( Var 'B' ) ) ) ( Var 'B' )

p5 :: Prop
p5 = Imply ( Imply ( Var 'A') ( Var 'B') ) ( Or ( Not ( Var 'A' ) ) ( Var 'B' ) )

p6 :: Prop
p6 = Eq ( Or ( Var 'B' ) ( Var 'A' ) ) ( Or ( Var 'A' ) ( Var 'B' ) )
