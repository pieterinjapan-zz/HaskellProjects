{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2020-12-29

Implementation of a propositional logic calculator
-}
module PropositionalLogicCalculator where

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

-- make Prop an intance of Eq
{- here we declare two propositions to be
   equivalent if they share the same truth
   table -}
instance Eq Prop where
  p == q = truthTable p == truthTable q

-- associating boolean values to propositions :
type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool -- ex [('A',True)] assigns True to 'A'

------------------------------------------------
-- Functions :
------------------------------------------------

-- representations for Prop Types
neg :: Prop -> Prop
neg p_A = Not p_A

(/\) :: Prop -> Prop -> Prop -- conjucntion
p_A /\ p_B = And p_A p_B

(\/) :: Prop -> Prop -> Prop -- disjunction
p_A \/ p_B = Or p_A p_B

(-->) :: Prop -> Prop -> Prop -- implication
p_A --> p_B = Imply p_A p_B

(<-->) :: Prop -> Prop -> Prop -- equivalence
p_A <--> p_B = Eq p_A p_B

-- function returning the first value v for an associated key k in Assoc k v
find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k',v) <- t, k' == k ]

-- interface between propositions and Truth values
-- Given a list of substitutions for atomic propositions
-- evaluates the Truth value of a given proposition
eval :: Subst -> Prop -> Bool
eval _ ( Const b )   = b
eval s ( Var x )     = find x s
eval s ( Not p )     = not ( eval s p )
eval s ( And p q )   = ( eval s p ) && ( eval s q )
eval s ( Or  p q )   = ( eval s p ) || ( eval s q )
eval s ( Imply p q ) = ( not ( eval s p ) ) || ( eval s q )
eval s ( Eq  p q )   = ( eval s p ) == ( eval s q )

-- function for extracting atomic proposition names from a composite proposition
vars :: Prop -> [Char]
vars ( Const _ )   = []
vars ( Var x )     = [x]
vars ( Not p )     = vars p
vars ( And p q )   = vars p ++ vars q
vars ( Or  p q )   = vars p ++ vars q
vars ( Imply p q ) = vars p ++ vars q
vars ( Eq p q )    = vars p ++ vars q

-- remove duplicates from a list
rmdups [] = []
rmdups (x:ls) = x : ( filter (/= x) (rmdups ls) )

-- produce all the possible configurations of n booleans
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
        where
          bss = bools (n-1)

-- substitute possible truth values for a given proposition
subst :: Prop -> [Subst]
subst p = map (zip vp) bss
        where
          vp  = ( rmdups . vars ) p
          bss = bools ( length vp )

-- generate truth table for proposition
truthTable :: Prop -> [Bool]
truthTable p = ( map (\s -> eval s p) . subst ) p

-- TODO : add Show functionality for truth table (Prop -> IO ())

-- test whether given proposition is a tautology
isTautology :: Prop -> Bool
isTautology = and . truthTable

-- TODO : add equivalence checker

-- END
