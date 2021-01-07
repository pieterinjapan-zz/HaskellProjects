{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2021-01-06

Datatypes and instances for propositional logic calculator
-}
module PLCData where

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- synonym type for atomic proposition
--type Atom = Char
type Atom = String

-- data type for logical propositions, and operations on them
data Prop = Const Bool       -- truth value
          | Var Atom         -- name of proposition
          | Not Prop         -- negation
          | And Prop Prop    -- conjucntion
          | Or  Prop Prop    -- disjunction
          | Imply Prop Prop  -- implication
          | Eq Prop Prop     -- equivalence

-- associating boolean values to propositions
type Assoc k v = [(k,v)]
type Subst = Assoc Atom Bool -- ex [('A',True)] assigns True to 'A'

-- tree datastructure used for parsing String into Prop
data Tree a = Leaf a | Node1 a (Tree a) | Node2 a (Tree a) (Tree a)
 deriving (Show, Eq)

-- END
