{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2020-12-28

Unit and property tests for propositional logic calculator
-}
import PropositionalLogicCalculator

-- Helper Functions --
----------------------

-- show unit test result
showTestResult :: Bool -> String -> IO ()
showTestResult b str | b = putStrLn $ "Pass: " ++ str
                     | otherwise = putStrLn $ "Fail: " ++ str

-- TODO : property test for showTestResult

-- Unit Tests --
----------------

-- test propositions
p_A =  Var 'A'
p_B =  Var 'B'

-- Tautologies
p_tau_00 = p_A \/ (neg p_A) -- Principle of Excluded Middle
p_tau_01 = p_A ==> p_A -- Principle of Identity
p_tau_02 = neg (p_A /\ (neg p_A)) -- Principle of Contradiction
p_tau_03 = (neg (neg p_A)) <==> p_A -- Law of Double Negation
p_tau_04 = ((p_A ==> p_B) ==> p_A) ==> p_A -- Peirce's Law
p_tau_05 = (neg (p_A /\ p_B)) <==> ((neg p_A) \/ (neg p_B)) -- De Morgan's Law (And)
p_tau_06 = (neg (p_A \/ p_B)) <==> ((neg p_A) /\ (neg p_B)) -- De Morgan's Law (Or)
p_tau_07 = (p_A ==> p_B) <==> ((neg p_A) \/ p_B) -- Definition of Implication
p_tau_08 = (p_A <==> p_B) <==> ((p_A /\ p_B) \/ ((neg p_A) /\ (neg p_B))) -- Defenition of Equivalence

-- test propositions : Not Tautologies

{-
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
-}

main = do
  -- unit tests
  showTestResult (isTautology p_tau_00) "Principle of Excluded Middle"
  showTestResult (isTautology p_tau_01) "Principle of Identity"
  showTestResult (isTautology p_tau_02) "Principle of Contradiction"
  showTestResult (isTautology p_tau_03) "Law of Double Negation"
  showTestResult (isTautology p_tau_04) "Peirce's Law"
  showTestResult (isTautology p_tau_05) "De Morgan's Law (And)"
  showTestResult (isTautology p_tau_06) "De Morgan's Law (Or)"
  showTestResult (isTautology p_tau_07) "Definition of Implication"
  showTestResult (isTautology p_tau_08) "Definition of Equivalence"
