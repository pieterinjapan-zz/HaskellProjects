{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2020-12-28

Unit and property tests for propositional logic calculator
-}
import PropositionalLogicCalculator
import Test.QuickCheck

-- Helper Functions --
----------------------

-- show unit test result
showTestResult' :: Bool -> String -> String
showTestResult' b str | b = "Pass: " ++ str
                      | otherwise = "Fail: " ++ str

showTestResult :: Bool -> String -> IO ()
showTestResult b str = putStrLn $ showTestResult' b str

-- Unit Tests --
----------------

-- test propositions for show
p_00 = Const True
p_01 = Const False
p_02 = Not p_01
p_03 = Not (Var 'A')
p_04 = And (Var 'A') (Var 'B')

-- test propositions for tautology checker
p_A =  Var 'A'
p_B =  Var 'B'

-- tautologies
p_tau_00 = p_A \/ (neg p_A) -- Principle of Excluded Middle
p_tau_01 = p_A --> p_A -- Principle of Identity
p_tau_02 = neg (p_A /\ (neg p_A)) -- Principle of Contradiction
p_tau_03 = (neg (neg p_A)) <--> p_A -- Law of Double Negation
p_tau_04 = ((p_A --> p_B) --> p_A) --> p_A -- Peirce's Law
p_tau_05 = (neg (p_A /\ p_B)) <--> ((neg p_A) \/ (neg p_B)) -- De Morgan's Law (And)
p_tau_06 = (neg (p_A \/ p_B)) <--> ((neg p_A) /\ (neg p_B)) -- De Morgan's Law (Or)
p_tau_07 = (p_A --> p_B) <--> ((neg p_A) \/ p_B) -- Definition of Implication
p_tau_08 = (p_A <--> p_B) <--> ((p_A /\ p_B) \/ ((neg p_A) /\ (neg p_B))) -- Defenition of Equivalence

-- not tautologies
p_not_tau_00 = p_A /\ (neg p_A) -- Principle of Contradiction
p_not_tau_01 = neg p_A -- negation may be False
p_not_tau_02 = p_A /\ p_B -- conjunction may be False
p_not_tau_03 = p_A \/ p_B -- disjunction may be False
p_not_tau_04 = p_A --> p_B -- implication may be False
p_not_tau_05 = p_A <--> p_B -- equivalence may be False

-- Property Tests --
--------------------

-- property test for showTestResult
prop_showTestResult_00 :: Bool -> String -> Bool
prop_showTestResult_00 b str | b = result == "Pass: " ++ str
                             | otherwise = result == "Fail: " ++ str
  where result = showTestResult' b str

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

  showTestResult (not $ isTautology p_not_tau_00) "Principle of Contradiction"
  showTestResult (not $ isTautology p_not_tau_01) "negation may be False"
  showTestResult (not $ isTautology p_not_tau_02) "conjunction may be False"
  showTestResult (not $ isTautology p_not_tau_03) "disjunction may be False"
  showTestResult (not $ isTautology p_not_tau_04) "implication may be False"
  showTestResult (not $ isTautology p_not_tau_05) "equivalence may be False"

  -- property-based tests
  quickCheck (withMaxSuccess 10000 prop_showTestResult_00)
