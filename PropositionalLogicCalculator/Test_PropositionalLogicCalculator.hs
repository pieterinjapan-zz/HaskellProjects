{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2020-12-29

Unit and property tests for propositional logic calculator
-}
import PropositionalLogicCalculator
import PropositionalLogicCalculatorParser
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
p_show_00 = Const True
p_show_01 = Const False
p_show_02 = Not p_show_01
p_show_03 = Not (Var 'A')
p_show_04 = And (Var 'A') (Var 'B')
p_show_05 = Or (Var 'A') (Var 'B')
p_show_06 = Imply (Var 'A') (Var 'B')
p_show_07 = Eq (Var 'A') (Var 'B')
p_show_08 = Or (Var 'A') (Not (Var 'A'))
p_show_09 = Not (And (Var 'A') (Not (Var 'A')))
p_show_10 = Eq (Not (Not (Var 'A'))) (Var 'A')
p_show_11 = Imply (Imply (Imply (Var 'A') (Var 'B')) (Var 'A')) (Var 'A')
p_show_12 = Eq (Not (And (Var 'A') (Var 'B'))) (Or (Not (Var 'A')) (Not (Var 'B')))
p_show_13 = Eq (Not (Or (Var 'A') (Var 'B'))) (And (Not (Var 'A')) (Not (Var 'B')))
p_show_14 = Eq (Imply (Var 'A') (Var 'B')) (Or (Not (Var 'A')) (Var 'B'))
p_show_15 = Eq (Eq (Var 'A') (Var 'B')) (Or (And (Var 'A') (Var 'B')) (And (Not (Var 'A')) (Not (Var 'B'))))

test_p_show_00 = show p_show_00 == p_read_00
test_p_show_01 = show p_show_01 == p_read_01
test_p_show_02 = show p_show_02 == p_read_02
test_p_show_03 = show p_show_03 == p_read_03
test_p_show_04 = show p_show_04 == p_read_04
test_p_show_05 = show p_show_05 == p_read_05
test_p_show_06 = show p_show_06 == p_read_06
test_p_show_07 = show p_show_07 == p_read_07
test_p_show_08 = show p_show_08 == p_read_08
test_p_show_09 = show p_show_09 == p_read_09
test_p_show_10 = show p_show_10 == p_read_10
test_p_show_11 = show p_show_11 == p_read_11
test_p_show_12 = show p_show_12 == p_read_12
test_p_show_13 = show p_show_13 == p_read_13
test_p_show_14 = show p_show_14 == p_read_14
test_p_show_15 = show p_show_15 == p_read_15

-- test propositions for parser
p_read_00 = "True"
p_read_01 = "False"
p_read_02 = "True"
p_read_03 = "~A"
p_read_04 = "(A and B)"
p_read_05 = "(A or B)"
p_read_06 = "(A --> B)"
p_read_07 = "(A <--> B)"
p_read_08 =  "(A or ~A)"
p_read_09 = "~(A and ~A)"
p_read_10 = "(~~A <--> A)"
p_read_11 = "(((A --> B) --> A) --> A)"
p_read_12 = "(~(A and B) <--> (~A or ~B))"
p_read_13 = "(~(A or B) <--> (~A and ~B))"
p_read_14 = "((A --> B) <--> (~A or B))"
p_read_15 = "((A <--> B) <--> ((A and B) or (~A and ~B)))"

p_parseTree_00 = Leaf "True"
p_parseTree_01 = Leaf "False"
p_parseTree_02 = Leaf "True"
p_parseTree_03 = Node1 "~" (Leaf "A")
p_parseTree_04 = Node2 "and" (Leaf "A") (Leaf "B")
p_parseTree_05 = Node2 "or" (Leaf "A") (Leaf "B")
p_parseTree_06 = Node2 "-->" (Leaf "A") (Leaf "B")
p_parseTree_07 = Node2 "<-->" (Leaf "A") (Leaf "B")
p_parseTree_08 = Node2 "or" (Leaf "A") (Node1 "~" (Leaf "A"))
p_parseTree_09 = Node1 "~" (Node2 "and" (Leaf "A") (Node1 "~" (Leaf "A")))
p_parseTree_10 = Node2 "<-->" (Node1 "~" (Node1 "~" (Leaf "A"))) (Leaf "A")
p_parseTree_11 = Node2 "-->" (Node2 "-->" (Node2 "-->" (Leaf "A") (Leaf "B")) (Leaf "A")) (Leaf "A")
p_parseTree_12 = Node2 "<-->" (Node1 "~" (Node2 "and" (Leaf "A") (Leaf "B"))) (Node2 "or" (Node1 "~" (Leaf "A")) (Node1 "~" (Leaf "B")))  -- ((~A) or (~B)))"
p_parseTree_13 = Node2 "<-->" (Node1 "~" (Node2 "or" (Leaf "A") (Leaf "B"))) (Node2 "and" (Node1 "~" (Leaf "A")) (Node1 "~" (Leaf "B")))
p_parseTree_14 = Node2 "<-->" (Node2 "-->" (Leaf "A") (Leaf "B")) (Node2 "or" (Node1 "~" (Leaf "A")) (Leaf "B"))
p_parseTree_15 = Node2 "<-->" (Node2 "<-->" (Leaf "A") (Leaf "B"))
                 (Node2 "or" (Node2 "and" (Leaf "A") (Leaf "B")) (Node2 "and" (Node1 "~" (Leaf "A")) (Node1 "~" (Leaf "B"))))

test_p_parseTree_00 = parseTree p_read_00 == p_parseTree_00
test_p_parseTree_01 = parseTree p_read_01 == p_parseTree_01
test_p_parseTree_02 = parseTree p_read_02 == p_parseTree_02
test_p_parseTree_03 = parseTree p_read_03 == p_parseTree_03
test_p_parseTree_04 = parseTree p_read_04 == p_parseTree_04
test_p_parseTree_05 = parseTree p_read_05 == p_parseTree_05
test_p_parseTree_06 = parseTree p_read_06 == p_parseTree_06
test_p_parseTree_07 = parseTree p_read_07 == p_parseTree_07
test_p_parseTree_08 = parseTree p_read_08 == p_parseTree_08
test_p_parseTree_09 = parseTree p_read_09 == p_parseTree_09
test_p_parseTree_10 = parseTree p_read_10 == p_parseTree_10
test_p_parseTree_11 = parseTree p_read_11 == p_parseTree_11
test_p_parseTree_12 = parseTree p_read_12 == p_parseTree_12
test_p_parseTree_13 = parseTree p_read_13 == p_parseTree_13
test_p_parseTree_14 = parseTree p_read_14 == p_parseTree_14
test_p_parseTree_15 = parseTree p_read_15 == p_parseTree_15

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
  -- unit tests -
  ---------------

  -- show proposition test
  showTestResult test_p_show_00 "test_p_show_00"
  showTestResult test_p_show_01 "test_p_show_01"
  showTestResult test_p_show_02 "test_p_show_02"
  showTestResult test_p_show_03 "test_p_show_03"
  showTestResult test_p_show_04 "test_p_show_04"
  showTestResult test_p_show_05 "test_p_show_05"
  showTestResult test_p_show_06 "test_p_show_06"
  showTestResult test_p_show_07 "test_p_show_07"
  showTestResult test_p_show_08 "test_p_show_08"
  showTestResult test_p_show_09 "test_p_show_09"
  showTestResult test_p_show_10 "test_p_show_10"
  showTestResult test_p_show_11 "test_p_show_11"
  showTestResult test_p_show_12 "test_p_show_12"
  showTestResult test_p_show_13 "test_p_show_13"
  showTestResult test_p_show_14 "test_p_show_14"
  showTestResult test_p_show_15 "test_p_show_15"
  putStrLn ""

  -- parse proposition test
  showTestResult test_p_parseTree_00 "test_p_parseTree_00"
  showTestResult test_p_parseTree_01 "test_p_parseTree_01"
  showTestResult test_p_parseTree_02 "test_p_parseTree_02"
  showTestResult test_p_parseTree_03 "test_p_parseTree_03"
  showTestResult test_p_parseTree_04 "test_p_parseTree_04"
  showTestResult test_p_parseTree_05 "test_p_parseTree_05"
  showTestResult test_p_parseTree_06 "test_p_parseTree_06"
  showTestResult test_p_parseTree_07 "test_p_parseTree_07"
  showTestResult test_p_parseTree_08 "test_p_parseTree_08"
  showTestResult test_p_parseTree_09 "test_p_parseTree_09"
  showTestResult test_p_parseTree_10 "test_p_parseTree_10"
  showTestResult test_p_parseTree_11 "test_p_parseTree_11"
  showTestResult test_p_parseTree_12 "test_p_parseTree_12"
  showTestResult test_p_parseTree_13 "test_p_parseTree_13"
  showTestResult test_p_parseTree_14 "test_p_parseTree_14"
  showTestResult test_p_parseTree_15 "test_p_parseTree_15"
  putStrLn ""

  -- tautology checker tests
  showTestResult (isTautology p_tau_00) "Principle of Excluded Middle"
  showTestResult (isTautology p_tau_01) "Principle of Identity"
  showTestResult (isTautology p_tau_02) "Principle of Contradiction"
  showTestResult (isTautology p_tau_03) "Law of Double Negation"
  showTestResult (isTautology p_tau_04) "Peirce's Law"
  showTestResult (isTautology p_tau_05) "De Morgan's Law (And)"
  showTestResult (isTautology p_tau_06) "De Morgan's Law (Or)"
  showTestResult (isTautology p_tau_07) "Definition of Implication"
  showTestResult (isTautology p_tau_08) "Definition of Equivalence"
  putStrLn ""

  showTestResult (not $ isTautology p_not_tau_00) "Principle of Contradiction"
  showTestResult (not $ isTautology p_not_tau_01) "negation may be False"
  showTestResult (not $ isTautology p_not_tau_02) "conjunction may be False"
  showTestResult (not $ isTautology p_not_tau_03) "disjunction may be False"
  showTestResult (not $ isTautology p_not_tau_04) "implication may be False"
  showTestResult (not $ isTautology p_not_tau_05) "equivalence may be False"
  putStrLn ""

  -- property-based tests -
  -------------------------
  quickCheck (withMaxSuccess 10000 prop_showTestResult_00)
