{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2021-01-06

Unit and property tests for propositional logic calculator
-}
import PLCData
import PLCEngine
import PLCParser
import PLCDisplay
import Test.QuickCheck

-- Helper Functions --
----------------------

-- show unit test result
showTestResult' :: Bool -> String -> String
showTestResult' b str | b = "Pass: " ++ str
                      | otherwise = "Fail: " ++ str

showTestResult :: Bool -> String -> IO ()
showTestResult b str = putStrLn $ showTestResult' b str

-- run property test
runPropertyTest :: Testable p => p -> String -> IO ()
runPropertyTest prop str = do
  putStrLn str
  quickCheck (withMaxSuccess 10000 prop)
  putStrLn ""

-- helper function for property tests of read
modifyString :: String -> String -> String
modifyString str alt_str = let s = [c | c <- (concat $ words str), c /= '(', c /= ')', c /= '~']
                           in if elem s ["","and","or","-->","<-->"]
                              then alt_str
                              else s

-- Test Data --
---------------

-- test propositions for show
p_show_00 = Const True
p_show_01 = Const False
p_show_02 = Not p_show_01
p_show_03 = Not (Var "A")
p_show_04 = And (Var "A") (Var "B")
p_show_05 = Or (Var "A") (Var "B")
p_show_06 = Imply (Var "A") (Var "B")
p_show_07 = Eq (Var "A") (Var "B")
p_show_08 = Or (Var "A") (Not (Var "A"))
p_show_09 = Not (And (Var "A") (Not (Var "A")))
p_show_10 = Eq (Not (Not (Var "A"))) (Var "A")
p_show_11 = Imply (Imply (Imply (Var "A") (Var "B")) (Var "A")) (Var "A")
p_show_12 = Eq (Not (And (Var "A") (Var "B"))) (Or (Not (Var "A")) (Not (Var "B")))
p_show_13 = Eq (Not (Or (Var "A") (Var "B"))) (And (Not (Var "A")) (Not (Var "B")))
p_show_14 = Eq (Imply (Var "A") (Var "B")) (Or (Not (Var "A")) (Var "B"))
p_show_15 = Eq (Eq (Var "A") (Var "B")) (Or (And (Var "A") (Var "B")) (And (Not (Var "A")) (Not (Var "B"))))

-- test propositions for read
p_read_00 = "True"
p_read_01 = "False"
p_read_02 = "True"
p_read_03 = "~A"
p_read_04 = "A and B" --"(A and B)"
p_read_05 = "A or B" -- "(A or B)"
p_read_06 = "A --> B" -- "(A --> B)"
p_read_07 = "A <--> B" -- "(A <--> B)"
p_read_08 = "A or ~A" -- "(A or ~A)"
p_read_09 = "~(A and ~A)"
p_read_10 = "~~A <--> A" --"(~~A <--> A)"
p_read_11 = "((A --> B) --> A) --> A" --"(((A --> B) --> A) --> A)"
p_read_12 = "~(A and B) <--> (~A or ~B)" -- "(~(A and B) <--> (~A or ~B))"
p_read_13 = "~(A or B) <--> (~A and ~B)" -- "(~(A or B) <--> (~A and ~B))"
p_read_14 = "(A --> B) <--> (~A or B)" -- "((A --> B) <--> (~A or B))"
p_read_15 = "(A <--> B) <--> ((A and B) or (~A and ~B))" --"((A <--> B) <--> ((A and B) or (~A and ~B)))"

-- test propositions for parseTree
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

-- test propositions for tautology checker
p_A =  Var "A"
p_B =  Var "B"

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

-- Unit Tests --
----------------

-- unit tests for show
test_show_00 = show p_show_00 == p_read_00
test_show_01 = show p_show_01 == p_read_01
test_show_02 = show p_show_02 == p_read_02
test_show_03 = show p_show_03 == p_read_03
test_show_04 = show p_show_04 == p_read_04
test_show_05 = show p_show_05 == p_read_05
test_show_06 = show p_show_06 == p_read_06
test_show_07 = show p_show_07 == p_read_07
test_show_08 = show p_show_08 == p_read_08
test_show_09 = show p_show_09 == p_read_09
test_show_10 = show p_show_10 == p_read_10
test_show_11 = show p_show_11 == p_read_11
test_show_12 = show p_show_12 == p_read_12
test_show_13 = show p_show_13 == p_read_13
test_show_14 = show p_show_14 == p_read_14
test_show_15 = show p_show_15 == p_read_15

-- unit tests for parseTree
test_parseTree_00 = parseTree p_read_00 == p_parseTree_00
test_parseTree_01 = parseTree p_read_01 == p_parseTree_01
test_parseTree_02 = parseTree p_read_02 == p_parseTree_02
test_parseTree_03 = parseTree p_read_03 == p_parseTree_03
test_parseTree_04 = parseTree p_read_04 == p_parseTree_04
test_parseTree_05 = parseTree p_read_05 == p_parseTree_05
test_parseTree_06 = parseTree p_read_06 == p_parseTree_06
test_parseTree_07 = parseTree p_read_07 == p_parseTree_07
test_parseTree_08 = parseTree p_read_08 == p_parseTree_08
test_parseTree_09 = parseTree p_read_09 == p_parseTree_09
test_parseTree_10 = parseTree p_read_10 == p_parseTree_10
test_parseTree_11 = parseTree p_read_11 == p_parseTree_11
test_parseTree_12 = parseTree p_read_12 == p_parseTree_12
test_parseTree_13 = parseTree p_read_13 == p_parseTree_13
test_parseTree_14 = parseTree p_read_14 == p_parseTree_14
test_parseTree_15 = parseTree p_read_15 == p_parseTree_15

-- unit tests for treeToProp
test_treeToProp_00 = treeToProp p_parseTree_00 == p_show_00
test_treeToProp_01 = treeToProp p_parseTree_01 == p_show_01
test_treeToProp_02 = treeToProp p_parseTree_02 == p_show_02
test_treeToProp_03 = treeToProp p_parseTree_03 == p_show_03
test_treeToProp_04 = treeToProp p_parseTree_04 == p_show_04
test_treeToProp_05 = treeToProp p_parseTree_05 == p_show_05
test_treeToProp_06 = treeToProp p_parseTree_06 == p_show_06
test_treeToProp_07 = treeToProp p_parseTree_07 == p_show_07
test_treeToProp_08 = treeToProp p_parseTree_08 == p_show_08
test_treeToProp_09 = treeToProp p_parseTree_09 == p_show_09
test_treeToProp_10 = treeToProp p_parseTree_10 == p_show_10
test_treeToProp_11 = treeToProp p_parseTree_11 == p_show_11
test_treeToProp_12 = treeToProp p_parseTree_12 == p_show_12
test_treeToProp_13 = treeToProp p_parseTree_13 == p_show_13
test_treeToProp_14 = treeToProp p_parseTree_14 == p_show_14
test_treeToProp_15 = treeToProp p_parseTree_15 == p_show_15

-- unit tests for read
test_read_00 = (read p_read_00 :: Prop) == p_show_00
test_read_01 = (read p_read_01 :: Prop) == p_show_01
test_read_02 = (read p_read_02 :: Prop) == p_show_02
test_read_03 = (read p_read_03 :: Prop) == p_show_03
test_read_04 = (read p_read_04 :: Prop) == p_show_04
test_read_05 = (read p_read_05 :: Prop) == p_show_05
test_read_06 = (read p_read_06 :: Prop) == p_show_06
test_read_07 = (read p_read_07 :: Prop) == p_show_07
test_read_08 = (read p_read_08 :: Prop) == p_show_08
test_read_09 = (read p_read_09 :: Prop) == p_show_09
test_read_10 = (read p_read_10 :: Prop) == p_show_10
test_read_11 = (read p_read_11 :: Prop) == p_show_11
test_read_12 = (read p_read_12 :: Prop) == p_show_12
test_read_13 = (read p_read_13 :: Prop) == p_show_13
test_read_14 = (read p_read_14 :: Prop) == p_show_14
test_read_15 = (read p_read_15 :: Prop) == p_show_15

-- unit tests for tautology checker
test_isTautology_00 = isTautology p_tau_00
test_isTautology_01 = isTautology p_tau_01
test_isTautology_02 = isTautology p_tau_02
test_isTautology_03 = isTautology p_tau_03
test_isTautology_04 = isTautology p_tau_04
test_isTautology_05 = isTautology p_tau_05
test_isTautology_06 = isTautology p_tau_06
test_isTautology_07 = isTautology p_tau_07
test_isTautology_08 = isTautology p_tau_08

test_notTautology_00 = not $ isTautology p_not_tau_00
test_notTautology_01 = not $ isTautology p_not_tau_01
test_notTautology_02 = not $ isTautology p_not_tau_02
test_notTautology_03 = not $ isTautology p_not_tau_03
test_notTautology_04 = not $ isTautology p_not_tau_04
test_notTautology_05 = not $ isTautology p_not_tau_05

-- Property Tests --
--------------------

-- property test for showTestResult
prop_showTestResult_00 :: Bool -> String -> Bool
prop_showTestResult_00 b str | b = result == "Pass: " ++ str
                             | otherwise = result == "Fail: " ++ str
  where result = showTestResult' b str

-- property test for read
prop_read_00 :: String -> Bool
prop_read_00 str1 = prop1 == prop2
  where str1' = modifyString str1 "A"
        prop_str = "~" ++ str1'
        prop1 = Not (Var str1')
        prop2 = read prop_str :: Prop

prop_read_binary :: (Prop -> Prop -> Prop) -> String -> String -> String -> Bool
prop_read_binary op op_str str1 str2 = prop1 == prop2
  where str1' = modifyString str1 "A"
        str2' = modifyString str2 "B"
        prop_str = str1' ++ op_str ++ str2'
        prop1 = op (Var str1') (Var str2')
        prop2 = read prop_str :: Prop

prop_read_01 = prop_read_binary And " and "
prop_read_02 = prop_read_binary Or " or "
prop_read_03 = prop_read_binary Imply " --> "
prop_read_04 = prop_read_binary Eq " <--> "

main = do
  -- unit tests -
  ---------------

  putStrLn "show proposition test"
  showTestResult test_show_00 "test_show_00"
  showTestResult test_show_01 "test_show_01"
  showTestResult test_show_02 "test_show_02"
  showTestResult test_show_03 "test_show_03"
  showTestResult test_show_04 "test_show_04"
  showTestResult test_show_05 "test_show_05"
  showTestResult test_show_06 "test_show_06"
  showTestResult test_show_07 "test_show_07"
  showTestResult test_show_08 "test_show_08"
  showTestResult test_show_09 "test_show_09"
  showTestResult test_show_10 "test_show_10"
  showTestResult test_show_11 "test_show_11"
  showTestResult test_show_12 "test_show_12"
  showTestResult test_show_13 "test_show_13"
  showTestResult test_show_14 "test_show_14"
  showTestResult test_show_15 "test_show_15"
  putStrLn ""

  putStrLn "parseTree test"
  showTestResult test_parseTree_00 "test_parseTree_00"
  showTestResult test_parseTree_01 "test_parseTree_01"
  showTestResult test_parseTree_02 "test_parseTree_02"
  showTestResult test_parseTree_03 "test_parseTree_03"
  showTestResult test_parseTree_04 "test_parseTree_04"
  showTestResult test_parseTree_05 "test_parseTree_05"
  showTestResult test_parseTree_06 "test_parseTree_06"
  showTestResult test_parseTree_07 "test_parseTree_07"
  showTestResult test_parseTree_08 "test_parseTree_08"
  showTestResult test_parseTree_09 "test_parseTree_09"
  showTestResult test_parseTree_10 "test_parseTree_10"
  showTestResult test_parseTree_11 "test_parseTree_11"
  showTestResult test_parseTree_12 "test_parseTree_12"
  showTestResult test_parseTree_13 "test_parseTree_13"
  showTestResult test_parseTree_14 "test_parseTree_14"
  showTestResult test_parseTree_15 "test_parseTree_15"
  putStrLn ""

  putStrLn "treeToProp test"
  showTestResult test_treeToProp_00 "test_treeToProp_00"
  showTestResult test_treeToProp_01 "test_treeToProp_01"
  showTestResult test_treeToProp_02 "test_treeToProp_02"
  showTestResult test_treeToProp_03 "test_treeToProp_03"
  showTestResult test_treeToProp_04 "test_treeToProp_04"
  showTestResult test_treeToProp_05 "test_treeToProp_05"
  showTestResult test_treeToProp_06 "test_treeToProp_06"
  showTestResult test_treeToProp_07 "test_treeToProp_07"
  showTestResult test_treeToProp_08 "test_treeToProp_08"
  showTestResult test_treeToProp_09 "test_treeToProp_09"
  showTestResult test_treeToProp_10 "test_treeToProp_10"
  showTestResult test_treeToProp_11 "test_treeToProp_11"
  showTestResult test_treeToProp_12 "test_treeToProp_12"
  showTestResult test_treeToProp_13 "test_treeToProp_13"
  showTestResult test_treeToProp_14 "test_treeToProp_14"
  showTestResult test_treeToProp_15 "test_treeToProp_15"
  putStrLn ""

  putStrLn "read proposition test"
  showTestResult test_read_00 "test_read_00"
  showTestResult test_read_01 "test_read_01"
  showTestResult test_read_02 "test_read_02"
  showTestResult test_read_03 "test_read_03"
  showTestResult test_read_04 "test_read_04"
  showTestResult test_read_05 "test_read_05"
  showTestResult test_read_06 "test_read_06"
  showTestResult test_read_07 "test_read_07"
  showTestResult test_read_08 "test_read_08"
  showTestResult test_read_09 "test_read_09"
  showTestResult test_read_10 "test_read_10"
  showTestResult test_read_11 "test_read_11"
  showTestResult test_read_12 "test_read_12"
  showTestResult test_read_13 "test_read_13"
  showTestResult test_read_14 "test_read_14"
  showTestResult test_read_15 "test_read_15"
  putStrLn ""

  putStrLn "tautology checker tests"
  showTestResult test_isTautology_00 "test_isTautology_00 - Principle of Excluded Middle"
  showTestResult test_isTautology_01 "test_isTautology_01 - Principle of Identity"
  showTestResult test_isTautology_02 "test_isTautology_02 - Principle of Contradiction"
  showTestResult test_isTautology_03 "test_isTautology_03 - Law of Double Negation"
  showTestResult test_isTautology_04 "test_isTautology_04 - Peirce's Law"
  showTestResult test_isTautology_05 "test_isTautology_05 - De Morgan's Law (And)"
  showTestResult test_isTautology_06 "test_isTautology_06 - De Morgan's Law (Or)"
  showTestResult test_isTautology_07 "test_isTautology_07 - Definition of Implication"
  showTestResult test_isTautology_08 "test_isTautology_08 - Definition of Equivalence"

  showTestResult test_notTautology_00 "test_notTautology_01 - Principle of Contradiction"
  showTestResult test_notTautology_01 "test_notTautology_02 - negation may be False"
  showTestResult test_notTautology_02 "test_notTautology_03 - conjunction may be False"
  showTestResult test_notTautology_03 "test_notTautology_04 - disjunction may be False"
  showTestResult test_notTautology_04 "test_notTautology_05 - implication may be False"
  showTestResult test_notTautology_05 "test_notTautology_06 - equivalence may be False"
  putStrLn ""

  -- property-based tests -
  -------------------------

  -- tests for showTestResult
  runPropertyTest prop_showTestResult_00 "test prop_showTestResult_00"

  -- tests for read
  runPropertyTest prop_read_00 "test prop_read_00: Not"
  runPropertyTest prop_read_01 "test prop_read_01: And"
  runPropertyTest prop_read_02 "test prop_read_02: Or"
  runPropertyTest prop_read_03 "test prop_read_03: Imply"
  runPropertyTest prop_read_04 "test prop_read_04: Eq"

-- END
