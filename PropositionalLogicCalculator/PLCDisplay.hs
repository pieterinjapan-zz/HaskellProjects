{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2021-01-08

Display functionality for propositional logic calculator
-}
module PLCDisplay where
import PLCEngine

------------------------------------------------
-- Instances :
------------------------------------------------

------------------------------------------------
-- Functions :
------------------------------------------------

-- top level display string
start_display = "\ESC[2J"                                  ++
                "--------------------------------------\n" ++
                "--- Propositional Logic Calculator ---\n" ++
                "--------------------------------------\n"

-- rules for proposition input
input_rule_display = "For proposition names use alphabetical characters and/or digits \n" ++
                     "ex) A, B, P1, Q2 \n\n"                                              ++
                     "For negation use prefix \"~\" (no space)\n"                         ++
                     "ex) negation of A is ~A \n\n"                                       ++
                     "For conjunction use infix \" and \" (with spaces)\n"                ++
                     "ex) conjunction of A, B is A and B \n\n"                            ++
                     "For disjunction use infix \" or \" (with spaces)\n"                 ++
                     "ex) disjunction of A, B is A or B \n\n"                             ++
                     "For implication use infix \" --> \" (with spaces)\n"                ++
                     "ex) implication of A, B is A --> B \n\n"                            ++
                     "For equivalence use infix \" <--> \" (with spaces)\n"               ++
                     "ex) equivalence of A, B is A <--> B \n\n"                           ++
                     "Use parentheses to indicate order of evaluation\n"                  ++
                     "ex) (A and B) --> C: conjunction evaluated first\n"                 ++
                     "    A and (B --> C): implication evaluated first\n\n"

-- prompt for getting input from command-line and calling appropriate logical operation
-- gives error message and restart prompt for incorrect input
choose_operation_display = " Please choose an operation:                     \n" ++
                           " - For tautology checker type 1                  \n" ++
                           " - To re-display rules for writing propositions r\n" ++
                           " - To exit program type q                        \n"

choose_operation_displayer :: IO ()
choose_operation_displayer = do
  putStr choose_operation_display
  str <- getLine
  case str of
    "1" -> isTautology_displayer
    "r" -> do putStrLn input_rule_display
              choose_operation_displayer
    "q" -> return ()
    _   -> do putStrLn "Incorrect Input!\n\n"
              choose_operation_displayer

-- TODO : add display for proposition evaluation

-- TODO : add display for equivalece checker

-- display for tautology checker
isTaautology_display_top = "-- Selected Tautology Checker -- \n" ++
                           "Please input proposition           "

isTautology_displayer = do
  putStrLn isTaautology_display_top
  putStrLn "example input: ~(A or B) <--> (~A and ~B)"
  prop_str <- getLine
  let result_str = isTautology_parser prop_str
  putStrLn result_str
  choose_operation_displayer

-- TODO : add display for truth table

-- END
