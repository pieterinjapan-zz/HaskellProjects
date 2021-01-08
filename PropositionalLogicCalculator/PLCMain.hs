{-
Pieter van Wyk
Created : 2019-05-24
Updated : 2021-01-08

Main function for propositional logic calculator
-}
module PLCMain where
import PLCDisplay

-- TODO : impliment main function
main = do
  putStr start_display
  putStr input_rule_display
  choose_operation_displayer

-- END
