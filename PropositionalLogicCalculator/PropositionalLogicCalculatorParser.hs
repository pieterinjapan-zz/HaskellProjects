{-
Pieter van Wyk
Created : 2019-12-29
Updated : 2020-12-29

Parser for propositional logic calculator.
Functionality for mapping Prop into String,
and mapping string representing proposition
into Prop.
-}
module PropositionalLogicCalculatorParser where
import PropositionalLogicCalculator

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- making Prop an intance of Show
instance Show Prop where
  show prop = case prop of
    Const b -> show b
    Var x   -> [x]
    Not p   -> case p of
      Const b -> show (not b)
      _       -> "~" ++ show p --"(~" ++ show p ++ ")"
    And p q -> showBinary p q " and "
    Or p q  -> showBinary p q " or "
    Imply p q -> showBinary p q " --> "
    Eq  p q -> showBinary p q " <--> "
    where showBinary p q str = "(" ++ show p ++ str ++ show q ++ ")"

-- TODO : add read functionality, so proposition can be input as a String (String -> Prop)

-- tree datastructure used for parsing String into Prop
data Tree a = Leaf a | Node1 a (Tree a) | Node2 a (Tree a) (Tree a)
 deriving (Show, Eq)

------------------------------------------------
-- Functions :
------------------------------------------------

-- i) parse string into tree

-- parse string representing proposition into Tree
parseTree :: String -> Tree String
parseTree str | len == 1 = let h_str_s = head str_s
                           in case h_str_s of
                             '~':s -> Node1 "~" (parseTree s)
                             _     -> Leaf h_str_s
              | len == 2 = Node1 "~" (parseTree (str_s!!1))
              | otherwise = let [prop_L,conector,prop_R] = str_s
                            in Node2 conector (parseTree prop_L) (parseTree prop_R)
  where str_s = splitProp str
        len = length str_s

-- helper function for parsing proposition
splitProp :: String -> [String]
splitProp "" = []
splitProp str = aux [] "" str_striped
  where  str_striped = if str!!0 == '(' then tail $ init str else str
         aux acc word "" = if word == "" then acc else acc ++ [word]
         aux acc word (c:str) | c == ' ' = if word == ""
                                           then aux acc "" str
                                           else aux (acc++[word]) "" str
                              | c == '(' = let (word',str') = cutParentheses (c:str)
                                           in if word == ""
                                              then aux (acc++[word']) "" str'
                                              else aux (acc++[word++word']) "" str'
                              | otherwise = aux acc (word ++ [c]) str

-- helper function for parsing proposition
cutParentheses :: String -> (String,String)
cutPatentheses "" = ("","")
cutParentheses (_:str) = aux 1 "(" str
  where aux 0 acc str = (acc,str)
        aux _ acc ""  = (acc,"")
        aux counter acc (c:str) = let acc' = acc ++ [c]
                                  in case c of
                                    '(' -> aux (counter + 1) acc' str
                                    ')' -> aux (counter - 1) acc' str
                                    _   -> aux counter acc' str

-- ii) map tree into proposition

-- END
