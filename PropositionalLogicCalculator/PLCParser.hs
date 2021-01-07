{-
Pieter van Wyk
Created : 2019-12-29
Updated : 2021-01-06

Parser for propositional logic calculator.
Functionality for mapping Prop into String,
and mapping String representing proposition
into Prop.
-}
module PLCParser where
import PLCData
import Data.Char

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- making Prop an intance of Show
instance Show Prop where
  show prop = stripOuterParentheses $ show' prop
    where showBinary p q str = "(" ++ show' p ++ str ++ show' q ++ ")"
          show' prop = case prop of
            Const b -> show b
            Var x   -> x -- map toUpper x
            Not p   -> case p of
              Const b -> show (not b)
              _       -> "~" ++ show' p
            And p q -> showBinary p q " and "
            Or p q  -> showBinary p q " or "
            Imply p q -> showBinary p q " --> "
            Eq  p q -> showBinary p q " <--> "

-- making Prop an intance of Read
instance Read Prop where
  readsPrec _ = readProp
    where readProp prop_str = [(treeToProp $ parseTree prop_str,"")]

------------------------------------------------
-- Functions :
------------------------------------------------

-- i) parse string into tree

-- helper function for stripping of redundant outer parentheses
stripOuterParentheses :: String -> String
stripOuterParentheses [] = []
stripOuterParentheses [x] = [x]
stripOuterParentheses str | head str == '(' && last str == ')' = if aux 0 striped_str
                                                                 then striped_str
                                                                 else str
                          | otherwise = str
  where striped_str = tail $ init str
        aux _ [] = True
        aux n (c:str) | n < 0 = False
                      | c == '(' = aux (n + 1) str
                      | c == ')' = aux (n - 1) str
                      | otherwise = aux n str

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
  where  str_striped = stripOuterParentheses str -- if str!!0 == '(' then tail $ init str else str
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
treeToProp :: Tree String -> Prop
treeToProp (Leaf a) = case a of
  "True"  -> Const True
  "False" -> Const False
  _       -> Var a -- (map toUpper a)
treeToProp (Node1 neg tree) = Not (treeToProp tree)
treeToProp (Node2 connector treeL treeR) = case connector of
  "and"  -> And (treeToProp treeL) (treeToProp treeR)
  "or"   -> Or (treeToProp treeL) (treeToProp treeR)
  "-->"  -> Imply (treeToProp treeL) (treeToProp treeR)
  "<-->" -> Eq (treeToProp treeL) (treeToProp treeR)
  _      -> error "incorrect representation for proposition"

-- END
