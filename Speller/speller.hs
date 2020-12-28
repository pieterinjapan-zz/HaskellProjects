{-
Pieter van Wyk
Created : 2019-04-17
Updated : 2019-06-24

Speller :
Simple program, making use of a main function "speller", 
for converting a list of words into a spelling dictionary.

For example :
speller ["abacus"] -- > "a is for abacus"
speller [] -- > ""
speller ["apple", "banana", "coconut"] -- > "a is for apple, b is for banana, and c is for coconut"
speller ["whisky", "x-ray"] -- > "w is for whisky, and x is for x-ray"
-}

------------------------------------------------
-- Functions :
------------------------------------------------

-- one word case :
{- function for converting single word
   into spelling dictionary format.
   - base case maps the empty list to "".
   - iterative case takes the first character
     of the word, ads " is for ", and then adds
     the word.
   ex) "person" --> "p is for person" -}
spellDic :: String -> String
spellDic [] = ""
spellDic (ch:wd) = [ch] ++ " is for " ++ ch:wd

-- multiple word case (main function) :
{- function for converting multiple words
   into spelling dictionary format.
   - base case 1 maps the empty list to "".
   - base case 2 maps single word spelling
     dictionary format (no and).
   - iterative case converts the word list
     into spelling dictionary format, placing
     and at the end of the final phrase.
   ex) ["hello","person"] --> "h is for hello, and p is for person."
   here fp_r, fp_l or fp_c (defined bellow),
   should be used for fp.
-}
speller :: ([String] -> String) -> [String] -> String
speller fp []   = "" -- base case 1
speller fp [wd] = case wd of
  "" -> ""
  _  -> spellDic wd ++ "." -- base case 2
speller fp wds  = (fp . spell_lst) wds ++ "and " ++ ( last . spell_lst ) wds ++ "."
               where
                 spell_lst  = map spellDic

--(1) using foldr :
fp_r :: [String] -> String
fp_r = foldr (\wd acc -> wd ++ ", " ++ acc ) "" . init

--(2) using foldl :
fp_l :: [String] -> String
fp_l = foldl (\acc wd -> acc ++ wd ++ ", " ) "" . init

--(3) using concat :
fp_c :: [String] -> String
fp_c = concat . map (\wd -> wd ++ ", " ) . init

------------------------------------------------
-- Input / Output :
------------------------------------------------

main :: IO()
main = do

  -- test word lists :
  let wds1 = ["hello","world"]
  let wds2 = ["blue","green","pink"]
  let wds3 = ["star","burns","bright"]

  putStrLn "Test of speller using foldr :"
  let dic1 = speller fp_r wds1
  putStrLn (show dic1)
  putStrLn ""

  putStrLn "Test of speller using foldl :"
  let dic2 = speller fp_l wds2
  putStrLn (show dic2)
  putStrLn ""

  putStrLn "Test of speller using concat :"
  let dic3 = speller fp_c wds3
  putStrLn (show dic3)
  putStrLn ""
