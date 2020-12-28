{-
Pieter van Wyk
Created : 2016-04-16
Updated : 2019-06-24

Starman :        
a Simple text based game written in Haskell for guessing a hidden word,
showing basic use of IO in Haskell.

Uses a random number to pick a hidden word from a dictionary.
The player enters characters to try and guess the hidden word.
The player is allowed 5 mistakes.       
-}
import System.Random

------------------------------------------------
-- Functions :
------------------------------------------------

-- pure functions :
-------------------

-- dictionary for starman game :
dic = ["star","burns","twice","bright","half","long"]

-- checking if guessed character is in the hidden word :
check :: String -> String -> Char -> ( Bool , String )
check word display ch = ( b, display' )
                      where b = elem ch word
                            display' = [ if ch == x
                              then x
                              else y | (x,y) <- zip word display ]

-- determining if there is another turn left to guess :
turn :: String -> String -> Int -> IO()
turn word display n | n == 0          = putStrLn "You lose!"
                    | word == display = putStrLn "You Win!"
                    | otherwise       = mkguess word display n

-- allowing player to guess next character :
mkguess :: String -> String -> Int -> IO()
mkguess word display n = do
                          putStrLn (display ++ " " ++ take n (repeat '*'))
                          putStr " Enter your guess: "
                          q <- getLine
                          let (correct, display') = check word display (q!!0)
                          let n' = if correct then n else (n-1)
                          turn word display' n'
                          
-- IO functions :
-----------------    

-- main function of starman game :
starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] n
                          
------------------------------------------------
-- Input / Output :
------------------------------------------------                          

main :: IO ()
main = do
    n_ran' <- randomIO :: IO Int
    let n_ran = abs( mod n_ran' 7 )
    let word = dic!!n_ran
    starman word 5
