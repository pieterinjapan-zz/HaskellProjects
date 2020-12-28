{-
Pieter van Wyk
Created : 2019-06-20
Updated : 2019-06-24

Nim :
2 player board game implemented in Haskell.
- game board is given as 5 rows of stars
- player is represented by an integer ( 1 or 2 )
- players take turns removing stars from a row of choice
- game is won by the player that clears the board
-}
import Data.Char

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- datatype for game board :
type Board = [Int]

initial :: Board -- initial game board :
initial = [5,4,3,2,1]

------------------------------------------------
--  Functions :
------------------------------------------------

--  Pure Functions :
--------------------

-- determining if the game is over :
finished :: Board -> Bool
finished = all (== 0)

-- choosing next player :
next :: Int -> Int
next 1 = 2
next 2 = 1

-- is chosen move valid :
valid :: Board -> Int -> Int -> Bool
valid board row num = board!!(row-1) >= num

-- execute valid move on the board :
move :: Board -> Int -> Int -> Board
move board row num = foldr ((:).update) [] (zip [1..] board)
                   where
                     update (r,n) = if (r == row) then (n-num) else n

--  IO Functions :
------------------

-- display given number of stars in given row of the board :
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn $ concat ( take num $ repeat "* " )

-- display game board :
putBoard' :: [(Int,Int)] -> IO ()
putBoard' []     = return ()
putBoard' (p:ps) = do putRow (fst p) (snd p)
                      putBoard' ps
putBoard :: Board -> IO ()
putBoard board = putBoard' (zip [1..] board)

-- prompt the input of a digit and check if valid :
getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     d <- getChar
                     getChar
                     if isDigit d then
                        return (digitToInt d)
                     else
                        do putStrLn "ERROR: Invalid Digit!"
                           getDigit prompt

-- main game function :
play :: Board -> Int -> IO ()
play board player =
  do putChar '\n'
     putBoard board -- display curent gameboard
     if finished board then -- end case
       do putChar '\n'
          putStr "Player "
          putStr (show (next player)) -- if the board is empty the other player won
          putStrLn " wins!!"
     else -- continue case
       do putChar '\n'
          putStr "Player "
          putStrLn (show player)
          row <- getDigit "Enter a row number: "
          num <- getDigit "Stars to remove: "
          if valid board row num then -- check validity of move
            play (move board row num) (next player) -- play with modified board
          else
            do putChar '\n'
               putStrLn "ERROR: Invalid Move!"
               play board player

------------------------------------------------
--  Input / Output :
------------------------------------------------

main :: IO ()
main = play initial 1
