{-
Pieter van Wyk
2019-06-21

Tic-Tac-Toe :

Game of Tic-Tac-Toe between two human players
implemented in Haskell.
-}
import Data.Char

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- input parameters :
---------------------

-- game board size :
size = 3 :: Int

-- type for gameboard (each inner list must have length = size ) :
type Grid = [[Player]]

-- possible player states ( B represents the blank state ) :
data Player = O | B | X
            deriving(Show,Eq,Ord)

------------------------------------------------
--  Functions :
------------------------------------------------

test_grd1 = [ [X,X,O],
              [X,B,X],
              [O,O,B] ] :: Grid
test_grd2 = [ [X,O,O],
              [X,O,X],
              [B,O,X] ] :: Grid
test_grd3 = [ [X,X,B],
              [X,X,X],
              [O,O,X] ] :: Grid

--  Pure Functions :
--------------------

-- generate empty grid :
empty :: Grid
empty = replicate size (replicate size B)

-- count number of given player state on a grid :
count_ply :: Player -> Grid -> Int
count_ply ply = length . filter (==ply) . concat

-- check is board is full (none of the player states are B) :
full :: Grid -> Bool
full = (==0) . count_ply B

-- turn of next player :
turn :: Grid -> Player
turn grd | o_num <= x_num = O
         | otherwise      = X
         where
           x_num = count_ply X grd
           o_num = count_ply O grd

-- deciding if a player has won the game :
wins :: Player -> Grid -> Bool
wins plr grd = foldr ((||).(\win_f -> win_f plr grd)) False [wins_hor,wins_ver,wins_diag]
             where
               wins_hor plr      = or . map ( \pl -> and $ map (==plr) pl ) -- horizontal case
               wins_ver plr grd  = wins_hor plr (trans grd size)            -- vertical case
               wins_diag plr grd = wins_hor plr [diag grd,offdiag grd]      -- diagonal case

-- transpose grid :
trans :: Grid -> Int -> Grid 
trans grd 0 = []
trans grd n = ( map (\pl -> pl!!(n-1) ) grd ) : ( trans grd (n-1) )

-- get off diagonals of grid :
offdiag :: Grid -> [Player] 
offdiag []       = []
offdiag (pl:grd) = pl!!(n_grd - 1) : (offdiag grd)
                 where
                   n_grd = length (pl:grd)

-- get diagonals of grid :
diag :: Grid -> [Player] 
diag = offdiag . map reverse

-- display list of Players as a list of Strings :
showRow :: [Player] -> [String]
showRow pls = ( beside . interleave bar_ver . map showPlayer ) pls
            where
              beside  = foldr1 ( zipWith (++) )
              bar_ver = replicate size "|"

-- display player as a list of strings (making B a blank space) :
showPlayer :: Player -> [String]
showPlayer pl = [ "   ", showPl pl, "   " ]
              where
                showPl pl = " " ++ ( if (pl == B) then " " else show pl ) ++ " "

-- insert new element inbetween each element of a list :
interleave :: Eq a => a -> [a] -> [a]
interleave x ys = foldr ( \y vs -> if ( vs == [] ) then [y] else (y : x : vs) ) [] ys

-- check if a move is valid according to the enumeration of the grid :
--  - move must be an index of the grid
--  - index of the grid for given move must be empty
valid :: Grid -> Int -> Bool
valid grd i = ( 0 <= i && i <= size^2 ) && ( ( concat grd )!!i == B )

-- generate new grid after valid move :
move :: Grid -> Int -> Player -> [Grid]
move grd i pl | valid grd i = [ chop size ( xs ++ (pl:ys) ) ]
              | otherwise   = []
              where (xs,B:ys) = splitAt i ( concat grd ) -- if the move is valid there should be a B at position i

-- breaks list into maximal segments of a given length :
chop :: Int -> [a] -> [[a]]
chop n ls = foldl chop_f [[]] ls
          where
            chop_f acc_lss l | length ( last acc_lss ) >= n = acc_lss ++ [[l]]
                             | otherwise = ( init acc_lss ) ++ [ ( last acc_lss ) ++ [l] ]

-- prompts player to enter a move :
prompt :: Player -> String
prompt pl = "Player " ++ show pl ++ ", enter your move: "

-- move to next player :
next :: Player -> Player
next X = O
next O = X
next B = B

--  IO Functions :
------------------

-- clears screen :
cls :: IO ()
cls = putStr "\ESC[2J"

-- move to a given position on the console :
goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- display the game grid :
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar_hor . map showRow
        where
          bar_hor = [ replicate ( 4*size - 1 ) '-' ]

-- prompt the input of a digit check :
getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs :: Int)
                   else
                     do putStrLn "ERROR: Invalid Number!"
                        getNat prompt

-- display current grid, and invoke next round :
run :: Grid -> Player -> IO ()
run grd pl = do cls
                goto (1,1)
                putGrid grd
                run' grd pl

-- executes one round of tictactoe :
run' :: Grid -> Player -> IO ()
run' grd pl | wins O grd = putStrLn "Player O wins!\n"
            | wins X grd = putStrLn "Player X wins!\n"
            | full grd   = putStrLn "It's a draw!\n"
            | otherwise  =
              do i <- getNat (prompt pl)
                 case move grd i pl of
                   []     -> do putStrLn "ERROR: Invalid move!"
                                run' grd pl
                   [grd'] -> run grd' (next pl)

-- main game function :
tictactoe :: IO ()
tictactoe = run empty O

------------------------------------------------
--  Input / Output :
------------------------------------------------

main :: IO ()
main = do tictactoe
