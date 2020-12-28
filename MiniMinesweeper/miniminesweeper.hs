{-
Pieter van Wyk
Created : 2019-04-16
Updated : 2019-06-24

Mini Minesweeper :

Choice game written in Haskell, based on minesweeper.

The goal of the game is to sweep a 1D grid, and not touch the mine. 
The grid has 6 points, and the position of the mine is randomized.
Each turn the player guesses a point on the grid that is safe.
If the point is safe it turns to an O.

If it’s not, it turns to an X and the game is over.
The player wins by clearing all the safe points.
-}
import System.Random

------------------------------------------------
-- Functions :
------------------------------------------------

-- Pure Functions :
-------------------

-- setup the game grid :
setupGrid :: Int -> String
setupGrid n = take n (repeat '*')

-- change grid after choice :
newgrid :: String -> Int -> Int -> String
newgrid grid mine_pt point | point == mine_pt = newgrid' grid point 'X'
                           | otherwise        = newgrid' grid point 'O'
newgrid' :: String -> Int -> Char -> String
newgrid' grid point ch = [ if i == point then ch else g | (i,g) <- zip [0..] grid ]

-- calculate score (number of 'O's in grid) :
score :: String -> Int
score grid = sum [ 1 | x <- grid, x == 'O' ]

-- IO Functions :
-----------------

-- check if player has another turn :
turn :: String -> Int -> Int -> IO()
turn grid mine_pt point | point == mine_pt              = putStrLn "Boom!!!"
                        | score grid == length grid - 1 = putStrLn "You Win!"
                        | otherwise                     = mkguess grid mine_pt

-- allowing player to guess next grid point :
mkguess :: String -> Int -> IO()
mkguess grid mine_pt = do
                              putStr " Enter your guess : "
                              new_point' <- getLine
                              let new_point = read new_point' :: Int
                              let new_grid  = newgrid grid mine_pt new_point
                              putStrLn ( "grid : " ++ new_grid )
                              turn new_grid mine_pt new_point

------------------------------------------------
-- Input / Output :
------------------------------------------------

main :: IO()
main = do

    let size = 6 -- Grid Size

    -- Random Point for Mine :
    n_ran' <- randomIO :: IO Int
    let n_ran = abs( mod n_ran' (size + 1) )

    -- Initialization :
    let grid    = setupGrid 6 --Setup Grid
    let mine_pt = n_ran       --Mine Point
    let score   = 0           --Initial Score

    -- Show Initial Grid :
    putStrLn ( "grid : " ++ grid )

    -- Begin Mini-Minesweeper Game :
    mkguess grid mine_pt
