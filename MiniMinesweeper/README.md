# MiniMinesweeper
Choice game written in Haskell, based on minesweeper.

The goal of the game is to sweep a 1D grid, and not touch the mine. 
The grid has 6 points, and the position of the mine is randomized.
Each turn the player guesses a point on the grid that is safe.
If the point is safe it turns to an O.

If it’s not, it turns to an X and the game is over.
The player wins by clearing all the safe points.
