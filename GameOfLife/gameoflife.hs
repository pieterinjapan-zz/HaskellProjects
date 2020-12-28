{-
Pieter van Wyk
Created : 2019-06-14
Updated : 2019-06-24

game of life :

Artificial life implimentation in Haskell.
- The environment for a set of living cells is setup as a 2D grid, called “board”, with periodic boundary conditions. 
- The neighborhood of a site is its 8 nearest sites.
- A cell survives to the next generation if it has 2 or 3 living cells in its neighborhood.
- A new cell is born on an empty site if it has 3 living cells in its neighborhood.
-}

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- coordinate position of cell :
type Pos = (Int,Int)

-- board : list of positions containing a living cell :
type Board = [Pos]

-- input parameters :
---------------------

-- board size for game of life :
size   = 30   :: Int
width  = size :: Int
height = size :: Int

-- initial board setup :
glider :: Board
glider = [(1,1),(4,2),(2,3),(4,3),(3,4),(4,4)]

------------------------------------------------
--  Functions :
------------------------------------------------

-- pure functions :
-------------------

-- check if living cell is at given position on board :
isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

-- check if position on board is empty :
isEmpty :: Board -> Pos -> Bool
isEmpty b = not . ( isAlive b )

-- gives neighborhood of cell :
neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x  ,y-1),(x+1,y-1),
                          (x-1,y  ),(x+1,y  ),
                          (x-1,y+1),(x  ,y+1),(x+1,y+1)]

-- include periodic boundary conditions :
wrap :: Pos -> Pos
wrap (x,y) = ( 1 + mod (x-1) width , 1 + mod (y-1) height )

-- counts living neigbors of a position :
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- cells that survive to next generation :
survivors :: Board -> [Pos]
survivors b = filter ( \p -> elem ( liveneighbs b p ) [2,3] ) b

-- empty cells where new cells are formed :
births :: Board -> [Pos]
births brd = filter ( \p -> ( liveneighbs brd p ) == 3 ) (co_board brd)

-- list of empty cells near living cells :
co_board :: Board -> [Pos]
co_board board = ( rmdups . concat ) [ filter (isEmpty board) (neighbs b) | b <- board ]

-- removes duplicates from list :
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (l:ls) = l : filter ( /= l ) (rmdups ls)

-- create next generation of the board :
nextgen :: Board -> Board
nextgen brd = survivors brd ++ births brd

-- IO functions :
-----------------

-- clears screen :
cls :: IO ()
cls = putStr "\ESC[2J"

-- display string at a certain position on the console :
writeat :: Pos -> String -> IO ()
writeat p str = do goto p
                   putStr str

-- move to a given position on the console :
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- display given board :
showcells :: Board -> IO ()
showcells = sequence_ . map ( \p -> writeat p "O" )

-- implementing game of life :
life :: Board -> IO ()
life brd = do cls
              showcells brd
              wait 300000
              life (nextgen brd)

-- slow down calculation :
wait :: Int -> IO ()
wait n = sequence_ [ return () | _ <- [1..n] ]

------------------------------------------------
--  Input / Output :
------------------------------------------------

main :: IO ()
main = do
   life glider
