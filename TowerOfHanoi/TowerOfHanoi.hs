{-
Pieter van Wyk
2019-12-28

Tower-Of-Hanoi :
Solver for the tower of Hanoi puzzle 
implemented in Haskell.
(https://en.wikipedia.org/wiki/Tower_of_Hanoi)
-}

------------------------------------------------
-- Data Types and Instances :
------------------------------------------------

-- Int represents size of cube :
data Cube = Cube Int
  deriving (Show,Eq)

-- Char represents location of stack ( 'A', 'B' or 'C' )
data Stack = Stack Char [Cube]
 deriving (Show,Eq)

data Game = Game Stack Stack Stack
 deriving (Show,Eq)

------------------------------------------------
-- Functions :
------------------------------------------------

-- generate initial game state :
initGame :: Int -> Game
initGame n = Game sA sB sC
           where sA = initStack 'A' n
                 sB = initStack 'B' 0
                 sC = initStack 'C' 0

-- generate stack :
initStack :: Char -> Int -> Stack
initStack c 0 = Stack c []
initStack c n = Stack c (genCube n)

-- generate list of cubes for stack :
genCube :: Int -> [Cube]
genCube 0 = []
genCube n = genCube (n - 1) ++ [Cube n]

-- move cube from stack c1 to stack c2 :
move :: Game -> Char -> Char -> Game
move (Game sA sB sC) c1 c2 = case (c1,c2) of
  ('A','B') -> Game ( remCube sA ) ( addCube sA sB ) sC
  ('A','C') -> Game ( remCube sA ) sB ( addCube sA sC )
  ('B','A') -> Game ( addCube sB sA ) ( remCube sB ) sC
  ('B','C') -> Game sA ( remCube sB ) ( addCube sB sC )
  ('C','A') -> Game ( addCube sC sA ) sB ( remCube sC )
  ('C','B') -> Game sA ( addCube sC sB ) ( remCube sC )

-- remove cube from stack :
remCube :: Stack -> Stack
remCube (Stack c ls) = Stack c (drop 1 ls)

-- move cube from stack c1 to stack c2 :
addCube :: Stack -> Stack -> Stack
addCube (Stack _ []) s = s
addCube (Stack c1 (c:cs)) (Stack c2 cs') = Stack c2 (c:cs')

solveN :: Int -> Game -> Char -> Char -> Char -> Game
solveN n g a b c | n == 1 = move g a c
                 | otherwise = let gAC = move gAB a c
                                   gAB = solveN (n-1) g a c b
                               in solveN (n-1) gAC b a c

-- Functions for IO :
---------------------

stackChar :: Stack -> Char
stackChar (Stack c _) = c

showStack' :: Stack -> String
showStack' (Stack _ []) = ""
showStack' (Stack c (x:xs)) = showStack' (Stack c xs) ++ " " ++ (show n)
  where Cube n = x

showStack :: Stack -> String
showStack s = stackChar s : (" |" ++ showStack' s)

showGame :: Game -> IO()
showGame (Game sA sB sC) = do putStrLn (showStack sA)
                              putStrLn (showStack sB)
                              putStrLn (showStack sC)

solveN_IO :: Int -> Game -> Char -> Char -> Char -> IO()
solveN_IO n g a b c | n == 1 = do let g' = move g a c
                                  showGame g'
                                  putStrLn ""
                    | otherwise = do let gAB = solveN (n-1) g a c b
                                     solveN_IO (n-1) g a c b
                                     let gAC = move gAB a c
                                     showGame gAC
                                     putStrLn ""
                                     solveN_IO (n-1) gAC b a c

solveGame' :: Int -> Game -> Char -> Char -> Char -> IO()
solveGame' n g a b c = do putStrLn "Initial State :"
                          showGame g
                          putStrLn ""
                          solveN_IO n g a b c

solveGame :: Int -> IO()
solveGame n = solveGame' n g 'A' 'B' 'C'
            where g = initGame n

-- clears screen :
cls :: IO ()
cls = do putStrLn "\ESC[2H"
         putStrLn "\ESC[2J"

main :: IO()
main = do putStrLn "How many initial cubes?"
          n_temp <- getLine
          let n = read n_temp :: Int
          cls
          solveGame n
          putStrLn "Game Solved!"
