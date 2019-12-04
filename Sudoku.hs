module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
import System.Random

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
  deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 r 
              where r = replicate 9 c 
                    c = Nothing


-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = length rows == 9 && all validRow rows

validRow :: [Cell] -> Bool
validRow cells = and [isNothing c || (c < Just 10 && c > Just 0) | c <- cells]

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = all isJust (concat rows)


------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku  = sequence_ (map (\r -> printRow r "") rs)
                      where rs = rows sudoku

printRow :: [Cell] -> String -> IO()
printRow [] s           = putStrLn s 
printRow (Just c:cs)  s = printRow cs ( s ++ [ intToDigit c ] )
printRow (Nothing:cs) s = printRow cs ( s ++ ".")


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do 
        contents <- readFile fp
        let rows       = lines contents
        let cellRows   = [createCells row | row <- rows]
        if isSudoku    (Sudoku cellRows) 
          then return  (Sudoku cellRows)
          else error   "Not a sudoku"


createCells :: [Char] -> [Cell]
createCells [] = []
createCells (c:cs) | c == '.'  = Nothing : createCells cs
                    | otherwise = Just (digitToInt c) : createCells cs



------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [ (9, elements [ Nothing ]) , (1, elements [ Just n | n <- [1..9] ]) ]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = 
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)
  -- hint: get to know the QuickCheck function vectorOf
  
-- * C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x:xs) 
  | isNothing x = isOkayBlock xs
  | otherwise   = notElem x xs && isOkayBlock xs


-- * D2

blocks :: Sudoku -> [Block]
blocks (Sudoku rows') = concat [rows' , transpose rows', createBlocks rows' ]

-- Helpers
createBlocks :: [Block] -> [Block]
createBlocks sud 
    | any null sud = []
    | otherwise = merge (map (take 3) sud) ++ createBlocks (map (drop 3) sud)

merge :: [Block] -> [Block]
merge blocks 
    | not (null blocks) =  concat (take 3 blocks): merge (drop 3 blocks)
    | otherwise = []

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length (blocks s) == 27 && all (\b -> length b == 9) (blocks s)


-- * D3

isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks sudoku = getBlankRows 0 rs
  where rs = rows sudoku

getBlankRows:: Int -> [Row] -> [Pos]
getBlankRows 9 _ = []
getBlankRows i (r:rs) = getBlankCells i 0 r ++ getBlankRows (i+1) rs


getBlankCells:: Int -> Int -> Row -> [(Int,Int)]
getBlankCells _ 9 _ = []
getBlankCells r i xs | isNothing (xs!!i) = (r,i) : getBlankCells r (i+1) xs
               | otherwise = getBlankCells r (i+1) xs

prop_blanks_allBlanks :: Sudoku -> Bool
prop_blanks_allBlanks s = all isNothing $ cells s $ blanks s

cells :: Sudoku -> [Pos] -> [Cell]
cells s (x:xs) | null xs = [getPosition x s]
               | otherwise = getPosition x s : cells s xs

getPosition:: Pos -> Sudoku -> Cell
getPosition (i,j) s |i>=0 && i<9 && j >=0 && j<9 = (rows s !! i)  !! j
                    |otherwise = error "Out of bounds"
 -- where row = rows s !! i  


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
[]     !!=  _     = []
(x:xs) !!= (i,y) | i<0 || i >= length (x:xs) = x:xs
                 | i==0 = y:xs
                 | otherwise = x:xs !!= (i-1 ,y)



prop_bangBangEquals_correct:: [Int] -> Int -> Bool
prop_bangBangEquals_correct [] _ = True
prop_bangBangEquals_correct as j = do 
                            let g = mkStdGen 6
                            let (i, g2) = randomR (0, length as -1) g
                            let newas = as !!= (i,j) 
                            let cutas = take i newas ++ drop (i + 1) newas
                            as `union` cutas == as


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku s) (i,j) n | i>=0 && i<9 && j >=0 && j<9 = Sudoku (s !!= (i, (s!!i) !!= (j,n) ))
                          | otherwise = error "Out of bounds"
  --where row = s!!i
    --    newrow = row !!= (j,n)

prop_update_updated :: Sudoku -> Cell -> Bool
prop_update_updated s n = do
            let g = mkStdGen 6
            let (i, g2) = randomR (0, 8) g
            let (j, g3) = randomR (0, 8) g2
            let newSudoku = update s (i,j) n
            getPosition (i,j) newSudoku == n
         

------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve s | solutions /= [] = Just $ head solutions
        | otherwise = Nothing
        where solutions = solve' s

solve' :: Sudoku -> [Sudoku]
solve' s | null $ blanks s = [s] 
         | otherwise = concatMap solve' possibleBoards
        where b:bs = blanks s
              possibleBoards = possibleMoves b s

possibleMoves :: Pos -> Sudoku -> [Sudoku]
possibleMoves pos s = filter isOkay [update s pos (Just n) | n <- range]
    where range = [1..9]


-- * F2

readAndSolve :: FilePath -> IO()
readAndSolve fp = do 
                   s <- readSudoku fp
                   maybe (putStrLn "Has no solution") printSudoku (solve s)

-- * F3

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = isOkay s1 && isFilled s1
                    &&  putAllBlank (blanks s2) s1 == s2

putAllBlank :: [Pos] -> Sudoku -> Sudoku
putAllBlank []     s = s
putAllBlank (p:ps) s = putAllBlank ps (update s p Nothing)

-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isSudoku s && isOkay s && isJust (solve s) ==> fromJust (solve s) `isSolutionOf` s