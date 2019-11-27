module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List

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
isSudoku (Sudoku rows) = length rows == 9 && and [validRow row | row <- rows]

validRow :: [Cell] -> Bool
validRow cells = and [c == Nothing || (c < Just 10 && c > Just 0) | c <- cells]

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = and [ and [ cell /= Nothing | cell <- row] | row <- rows]


------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku  = sequence_ [printRow r "" | r <- rs]
                      where rs = rows sudoku

printRow :: [Cell] -> String -> IO()
printRow [] s           = putStrLn    ( s ++ "\n" )
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


createCells :: [Char]-> [Cell]
createCells [] = []
createCells (c:cs) | c == '.'  = [ Nothing ]             ++ createCells cs
                    | otherwise = [ Just (digitToInt c) ] ++ createCells cs



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
  | x == Nothing = isOkayBlock xs
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
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
