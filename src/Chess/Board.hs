module Chess.Board
  ( Board
  , empty
  , place
  , set
  , pieceAt
  , prettyBoard
  ) where

import           Chess.Types
import           Data.List      (foldl')
import           Data.Vector    (Vector, (!))
import qualified Data.Vector as V

-- Board as a 64-slot vector (a1=0 .. h8=63)
newtype Board = Board { unBoard :: Vector (Maybe Piece) }
  deriving (Eq)

-- Start from an empty board
empty :: Board
empty = Board (V.replicate 64 Nothing)

-- Place a single piece
set :: Board -> Square -> Piece -> Board
set (Board v) (Square i) p = Board (v V.// [(i, Just p)])

-- Bulk placement
place :: [(Square, Piece)] -> Board
place = foldl' (\bd (sq, pc) -> set bd sq pc) empty

-- Read a square
pieceAt :: Board -> Square -> Maybe Piece
pieceAt (Board v) (Square i) = v ! i

-- Human-friendly ASCII rendering (ranks 8..1)
prettyBoard :: Board -> String
prettyBoard (Board v) = unlines $ header : rows ++ [footer]
  where
    header = "    a   b   c   d   e   f   g   h"
    footer = header
    rows   = [ row r | r <- [7,6..0] ]
    row r  = show (r+1) ++ " | " ++ cells r ++ " | " ++ show (r+1)
    cells r = concat [ cell (r*8 + f) ++ if f < 7 then " " else "" | f <- [0..7] ]
    cell i = case v ! i of
      Nothing -> ".  "
      Just p  -> [renderPieceChar p] ++ "  "
