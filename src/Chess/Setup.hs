module Chess.Setup
  ( examplePuzzle
  , exampleBoard
  ) where

import Chess.Types
import Chess.Board
import Chess.Gen (kqkBox)

-- Keep an explicit puzzle list for reference
examplePuzzle :: [(Square, Piece)]
examplePuzzle =
  [ at 'g' 1 .= w King
  , at 'h' 5 .= w Queen
  , at 'f' 1 .= w Rook
  , at 'g' 2 .= w Pawn
  , at 'h' 2 .= w Pawn
  , at 'g' 8 .= b King
  , at 'f' 7 .= b Pawn
  , at 'g' 7 .= b Pawn
  , at 'h' 7 .= b Pawn
  ]
  where (.=) = (,)

-- Use a generated board for the running demo
exampleBoard :: Board
exampleBoard = kqkBox
