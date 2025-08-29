module Chess.Setup
  ( examplePuzzle
  , exampleBoard
  ) where

import Chess.Types
import Chess.Board

-- White: Kg1, Qh5, Rf1, Pawns g2 h2
-- Black: Kg8, Pawns f7 g7 h7
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
  where
    (.=) = (,)

exampleBoard :: Board
exampleBoard = place examplePuzzle
