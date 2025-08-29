module Chess.Gen
  ( kqkBox
  , rookNet
  ) where

import Chess.Types
import Chess.Board

infix 1 .=
(.=) :: Square -> Piece -> (Square, Piece)
(.=) = (,)

-- | King+Queen vs King in a simple "box" configuration.
kqkBox :: Board
kqkBox = place
  [ at 'g' 1 .= w King
  , at 'e' 4 .= w Queen
  , at 'a' 8 .= b King
  ]

-- | Rook net around a cornered black king (not verified as mate).
rookNet :: Board
rookNet = place
  [ at 'h' 1 .= w King
  , at 'h' 5 .= w Rook
  , at 'g' 5 .= w King
  , at 'h' 8 .= b King
  ]
