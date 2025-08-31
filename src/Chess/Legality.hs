module Chess.Legality
  ( inCheck
  , legalMoves
  , isCheckmate
  , isStalemate
  ) where

import Chess.Types
import Chess.Board
import Chess.Game
import Chess.Move
import Chess.MoveGen (pseudoMoves, attacksFrom)
import Chess.Apply   (applyMoveRules)
import Data.Maybe    (listToMaybe)

-- | Locate the king for a color.
kingSquare :: Board -> Color -> Maybe Square
kingSquare bd side =
  listToMaybe
    [ sq
    | i <- [0..63]
    , let sq = Square i
    , Just (Piece c King) <- [pieceAt bd sq]
    , c == side
    ]

-- | Is 'sq' attacked by 'by'?
squareAttackedBy :: Board -> Color -> Square -> Bool
squareAttackedBy bd by sqTarget =
  or [ sqTarget `elem` attacksFrom gDummy sq
     | i <- [0..63]
     , let sq = Square i
     , Just (Piece c _) <- [pieceAt bd sq]
     , c == by
     ]
  where
    -- attacksFrom only reads the board and the piece color at sq,
    -- so we can use a minimal Game wrapper.
    gDummy = startGame bd White

-- | Is 'side' currently in check?
inCheck :: Game -> Color -> Bool
inCheck g side =
  case kingSquare (board g) side of
    Nothing     -> False  -- malformed position; be permissive
    Just kSquare ->
      squareAttackedBy (board g) (switchSide side) kSquare

-- | Extra castling requirements: not in check and the king cannot pass through check.
castleTransitSquares :: Color -> CastleSide -> (Square, [Square], Square)
castleTransitSquares White KingSide = (at 'e' 1, [at 'f' 1], at 'g' 1)
castleTransitSquares White QueenSide = (at 'e' 1, [at 'd' 1], at 'c' 1)
castleTransitSquares Black KingSide = (at 'e' 8, [at 'f' 8], at 'g' 8)
castleTransitSquares Black QueenSide = (at 'e' 8, [at 'd' 8], at 'c' 8)

-- | Legal moves = pseudoMoves filtered by "king not in check after the move".
--   Additionally, for castling we must ensure start/transit/destination squares are not attacked.
legalMoves :: Game -> [Move]
legalMoves g =
  [ m
  | m <- pseudoMoves g
  , isCastleOK g m
  , case applyMoveRules g m of
      Left _   -> False
      Right g' -> not (inCheck g' (switchSide (toMove g)))
  ]
  where
    isCastleOK :: Game -> Move -> Bool
    isCastleOK g0 (Move _ to _ (Just sideC)) =
      let side = toMove g0
          (start, midSquares, dest) = castleTransitSquares side sideC
          bd = board g0
          opp = switchSide side
      in  -- Start square must match the king:
          start == (case side of White -> at 'e' 1; Black -> at 'e' 8)
          && to == dest
          && not (inCheck g0 side)                               -- cannot castle out of check
          && all (not . squareAttackedBy bd opp) (start : midSquares ++ [dest])
    isCastleOK _ _ = True

-- | Mate iff: side to move is in check AND has zero legal moves.
isCheckmate :: Game -> Bool
isCheckmate g = inCheck g (toMove g) && null (legalMoves g)

-- | Stalemate iff: side to move is NOT in check AND has zero legal moves.
isStalemate :: Game -> Bool
isStalemate g = not (inCheck g (toMove g)) && null (legalMoves g)
