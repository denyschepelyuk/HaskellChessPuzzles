module Chess.Legality
  ( inCheck
  , legalMoves
  , isCheckmate
  , isStalemate
  ) where

import Data.Maybe (listToMaybe)
import Chess.Types
import Chess.Board
import Chess.Game
import Chess.Move        (Move(..), CastleSide(..))
import Chess.MoveGen     (pseudoMoves, attacksFrom)
import Chess.Apply       (applyMoveRules)

-- Locate the king of a color on the board
kingSquare :: Board -> Color -> Maybe Square
kingSquare bd side =
  listToMaybe
    [ sq
    | i <- [0..63]
    , let sq = Square i
    , Just (Piece c King) <- [pieceAt bd sq]
    , c == side
    ]

-- Is target square attacked by 'by' color?
squareAttackedBy :: Board -> Color -> Square -> Bool
squareAttackedBy bd by target =
  or [ target `elem` attacksFrom gDummy sq
     | i <- [0..63]
     , let sq = Square i
     , Just (Piece c _) <- [pieceAt bd sq]
     , c == by
     ]
  where
    -- attacksFrom only needs the board; other Game fields don't matter here
    gDummy = startGame bd White

-- Is 'side' currently in check?
inCheck :: Game -> Color -> Bool
inCheck g side =
  case kingSquare (board g) side of
    Nothing  -> False
    Just kSq -> squareAttackedBy (board g) (switchSide side) kSq

-- Forbid moves that "capture the king" (not a legal chess move)
capturesKing :: Game -> Move -> Bool
capturesKing g m =
  case pieceAt (board g) (to m) of
    Just (Piece _ King) -> True
    _                   -> False

-- Extra castling requirements: start/transit/destination not attacked
castleTransitSquares :: Color -> CastleSide -> (Square, [Square], Square)
castleTransitSquares White KingSide  = (at 'e' 1, [at 'f' 1], at 'g' 1)
castleTransitSquares White QueenSide = (at 'e' 1, [at 'd' 1], at 'c' 1)
castleTransitSquares Black KingSide  = (at 'e' 8, [at 'f' 8], at 'g' 8)
castleTransitSquares Black QueenSide = (at 'e' 8, [at 'd' 8], at 'c' 8)

-- Legal moves: pseudo-moves filtered by rules + "our king not in check after move".
legalMoves :: Game -> [Move]
legalMoves g =
  [ m
  | m <- pseudoMoves g
  , not (capturesKing g m)               -- forbid capturing the king outright
  , isCastleOK g m
  , case applyMoveRules g m of
      Left _   -> False
      Right g' -> not (inCheck g' (toMove g))
  ]
  where
    isCastleOK :: Game -> Move -> Bool
    isCastleOK g0 (Move _ dest _ (Just sideC)) =
      let side = toMove g0
          (start, mids, target) = castleTransitSquares side sideC
          bd  = board g0
          opp = switchSide side
          path = start : (mids ++ [target])
      in  dest == target
       && not (inCheck g0 side)                    -- can’t castle out of check
       && all (not . squareAttackedBy bd opp) path -- can’t pass/land through check
    isCastleOK _ _ = True

-- Mate iff: side to move is in check AND has zero legal moves.
isCheckmate :: Game -> Bool
isCheckmate g = inCheck g (toMove g) && null (legalMoves g)

-- Stalemate iff: side to move NOT in check AND zero legal moves.
isStalemate :: Game -> Bool
isStalemate g = not (inCheck g (toMove g)) && null (legalMoves g)
