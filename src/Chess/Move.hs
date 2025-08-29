module Chess.Move
  ( Move(..)
  , CastleSide(..)
  , uciToMove
  , uciOfMove
  ) where

import Chess.Types
import Data.Char (isDigit)

data CastleSide = KingSide | QueenSide
  deriving (Eq, Show)

-- For now: castle field unused; promo covers e7e8Q style.
data Move = Move
  { from   :: Square
  , to     :: Square
  , promo  :: Maybe PieceType
  , castle :: Maybe CastleSide
  } deriving (Eq, Show)

uciToMove :: String -> Maybe Move
uciToMove [f1,r1,f2,r2]     = mk f1 r1 f2 r2 Nothing
uciToMove [f1,r1,f2,r2,pch] = do
  m <- mk f1 r1 f2 r2 Nothing
  pt <- charToPromo pch
  pure m { promo = Just pt }
uciToMove _ = Nothing

mk :: Char -> Char -> Char -> Char -> Maybe CastleSide -> Maybe Move
mk f1 r1 f2 r2 c
  | all validFile [f1,f2] && all isDigit [r1,r2] =
      let s1 = at f1 (d2i r1)
          s2 = at f2 (d2i r2)
      in Just (Move s1 s2 Nothing c)
  | otherwise = Nothing
  where
    validFile ch = ch >= 'a' && ch <= 'h'
    d2i ch = fromEnum ch - fromEnum '0'

charToPromo :: Char -> Maybe PieceType
charToPromo 'q' = Just Queen
charToPromo 'Q' = Just Queen
charToPromo 'r' = Just Rook
charToPromo 'R' = Just Rook
charToPromo 'b' = Just Bishop
charToPromo 'B' = Just Bishop
charToPromo 'n' = Just Knight
charToPromo 'N' = Just Knight
charToPromo _   = Nothing

uciOfMove :: Move -> String
uciOfMove (Move f t pr _) = prettySquare f ++ prettySquare t ++ maybe "" pp pr
  where
    pp Queen  = "q"
    pp Rook   = "r"
    pp Bishop = "b"
    pp Knight = "n"
    pp Pawn   = "p"
    pp King   = "k"
