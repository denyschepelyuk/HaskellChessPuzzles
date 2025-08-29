module Chess.Types
  ( Color(..)
  , PieceType(..)
  , Piece(..)
  , Square(..)
  , fileOf, rankOf
  , at
  , prettySquare
  , renderPieceChar
  , w, b
  ) where

import Data.Char (ord)

-- Side to move / ownership of a piece
data Color = White | Black
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- The six kinds of chessmen
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- A piece is a color and a kind
data Piece = Piece { color :: Color, kind :: PieceType }
  deriving (Eq, Ord)

instance Show Piece where
  show p = [renderPieceChar p]

-- A board square, encoded 0..63 as a1..h8
newtype Square = Square { unSquare :: Int }
  deriving (Eq, Ord)

instance Show Square where
  show = prettySquare

-- File (0=a .. 7=h)
fileOf :: Square -> Int
fileOf (Square i) = i `mod` 8

-- Rank (0=1 .. 7=8)
rankOf :: Square -> Int
rankOf (Square i) = i `div` 8

-- Construct a square from algebraic components, e.g. at 'e' 4
-- Throws an error for invalid input to keep core simple for now.
at :: Char -> Int -> Square
at f r
  | f >= 'a' && f <= 'h' && r >= 1 && r <= 8 =
      Square ((r - 1) * 8 + (ord f - ord 'a'))
  | otherwise = error ("Invalid square: " ++ [f] ++ show r)

-- Render a Square like "e4"
prettySquare :: Square -> String
prettySquare (Square i) = [toFile i] ++ show (toRank i)
  where
    toFile k = toEnum (fromEnum 'a' + (k `mod` 8))
    toRank k = 1 + (k `div` 8)

-- Compact ASCII rendering for pieces
renderPieceChar :: Piece -> Char
renderPieceChar (Piece White King)   = 'K'
renderPieceChar (Piece White Queen)  = 'Q'
renderPieceChar (Piece White Rook)   = 'R'
renderPieceChar (Piece White Bishop) = 'B'
renderPieceChar (Piece White Knight) = 'N'
renderPieceChar (Piece White Pawn)   = 'P'
renderPieceChar (Piece Black King)   = 'k'
renderPieceChar (Piece Black Queen)  = 'q'
renderPieceChar (Piece Black Rook)   = 'r'
renderPieceChar (Piece Black Bishop) = 'b'
renderPieceChar (Piece Black Knight) = 'n'
renderPieceChar (Piece Black Pawn)   = 'p'

-- Helpers to build pieces succinctly
w :: PieceType -> Piece
w t = Piece White t

b :: PieceType -> Piece
b t = Piece Black t
