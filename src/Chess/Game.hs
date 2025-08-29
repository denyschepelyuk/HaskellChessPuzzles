module Chess.Game
  ( Castling(..)
  , Game(..)
  , startGame
  , exampleGame
  , switchSide
  ) where

import Chess.Types
import Chess.Board
import Chess.Setup (exampleBoard)

-- Minimal castling flags placeholder (unused for now)
data Castling = Castling { wK :: Bool, wQ :: Bool, bK :: Bool, bQ :: Bool }
  deriving (Eq, Show)

-- Core game state (kept simple so we can iterate later)
data Game = Game
  { board    :: Board
  , toMove   :: Color
  , castling :: Castling
  , epTarget :: Maybe Square
  , halfmove :: Int
  , fullmove :: Int
  } deriving (Eq, Show)

startGame :: Board -> Color -> Game
startGame b c = Game b c (Castling False False False False) Nothing 0 1

exampleGame :: Game
exampleGame = startGame exampleBoard White

switchSide :: Color -> Color
switchSide White = Black
switchSide Black = White
