module Main (main) where

import Chess.Board (prettyBoard)
import Chess.Setup (exampleBoard)

main :: IO ()
main = putStrLn (prettyBoard exampleBoard)