module Lib
    ( someFunc
    ) where

data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show)
data Color = Black | White deriving (Show)
data Square = Empty | Occupied Color Piece deriving (Show)
type Board = [[Square]]

initialBoard :: Board
initialBoard =
  [ [ Occupied Black Rook, Occupied Black Knight, Occupied Black Bishop, Occupied Black Queen, Occupied Black King, Occupied Black Bishop, Occupied Black Knight, Occupied Black Rook ]
  , [ Occupied Black Pawn, Occupied Black Pawn,   Occupied Black Pawn,   Occupied Black Pawn,  Occupied Black Pawn, Occupied Black Pawn,   Occupied Black Pawn,   Occupied Black Pawn ]
  , [ Empty,               Empty,                 Empty,                 Empty,                Empty,               Empty,                 Empty,                 Empty               ]
  , [ Empty,               Empty,                 Empty,                 Empty,                Empty,               Empty,                 Empty,                 Empty               ]
  , [ Empty,               Empty,                 Empty,                 Empty,                Empty,               Empty,                 Empty,                 Empty               ]
  , [ Empty,               Empty,                 Empty,                 Empty,                Empty,               Empty,                 Empty,                 Empty               ]
  , [ Occupied White Pawn, Occupied White Pawn,   Occupied White Pawn,   Occupied White Pawn,  Occupied White Pawn, Occupied White Pawn,   Occupied White Pawn,   Occupied White Pawn ]
  , [ Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White Queen, Occupied White King, Occupied White Bishop, Occupied White Knight, Occupied White Rook ]
  ]

renderBoard :: Board -> IO ()
renderBoard board = putStrLn $ unlines $ map renderRow board
  where
    renderSquare :: Square -> [Char]
    renderSquare Empty                      = "-"
    renderSquare (Occupied color piece)     = renderPiece color piece

    renderPiece :: Color -> Piece -> [Char]
    renderPiece Black King     = "♚"
    renderPiece Black Queen    = "♛"
    renderPiece Black Rook     = "♜"
    renderPiece Black Bishop   = "♝"
    renderPiece Black Knight   = "♞"
    renderPiece Black Pawn     = "♟"
    renderPiece White King     = "♔"
    renderPiece White Queen    = "♕"
    renderPiece White Rook     = "♖"
    renderPiece White Bishop   = "♗"
    renderPiece White Knight   = "♘"
    renderPiece White Pawn     = "♙"

    renderRow :: [Square] -> [Char]
    renderRow = concatMap renderSquare


someFunc :: IO ()
someFunc = renderBoard initialBoard
