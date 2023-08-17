module Lib
    ( someFunc
    ) where

import TiposBase

-- 1. Coletar Input do Usuario: devolver Posicao ini Posicao fin
-- 2.1 Validar o movimento da peca (origem x destino)
-- 2.2 Validar Excecoes (ROque, Cheque, Cheque Mate, En Passan)
-- 3.1 Atualizar o tabuleiro de acordo
-- 3.2 Retornar erro e pedir input novamente -> 2.
-- 4. Atualizar estado para sinalizar movimento do proximo jogador


{--
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
    renderSquare (Occupied piece)     = renderPiece color piece

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
--}

someFunc :: IO ()
someFunc = print "oi"
