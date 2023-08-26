module Lib ( someFunc ) where

import TiposBase
import Control.Monad.State
import Data.Char
    ( ord, digitToInt, chr, isAlpha, isDigit, toUpper )



data Check = No | Yes Player 
data Player = Player1 | Player2 deriving(Eq, Show)
type GameStatus = (Bool, Player)
type GameValue = Player

--state :: (s -> (a, s)) -> State s a
--updateState :: Bool -> State GameStatus GameValue
--updateState isStillGoing = do 
--  (on, player) <- get
--  if isStillGoing
--    then 
--      (if player == Player2 
--        then
--          put (isStillGoing, Player1)
--        else
--          put (isStillGoing, Player2))
--    else 
  --    return player


initialBoard :: Board
initialBoard =
  [ [ Occupied (Rook Black), Occupied (Knight Black), Occupied (Bishop Black), Occupied (Queen Black), Occupied (King Black), Occupied (Bishop Black), Occupied (Knight Black), Occupied (Rook Black) ]
  , [ Occupied (Pawn Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black),  Occupied (Pawn Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black) ]
  , [ Empty,               Empty,                 Empty,                 Empty,                Empty,               Empty,                 Empty,                 Empty               ]
  , [ Empty,               Empty,                 Empty,                 Empty,                Empty,               Empty,                 Empty,                 Empty               ]
  , [ Empty,               Empty,                 Empty,                 Empty,                Empty,               Empty,                 Empty,                 Empty               ]
  , [ Empty,               Empty,                 Empty,                 Empty,                Empty,               Empty,                 Empty,                 Empty               ]
  , [ Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),   Occupied (Pawn White),  Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),   Occupied (Pawn White) ]
  , [ Occupied (Rook White), Occupied (Knight White), Occupied (Bishop White), Occupied (Queen White), Occupied (King White), Occupied (Bishop White), Occupied (Knight White), Occupied (Rook White) ]
  ]


-- 1. Coletar Input do Usuario: devolver Posicao ini Posicao fin
-- 2.1 Validar o movimento da peca (origem x destino)
-- 2.2 Validar Excecoes (ROque, Cheque, Cheque Mate, En Passan)
-- 3.1 Atualizar o tabuleiro de acordo
-- 3.2 Retornar erro e pedir input novamente -> 2.
-- 4. Atualizar estado para sinalizar movimento do proximo jogador




renderBoard :: Board -> IO ()
renderBoard board = putStrLn $ unlines $ map renderRow board
  where
    renderSquare :: Square -> [Char]
    renderSquare Empty                      = "-"
    renderSquare (Occupied piece)     = renderPiece piece

    renderPiece :: Piece -> [Char]
    renderPiece (Queen Black)    = " bq "--"♛"
    renderPiece (King Black)     = " bk "--"♚"
    renderPiece (Rook Black)     = " br "--"♜"
    renderPiece (Bishop Black)   = " bb "--"♝"
    renderPiece (Knight Black)   = " bt "--"♞"
    renderPiece (Pawn Black)     = " bp "--"♟"
    renderPiece (King White)     = " wk "--"♔"
    renderPiece (Queen White)    = " wq "--"♕"
    renderPiece (Rook White)     = " wr "--"♖"
    renderPiece (Bishop White)   = " wb "--"♗"
    renderPiece (Knight White)   = " wt "--"♘"
    renderPiece (Pawn White)     = " wp "--"♙"

    renderRow :: [Square] -> [Char]
    renderRow = concatMap renderSquare

--Retorna o Square verificando o input
returnSquare :: Board -> String -> Square
returnSquare bd (x0:x1:str) = bd!!y1!!y0
                              where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
                                    y1 = 8 - (digitToInt x1)

--Verifica se casa está vazia
isEmptySquare :: Board -> String -> Bool
isEmptySquare bd (x0:x1:str) = bd!!y1!!y0 == Empty
                               where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
                                     y1 = 8 - (digitToInt x1)

--Verifica qual o proximo jogador
nextPlayer :: Player -> Player
nextPlayer player | player == Player1 = Player2
                  | otherwise = Player1

--Retorna qual peça está na casa (Precisa validar se está vazia antes)
whichPiece :: Square -> Piece
whichPiece (Occupied piece) = piece

--Retorna um novo board depois de movimentar uma peça de acordo com o input
movePiece :: Board -> String -> Board
movePiece board (x0:x1:x2:x3:x4) = [[if x == y3 && y == y2 then returnSquare board (x0:x1:x2:x3:x4) 
            else if x == y1 && y == y0 then Empty else board!!x!!y |
            y <- [0..((length (board!!x))-1)]]| x <- [0..((length board) - 1)]]
  where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
        y1 = 8 - (digitToInt x1)
        y2 = (digitToInt (chr ((ord (toUpper x2)) - 17))) 
        y3 = 8 - (digitToInt x3)

converteIntEmColuna :: Int -> Char
converteIntEmColuna 1 ='A' 
converteIntEmColuna 2 ='B' 
converteIntEmColuna 3 ='C' 
converteIntEmColuna 4 ='D' 
converteIntEmColuna 5 ='E' 
converteIntEmColuna 6 ='F' 
converteIntEmColuna 7 ='G' 
converteIntEmColuna 8 ='H' 

--Retorna cor do jogador
playerColor :: Player -> Color
playerColor Player1 = White
playerColor Player2 = Black

--Retorna uma lista de tuplas com a posição e peças de uma das cores
allPlayerPiecesPositions :: Board -> Color -> [(Position,  Piece)]
allPlayerPiecesPositions board color = [(Position (converteIntEmColuna(x + 1))  (y + 8), whichPiece (board!!x!!y))  | x <- [0..((length board) - 1)], y <- [0..((length (board!!x))-1)], board!!x!!y == Occupied (_ color)]

--Encontra o rei de uma certa cor
whereIsKing :: Board -> Color -> Position
whereIsKing board color = head [Position (converteIntEmColuna(x + 1))  (y + 8) | x <- [0..((length board) - 1)], y <- [0..((length (board!!x))-1)], board!!x!!y == Occupied (King color)]

validPositions :: String -> Int -> Bool
validPositions "" i = True
validPositions (x:xs) i | ((not (i `mod` 2 == 0)) && (isDigit x)  &&  (ord x >= 49 && ord x <= 56))
                            || (i `mod`  2 == 0 && (isAlpha x) && (ord (toUpper x) >= 65 && ord (toUpper x) <= 72)) = validPositions xs (i + 1)
                        | otherwise = False

isCheck :: Board -> String -> Player ->Bool
isCheck board userInput player = validaMovimento (whichPiece (returnSquare board userInput)) 
                             (Position (toUpper (userInput!!2)) (digitToInt (userInput!!3))) 
                              (whereIsKing board (playerColor (nextPlayer player)))

playGame :: Int -> Bool -> Player -> Board -> Check -> IO ()
playGame turn on player board check =
  if on
    then do
      renderBoard board
      print player
      userInput <- getLine
      if not (length userInput == 4 && validPositions userInput 0 && not (isEmptySquare board userInput)) --jogador e movimentar para a mesma casa
        || not (validaMovimento (whichPiece (returnSquare board userInput)) (Position (toUpper (userInput!!0)) (digitToInt (userInput!!1))) (Position (toUpper (userInput!!2)) (digitToInt (userInput!!3))))
          then do
            putStrLn "movivento invalido"
            playGame turn on player board check
            --    
      else if isCheck board userInput player --verifica se movimento deixou em check
        then
         playGame (turn + 1) on (nextPlayer player) (movePiece board userInput) (Yes (nextPlayer player))
        else
          playGame (turn + 1) on (nextPlayer player) (movePiece board userInput) check
  else
    putStrLn "Game Over"

someFunc :: IO ()
someFunc = playGame 1 True Player1 initialBoard No

