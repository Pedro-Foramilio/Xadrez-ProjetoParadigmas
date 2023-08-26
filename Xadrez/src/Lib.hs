module Lib ( someFunc ) where

import TiposBase
import Control.Monad.State
import Data.Char
    ( ord, digitToInt, chr, isAlpha, isDigit, toUpper )




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

returnSquare :: Board -> String -> Square
returnSquare bd (x0:x1:str) = bd!!y0!!y1
                              where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
                                    y1 = (digitToInt x1) -1

isEmptySquare :: Board -> String -> Bool
isEmptySquare bd (x0:x1:str) = bd!!y0!!y1 == Empty
                               where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
                                     y1 = (digitToInt x1) -1

nextPlayer :: Player -> Player
nextPlayer player | player == Player1 = Player2
                  | otherwise = Player1

whichPiece :: Square -> Piece
whichPiece (Occupied piece) = piece

movePiece :: Board -> String -> Board
movePiece board (x0:x1:x2:x3:x4) = [[if x == y2 && y == y3 then returnSquare board (x0:x1:x2:x3:x4) 
            else if x == y0 && y == y1 then Empty else board!!x!!y |
            y <- [0..((length (board!!x))-1)]]| x <- [0..((length board) - 1)]]
  where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
        y1 = (digitToInt x1) - 1
        y2 = (digitToInt (chr ((ord (toUpper x2)) - 17))) 
        y3 = (digitToInt x3) -1



validPositions :: String -> Int -> Bool
validPositions "" i = True
validPositions (x:xs) i | ((not (i `mod` 2 == 0)) && (isDigit x)  &&  (ord x >= 49 && ord x <= 56))
                            || (i `mod`  2 == 0 && (isAlpha x) && (ord (toUpper x) >= 65 && ord (toUpper x) <= 72)) = validPositions xs (i + 1)
                        | otherwise = False

playGame :: Int -> Bool -> Player -> Board -> IO ()
playGame turn on player board =
  if on
    then do
      renderBoard board
      print player
      userInput <- getLine
      if not (length userInput == 4 && validPositions userInput 0 && not (isEmptySquare board userInput)) 
        --not (validaMovimento (whichPiece (returnSquare board userInput)) (Position (userInput!!0) (digitToInt (userInput!!1))) (Position (userInput!!2) (digitToInt (userInput!!3))))
          then do  
            putStrLn "movivento invalido"
            playGame turn on player board
            --    
      else do
        playGame (turn + 1) on (nextPlayer player) (movePiece board userInput)
  else
    putStrLn "Game Over"
  



someFunc :: IO ()
someFunc = playGame 1 True Player1 initialBoard

