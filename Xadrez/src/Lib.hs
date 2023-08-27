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
    renderSquare Empty                      = " - "
    renderSquare (Occupied piece)     = renderPiece piece

    renderPiece :: Piece -> [Char]
    renderPiece (Queen  White)     = " ♛ "
    renderPiece (King   White)     = " ♚ "
    renderPiece (Rook   White)     = " ♜ "
    renderPiece (Bishop White)     = " ♝ "
    renderPiece (Knight White)     = " ♞ "
    renderPiece (Pawn   White)     = " ♟ "
    renderPiece (King   Black)     = " ♔ "
    renderPiece (Queen  Black)     = " ♕ "
    renderPiece (Rook   Black)     = " ♖ "
    renderPiece (Bishop Black)     = " ♗ "
    renderPiece (Knight Black)     = " ♘ "
    renderPiece (Pawn   Black)     = " ♙ "

    renderRow :: [Square] -> [Char]
    renderRow = concatMap renderSquare

--Retorna uma lista de tuplas com a posição e peças de uma das cores
allPlayerPiecesPositions :: Board -> Color -> [(Position,  Piece)]
allPlayerPiecesPositions board color = [(Position (converteIntEmColuna(x + 1))  (y + 8), whichPiece (board!!x!!y))  | x <- [0..((length board) - 1)], y <- [0..((length (board!!x))-1)], board!!x!!y == Occupied (_ color)]

--Encontra o rei de uma certa cor
whereIsKing :: Board -> Color -> Position
whereIsKing board color = head [Position (converteIntEmColuna(x + 1))  (y + 8) | x <- [0..((length board) - 1)], y <- [0..((length (board!!x))-1)], board!!x!!y == Occupied (King color)]

isCheck :: Board -> String -> Player ->Bool
isCheck board userInput player = validaMovimento (whichPiece (returnSquare board userInput)) 
                             (Position (toUpper (userInput!!2)) (digitToInt (userInput!!3))) 
                              (whereIsKing board (playerColor (nextPlayer player)))

returnSquare :: Board -> String -> Square
returnSquare bd (x0:x1:str) = bd!!y1!!y0
                              where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
                                    y1 = 8 - (digitToInt x1)

isEmptySquare :: Board -> String -> Bool
isEmptySquare bd (x0:x1:str) = bd!!y1!!y0 == Empty
                               where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
                                     y1 = 8 - (digitToInt x1)

nextPlayer :: Player -> Player
nextPlayer player | player == Player1 = Player2
                  | otherwise = Player1

playerColor :: Player -> Color
playerColor Player1 = White
playerColor Player2 = Black

whichPiece :: Square -> Piece
whichPiece (Occupied piece) = piece

movePiece :: Board -> String -> Board
movePiece board (x0:x1:x2:x3:x4) = [[if x == y3 && y == y2 then returnSquare board (x0:x1:x2:x3:x4) 
            else if x == y1 && y == y0 then Empty else board!!x!!y |
            y <- [0..((length (board!!x))-1)]]| x <- [0..((length board) - 1)]]
  where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
        y1 = 8 - (digitToInt x1)
        y2 = (digitToInt (chr ((ord (toUpper x2)) - 17))) 
        y3 = 8 - (digitToInt x3)



validPositions :: String -> Int -> Bool
validPositions "" i = True
validPositions (x:xs) i | ((not (i `mod` 2 == 0)) && (isDigit x)  &&  (ord x >= 49 && ord x <= 56))
                            || (i `mod`  2 == 0 && (isAlpha x) && (ord (toUpper x) >= 65 && ord (toUpper x) <= 72)) = validPositions xs (i + 1)
                        | otherwise = False

validaMexerPropriaPeca :: Player -> Board -> Position -> Bool
validaMexerPropriaPeca player board p1 
  = (player == Player1 && cor == White) || (player == Player2 && cor == Black)
  where
    cor = getCor $ getPiece $ getSquare board p1

validaInputPromocao :: String -> Bool
validaInputPromocao _ = True

atualizaBoardComPromocao :: Board -> Position -> Piece -> Board
atualizaBoardComPromocao board (Position charr y) peca = 
  case y of
    8 -> novaLinha : tail board
    1 -> init board ++ [novaLinha]
    where
      linhaParaAlterar = case y of
                    1 -> board !! 7
                    8 -> board !! 0
      novaLinha = [linhaParaAlterar !! i | i <- [0 .. converteColunaEmInt charr - 2] ]
                  ++ [Occupied peca] ++ [linhaParaAlterar !! i | i <- [converteColunaEmInt charr .. 7] ]

playGame :: Int -> Bool -> Player -> Board -> IO ()
playGame turn on player board =
  if on
    then do
      renderBoard board
      print player
      userInput <- getLine
      let p1 = (Position (toUpper (userInput!!0)) (digitToInt (userInput!!1)))
      let p2 = (Position (toUpper (userInput!!2)) (digitToInt (userInput!!3))) 
      if (not (length userInput == 4 && validPositions userInput 0 && not (isEmptySquare board userInput)) --jogador e movimentar para a mesma casa
        || not (validaMovimento (whichPiece (returnSquare board userInput)) p1 p2)) 
        && not (ehRoque board p1 p2)
          then do
            putStrLn "movivento invalido"
            playGame turn on player board
            --    
      else do
        if ehRoque board p1 p2 || (validaInterposicao board p1 p2 
            && validaComerPropriaPeca board p1 p2 
            && validaCasosEspeciais board p1 p2)
            && validaMexerPropriaPeca player board p1
          then
            if ehRoque board p1 p2
              then case p2 of
                Position 'A' 1 -> do
                                    let board1 = movePiece board "A1D1"
                                    let board2 = movePiece board1 "E1C1"
                                    playGame (turn + 1) on (nextPlayer player) board2
                Position 'H' 1 -> do
                                    let board1 = movePiece board "H1F1"
                                    let board2 = movePiece board1 "E1G1"
                                    playGame (turn + 1) on (nextPlayer player) board2
                Position 'A' 8 -> do
                                    let board1 = movePiece board "A8D8"
                                    let board2 = movePiece board1 "E8C8"
                                    playGame (turn + 1) on (nextPlayer player) board2
                Position 'H' 8 -> do
                                    let board1 = movePiece board "H8F8"
                                    let board2 = movePiece board1 "E8G8"
                                    playGame (turn + 1) on (nextPlayer player) board2
            else
              if ehPromocao board p1 p2
                then do
                        putStrLn "Promote to which piece? ('B' -> Bishop | 'N' -> Knight | 'Q' -> Queen)"
                        inputPromocao <- getLine
                        if validaInputPromocao inputPromocao
                          then case inputPromocao of
                                  "N" -> playGame (turn + 1) on (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Knight $ playerColor player)) 
                                  "B" -> playGame (turn + 1) on (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Bishop $ playerColor player)) 
                                  "Q" -> playGame (turn + 1) on (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Queen $ playerColor player)) 
                        else do
                                putStrLn "InputInvaldido!"
                                playGame turn on player board
              else playGame (turn + 1) on (nextPlayer player) (movePiece board userInput)  
        else do
            print "Movimento Invalido!!"
            playGame turn on player board
  else
    putStrLn "Game Over"
  
someFunc :: IO ()
someFunc = playGame 1 True Player1 initialBoard

