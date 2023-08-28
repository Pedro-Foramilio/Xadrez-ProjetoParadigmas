module Lib ( someFunc ) where

import TiposBase
import Control.Monad.State
import Data.Char
    ( ord, digitToInt, chr, isAlpha, isDigit, toUpper )
import GHC.RTS.Flags (TraceFlags(user))





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
initialBoard' =
  [ [ Occupied (Rook Black), Occupied (Knight Black), Occupied (Bishop Black), Occupied (Queen Black), Occupied (King Black), Occupied (Bishop Black), Occupied (Knight Black), Occupied (Rook Black) ]
  , [ Occupied (Pawn Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black),               Empty,    Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black) ]
  , [ Empty,                 Empty,                    Empty,                   Empty,                  Empty,               Empty,               Empty,                 Empty               ]
  , [ Empty,                 Empty,                    Empty,                   Empty,                  Empty,               Empty,                 Empty,                 Empty               ]
  , [ Empty,                 Empty,                    Empty,                   Empty,                  Empty,               Empty,                 Empty,                 Empty               ]
  , [ Empty,                 Empty,                    Empty,                   Empty,                  Empty,               Empty,                 Empty,                 Empty               ]
  , [ Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),   Empty,  Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),   Occupied (Pawn White) ]
  , [ Occupied (Rook White), Empty, Empty, Empty, Occupied (King White), Occupied (Bishop White), Occupied (Knight White), Occupied (Rook White) ]
  ]
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
allPlayerPiecesPositions board color = [(Position (converteIntEmColuna(y + 1))  (8 - x), whichPiece (board!!x!!y))  | x <- [0..((length board) - 1)], y <- [0..((length (board!!x))-1)], board!!x!!y /= Empty && verifyColorSquare (whichPiece (board!!x!!y)) color]


--isCheck :: Board -> String -> Player ->Bool
--isCheck board userInput player = validaMovimento (whichPiece (returnSquare board userInput)) 
--                             (Position (toUpper (userInput!!2)) (digitToInt (userInput!!3))) 
--                              (whereIsKing board (playerColor (nextPlayer player)))

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

--Retorna todas as peças que dao check
whichGiveCheck :: Board -> Position -> [(Position, Piece)] -> [Piece]
whichGiveCheck board kingPos otherPlayerPieces = [ snd x  | x <- otherPlayerPieces, validaMovimento (snd x) (fst x) kingPos && validaInterposicao board (fst x) kingPos && validaComerPropriaPeca board (fst x) kingPos ]

 -- Se tiver alguma peça que pode atacar o rei
isCheck :: [Piece] -> Bool
isCheck [] = False
isCheck pieces = True

--Verifica se a lista de posicoes se está vazia
isRoque :: [a] -> Bool
isRoque [] = True
isRoque pieces = False


roqueAlreadMoved :: (Piece, Position) -> [(Piece, Position)] -> Bool
roqueAlreadMoved p xs = isRoque([x | x <- xs, x == p])

addRoqueAlreadMoved :: (Piece, Position) -> [(Piece, Position)] -> [(Piece, Position)]
addRoqueAlreadMoved (King Black, Position 'E' 8) xs = xs ++ [(King Black, Position 'E' 8)]
addRoqueAlreadMoved (King White, Position 'E' 1) xs = xs ++ [(King White, Position 'E' 1)]
addRoqueAlreadMoved (Rook Black, Position 'A' 8) xs = xs ++ [(Rook Black, Position 'A' 8)]
addRoqueAlreadMoved (Rook Black, Position 'H' 8) xs = xs ++ [(Rook Black, Position 'H' 8)]
addRoqueAlreadMoved (Rook White, Position 'A' 1) xs = xs ++ [(Rook White, Position 'A' 1)]
addRoqueAlreadMoved (Rook White, Position 'H' 1) xs = xs ++ [(Rook White, Position 'H' 1)]
addRoqueAlreadMoved _ xs = xs

--Valida se nao existe alguma peça olhando para o caminho do roque
roqueCheck :: Board -> Position -> Position -> Bool
roqueCheck board (Position char1 y1) (Position char2 y2) 
    = char1 == 'E' && (piece == King White || piece == King Black ) &&
        case cor of
            White -> y1 == 1 && y2 == 1 && pieceDestino == Rook White &&
                case char2 of
                    'A' ->  isRoque([ x | x <- meioRoqueBrancoGrande, isCheck(whichGiveCheck board x (allPlayerPiecesPositions board Black))])
                    'H' ->  isRoque([ x | x <- meioRoqueBrancoGrande, isCheck(whichGiveCheck board x (allPlayerPiecesPositions board Black))])
            Black -> y1 == 8 && y2 == 8 && pieceDestino == Rook Black &&
                case char2 of
                    'A' ->  isRoque([ x | x <- meioRoqueBrancoGrande, isCheck(whichGiveCheck board x (allPlayerPiecesPositions board Black))])
                    'H' ->  isRoque([ x | x <- meioRoqueBrancoGrande, isCheck(whichGiveCheck board x (allPlayerPiecesPositions board Black))])
    where
        x1 = converteColunaEmInt char1
        x2 = converteColunaEmInt char2
        piece = getPiece $ getSquare board (Position char1 y1)
        cor = getCor piece
        pieceDestino = getPiece $ getSquare board (Position char2 y2)
        meioRoqueBrancoGrande = [
            Position 'B' 1,
            Position 'C' 1,
            Position 'D' 1,
            Position 'E' 1
            ]
        meioRoqueBrancoCurto = [
            Position 'E' 1,
            Position 'F' 1,
            Position 'G' 1
            ]
        meioRoquePretoGrande = [
            Position 'B' 8,
            Position 'C' 8,
            Position 'D' 8,
            Position 'E' 8
            ]
        meioRoquePretoCurto = [
            Position 'E' 8,
            Position 'F' 8,
            Position 'G' 8
            ]

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

validaInputPromocao :: Char -> Bool
validaInputPromocao 'Q' = True
validaInputPromocao 'N' = True
validaInputPromocao 'B' = True
validaInputPromocao _ = False

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

positionToInput :: Position -> Position -> String
positionToInput (Position x y) (Position x1 y1) = [x] ++ show y ++ [x1] ++ show y1

-- Retorna se todos os movimentos que saem do check
runOutcheckMate :: Board -> Player -> [(Position, (Position, Piece))] -> [(Piece, String)]
runOutcheckMate _ _ [] = []
runOutcheckMate board player allMovies = [ (snd (snd x), (positionToInput  (fst (snd x)) (fst x))) | x <- allMovies, not (check (movePiece board (positionToInput  (fst (snd x)) (fst x))) player)]

checkMate :: Board -> Player -> Bool
checkMate board player  = isCheckMate (runOutcheckMate board player (allMoviesFromUser board (allPlayerPiecesPositions board (playerColor player))) )

isCheckMate :: [(Piece, String)] -> Bool
isCheckMate [] = True
isCheckMate positions = False

-- Retorna todo od movimentos validos do usuário
allMoviesFromUser :: Board -> [(Position,  Piece)] -> [(Position, (Position,  Piece))]
allMoviesFromUser _ [] = []
allMoviesFromUser board (x0:xs) = allMoviesFromPiece board x0 ++ allMoviesFromUser board xs

-- Todos os movimentos validos de uma peça
allMoviesFromPiece :: Board -> (Position,  Piece) -> [(Position, (Position,  Piece))]
allMoviesFromPiece board (position, piece)  = [ (x, (position, piece)) | x <- geraMovimentos piece position, validaMovimento piece position x 
                                                                                                            && validaInterposicao board position x
                                                                                                            &&  validaComerPropriaPeca board position x]

check :: Board -> Player -> Bool
check board player = isCheck (whichGiveCheck board (whereIsKing board  (playerColor player)) (allPlayerPiecesPositions board (playerColor (nextPlayer player))))

printCheck :: Bool -> IO()
printCheck True = print "Voce esta em check"
printCheck False = putStrLn "Voce nao esta em check"

printCheckMate :: Player -> IO()
printCheckMate Player1 = putStrLn "Player 2 Ganhou"
printCheckMate Player2 = putStrLn "Player 1 Ganhou"

playGame :: Int -> Bool -> Player -> Board -> [(Piece, Position)] -> IO ()
playGame turn on player board roqueList = 
  if on
    then 
      if checkMate board player
        then do
          printCheckMate player
          playGame turn False player board roqueList
      else do
            renderBoard board
            printCheck (check board player)
            print player
            userInput <- getLine
            let p1 = (Position (toUpper (userInput!!0)) (digitToInt (userInput!!1)))
            let p2 = (Position (toUpper (userInput!!2)) (digitToInt (userInput!!3))) 
            if (not (length userInput == 4) || not(validPositions userInput 0) || isEmptySquare board userInput) --jogador e movimentar para a mesma casa
              || (not (validaMovimento (whichPiece (returnSquare board userInput)) p1 p2) 
              && not (ehRoque board p1 p2))
                then do
                  putStrLn "movivento invalido"
                  playGame turn on player board roqueList
                  --    
            else
              if check (movePiece board userInput) player
                then do
                    putStrLn ""
                    putStrLn "movivento invalido - ainda em check, tente um dos movimentos abaixo: "
                    putStrLn ""
                    print (runOutcheckMate board player (allMoviesFromUser board (allPlayerPiecesPositions board (playerColor player))))
                    playGame turn on player board roqueList
                else
                    if (ehRoque board p1 p2 && roqueCheck board p1 p2 && roqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList 
                      && roqueAlreadMoved (whichPiece (returnSquare board ([userInput!!2] ++ [userInput!!3])), p2) roqueList)
                      || (validaInterposicao board p1 p2 
                      && validaComerPropriaPeca board p1 p2 
                      && validaCasosEspeciais board p1 p2)
                      && validaMexerPropriaPeca player board p1
                      then
                        if ehRoque board p1 p2 && roqueCheck board p1 p2 && 
                          roqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList && 
                          roqueAlreadMoved (whichPiece (returnSquare board ([userInput!!2] ++ [userInput!!3])), p2) roqueList
                          then case p2 of
                            Position 'A' 1 -> do
                                          let board1 = movePiece board "A1D1"
                                          let board2 = movePiece board1 "E1C1"
                                          playGame (turn + 1) on (nextPlayer player) board2 (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                            Position 'H' 1 -> do
                                          let board1 = movePiece board "H1F1"
                                          let board2 = movePiece board1 "E1G1"
                                          playGame (turn + 1) on (nextPlayer player) board2 (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                            Position 'A' 8 -> do
                                          let board1 = movePiece board "A8D8"
                                          let board2 = movePiece board1 "E8C8"
                                          playGame (turn + 1) on (nextPlayer player) board2 (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                            Position 'H' 8 -> do
                                          let board1 = movePiece board "H8F8"
                                          let board2 = movePiece board1 "E8G8"
                                          playGame (turn + 1) on (nextPlayer player) board2 (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                        else
                          if ehPromocao board p1 p2
                            then do
                              putStrLn "Promote to which piece? ('B' -> Bishop | 'N' -> Knight | 'Q' -> Queen)"
                              inputPromocao <- getLine
                              if length inputPromocao == 1 && validaInputPromocao (toUpper (inputPromocao!!0))
                                then case inputPromocao of
                                        "N" -> playGame (turn + 1) on (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Knight $ playerColor player)) roqueList
                                        "B" -> playGame (turn + 1) on (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Bishop $ playerColor player)) roqueList
                                        "Q" -> playGame (turn + 1) on (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Queen $ playerColor player)) roqueList
                              else do
                                      putStrLn "InputInvaldido!"
                                      playGame turn on player board roqueList
                          else playGame (turn + 1) on (nextPlayer player) (movePiece board userInput) (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                    else do
                      print "Movimento Invalido!!"
                      playGame turn on player board roqueList
  else
    putStrLn "Game Over"
  
someFunc :: IO ()
someFunc = playGame 1 True Player1 initialBoard []

