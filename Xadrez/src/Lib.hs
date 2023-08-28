module Lib ( someFunc ) where

import TiposBase
import IA
import Control.Monad.State
import Data.Char
    ( ord, digitToInt, chr, isAlpha, isDigit, toUpper )
import GHC.RTS.Flags (TraceFlags(user))


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

renderBoard :: Board -> IO ()
renderBoard board = putStrLn $ unlines $ map renderRow board
  where
    renderSquare :: Square -> [Char]
    renderSquare Empty                      = " -- "
    renderSquare (Occupied piece)     = renderPiece piece

    renderPiece :: Piece -> [Char]
    renderPiece (Queen  White)     = " wq " --" ♛ "
    renderPiece (King   White)     = " wk " --" ♚ "
    renderPiece (Rook   White)     = " wr " --" ♜ "
    renderPiece (Bishop White)     = " wb " --" ♝ "
    renderPiece (Knight White)     = " wn " --" ♞ "
    renderPiece (Pawn   White)     = " wp " --" ♟ "
    renderPiece (King   Black)     = " bk " --" ♔ "
    renderPiece (Queen  Black)     = " bq " --" ♕ "
    renderPiece (Rook   Black)     = " br " --" ♖ "
    renderPiece (Bishop Black)     = " bb " --" ♗ "
    renderPiece (Knight Black)     = " bn " --" ♘ "
    renderPiece (Pawn   Black)     = " bp " --" ♙ "

    renderRow :: [Square] -> [Char]
    renderRow = concatMap renderSquare

isEmptySquare :: Board -> String -> Bool
isEmptySquare bd (x0:x1:str) = bd!!y1!!y0 == Empty
                               where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
                                     y1 = 8 - (digitToInt x1)

whichPiece :: Square -> Piece
whichPiece (Occupied piece) = piece

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

checkMate :: Board -> Player -> Bool
checkMate board player  = isCheckMate (runOutcheckMate board player (allMoviesFromUser board (allPlayerPiecesPositions board (playerColor player))) )

isCheckMate :: [(Piece, String)] -> Bool
isCheckMate [] = True
isCheckMate positions = False

printCheck :: Bool -> IO()
printCheck True = print "Voce esta em check"
printCheck False = putStrLn "Voce nao esta em check"

printCheckMate :: Player -> IO()
printCheckMate Player1 = putStrLn "Player 2 Ganhou"
printCheckMate Player2 = putStrLn "Player 1 Ganhou"

piaoAvancouDoisTurnoAnterior :: Square -> Position -> Position -> Bool
piaoAvancouDoisTurnoAnterior Empty _ _ = False
piaoAvancouDoisTurnoAnterior (Occupied (Pawn Black)) (Position y1 x1) (Position y2 x2) = y1 == y2 && x1 - x2  == 2
piaoAvancouDoisTurnoAnterior (Occupied (Pawn White)) (Position y1 x1) (Position y2 x2) = y1 == y2 && x2 - x1  == 2 
piaoAvancouDoisTurnoAnterior _ _ _ = False 

enPassant :: Bool -> Color ->[(Piece, Position)] -> (Piece, Position) -> [(Piece, Position)]
enPassant False color  xs _ = [ x |x <- xs, fst x /= Pawn color]
enPassant True color xs x = [ x |x <- xs, fst x /= Pawn color] ++ [x]

tryEnPassant :: Piece -> Position -> Position -> Bool
tryEnPassant (Pawn Black) (Position y1 x1) (Position y2 x2) = x1 == 4 && (y2 == chr ((ord y1) -1) || y2 == chr ((ord y1) + 1)) && x2 == 3
tryEnPassant (Pawn White) (Position y1 x1) (Position y2 x2) = x1 == 5 && (y2 == chr ((ord y1) -1) || y2 == chr ((ord y1) + 1)) && x2 == 6

isEnPassantA :: [(Piece, Position)] -> Color -> Position -> [(Piece, Position)]
isEnPassantA xs color (Position y1 x1) = [ x | x <- xs, x == (Pawn color, Position (chr ((ord y1) -1)) x1) || x == (Pawn color, Position (chr ((ord y1) + 1)) x1)]

isEnPassant :: [(Piece, Position)] -> Bool
isEnPassant [] = False
isEnPassant xs = True

playGame :: Int -> Bool -> Int -> Player -> Board -> [(Piece, Position)] -> IO ()
playGame turn on gameType player board roqueList = 
  if on
    then 
      if checkMate board player
        then do
          printCheckMate player
          playGame turn False gameType player board roqueList
      else do
            renderBoard board
            printCheck (check board player)
            print roqueList
            print player
            userInput <- pegaMovimento gameType player board
            let p1 = (Position (toUpper (userInput!!0)) (digitToInt (userInput!!1)))
            let p2 = (Position (toUpper (userInput!!2)) (digitToInt (userInput!!3))) 
            let piaoDois = piaoAvancouDoisTurnoAnterior (returnSquare board userInput) p1 p2
            print piaoDois
            if (not (length userInput == 4) || not(validPositions userInput 0) || isEmptySquare board userInput) --jogador e movimentar para a mesma casa
              || (not (validaMovimento (whichPiece (returnSquare board userInput)) p1 p2) 
              && not (ehRoque board p1 p2)) && 
              not (tryEnPassant (whichPiece (returnSquare board userInput)) p1 p2 && isEnPassant (isEnPassantA roqueList (playerColor (nextPlayer player)) p1))
                then do
                  putStrLn "movivento invalido"
                  playGame turn on gameType player board roqueList
                  --    
            else
              if check (movePiece board userInput) player
                then do
                    putStrLn ""
                    putStrLn "movivento invalido - ainda em check, tente um dos movimentos abaixo: "
                    putStrLn ""
                    print (runOutcheckMate board player (allMoviesFromUser board (allPlayerPiecesPositions board (playerColor player))))
                    playGame turn on gameType player board roqueList
                else
                    if (ehRoque board p1 p2 && roqueCheck board p1 p2 && roqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList 
                      && roqueAlreadMoved (whichPiece (returnSquare board ([userInput!!2] ++ [userInput!!3])), p2) roqueList)
                      || (validaInterposicao board p1 p2 
                      && validaComerPropriaPeca board p1 p2 
                      && validaCasosEspeciais board p1 p2) || (tryEnPassant (whichPiece (returnSquare board userInput)) p1 p2 && isEnPassant (isEnPassantA roqueList (playerColor (nextPlayer player)) p1))
                      && validaMexerPropriaPeca player board p1
                      then
                        if ehRoque board p1 p2 && roqueCheck board p1 p2 && 
                          roqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList && 
                          roqueAlreadMoved (whichPiece (returnSquare board ([userInput!!2] ++ [userInput!!3])), p2) roqueList
                          then case p2 of
                            Position 'A' 1 -> do
                                          let board1 = movePiece board "A1D1"
                                          let board2 = movePiece board1 "E1C1"
                                          playGame (turn + 1) on gameType (nextPlayer player) board2 (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                            Position 'H' 1 -> do
                                          let board1 = movePiece board "H1F1"
                                          let board2 = movePiece board1 "E1G1"
                                          playGame (turn + 1) on gameType (nextPlayer player) board2 (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                            Position 'A' 8 -> do
                                          let board1 = movePiece board "A8D8"
                                          let board2 = movePiece board1 "E8C8"
                                          playGame (turn + 1) on gameType (nextPlayer player) board2 (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                            Position 'H' 8 -> do
                                          let board1 = movePiece board "H8F8"
                                          let board2 = movePiece board1 "E8G8"
                                          playGame (turn + 1) on gameType (nextPlayer player) board2 (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                        else
                          if ehPromocao board p1 p2
                            then do
                              putStrLn "Promote to which piece? ('B' -> Bishop | 'N' -> Knight | 'Q' -> Queen)"
                              inputPromocao <- getLine
                              if length inputPromocao == 1 && validaInputPromocao (toUpper (inputPromocao!!0))
                                then case inputPromocao of
                                        "N" -> playGame (turn + 1) on gameType (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Knight $ playerColor player)) roqueList
                                        "B" -> playGame (turn + 1) on gameType (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Bishop $ playerColor player)) roqueList
                                        "Q" -> playGame (turn + 1) on gameType (nextPlayer player) (atualizaBoardComPromocao (movePiece board userInput) p2 (Queen $ playerColor player)) roqueList
                              else do
                                      putStrLn "InputInvaldido!"
                                      playGame turn on gameType player board roqueList
                          else 
                            do 
                              
                              playGame (turn + 1) on gameType (nextPlayer player) (movePiece board userInput) 
                                     (enPassant piaoDois (playerColor (nextPlayer player)) (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList) ((getPiece (returnSquare board userInput)), p))
                            --else do
                            --  playGame (turn + 1) on gameType (nextPlayer player) (movePiece board userInput) (addRoqueAlreadMoved (whichPiece (returnSquare board userInput), p1) roqueList)
                    else do
                      print (isEnPassantA roqueList (playerColor (nextPlayer player)) p1)
                      print (tryEnPassant (whichPiece (returnSquare board userInput)) p1 p2)
                      print (tryEnPassant (whichPiece (returnSquare board userInput)) p1 p2 && isEnPassant (isEnPassantA roqueList (playerColor (nextPlayer player)) p1))
                      print "Movimento Invalido!!"
                      playGame turn on gameType player board roqueList
  else
    putStrLn "Game Over"

pegaMovimento :: Int -> Player -> Board -> IO String
pegaMovimento 1 _ _ = do getLine
pegaMovimento 2 Player1 _ = do getLine
pegaMovimento 2 Player2 board = do gerarMovimentoPretas board 

validaGameType :: Char -> Bool
validaGameType '1' = True
validaGameType '2' = True
validaGameType _   = False

someFunc :: IO ()
someFunc = do
    putStrLn "Digite 1 para jogar contra um Humano e 2 para jogar contra uma IA"
    gameType <- getLine
    if length gameType == 1 && validaGameType (gameType!!0)
      then case head gameType of
        '1' -> playGame 1 True 1 Player1 initialBoard []
        '2' -> playGame 1 True 2 Player1 initialBoard []
        _ -> error "Invalid game type"
    else do
      print "invalido"
      someFunc

