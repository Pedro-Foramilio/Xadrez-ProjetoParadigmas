module TiposBase( Piece(..), Color(..), Square(..), Board(..), Position(..), Player(..)
    ,validaMovimento, getSquare, getPiece, getCor, geraCaminho, validaInterposicao, 
     validaComerPropriaPeca, validaCasosEspeciais, ehRoque, ehPromocao, geraMovimentos,
     converteColunaEmInt, converteIntEmColuna, isInBoard, verifyColorSquare, whereIsKing,
     positionToInput, runOutcheckMate, check, movePiece, isCheck, whichGiveCheck,
     playerColor, allPlayerPiecesPositions, nextPlayer, returnSquare, allMoviesFromUser, allMoviesFromPiece) where

import Prelude
import Data.Char
    ( ord, digitToInt, chr, isAlpha, isDigit, toUpper )

data Player = Player1 | Player2 deriving(Eq, Show)
data Piece = King Color | Queen Color | Rook Color
            | Bishop Color | Knight Color | Pawn Color deriving (Eq, Show)
data Color = Black | White deriving (Eq, Show)
data Position = Position Char Int deriving (Eq, Show)
data Square = Empty | Occupied Piece deriving (Eq, Show)
type Board = [[Square]]

converteColunaEmInt :: Char -> Int
converteColunaEmInt 'A' = 1
converteColunaEmInt 'B' = 2
converteColunaEmInt 'C' = 3
converteColunaEmInt 'D' = 4
converteColunaEmInt 'E' = 5
converteColunaEmInt 'F' = 6
converteColunaEmInt 'G' = 7
converteColunaEmInt 'H' = 8
converteColunaEmInt _ = -1

converteIntEmColuna :: Int -> Char
converteIntEmColuna 1 = 'A'
converteIntEmColuna 2 = 'B'
converteIntEmColuna 3 = 'C'
converteIntEmColuna 4 = 'D'
converteIntEmColuna 5 = 'E'
converteIntEmColuna 6 = 'F'
converteIntEmColuna 7 = 'G'
converteIntEmColuna 8 = 'H'
converteIntEmColuna _ = '-'

nextPlayer :: Player -> Player
nextPlayer player | player == Player1 = Player2
                  | otherwise = Player1

returnSquare :: Board -> String -> Square
returnSquare bd (x0:x1:str) = bd!!y1!!y0
                              where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
                                    y1 = 8 - (digitToInt x1)

playerColor :: Player -> Color
playerColor Player1 = White
playerColor Player2 = Black

positionToInput :: Position -> Position -> String
positionToInput (Position x y) (Position x1 y1) = [x] ++ show y ++ [x1] ++ show y1

isInBoard :: Position -> Bool
isInBoard (Position i y) =  x >= 1 && x <= 8 && y >= 1 && y <= 8 
    where x = converteColunaEmInt i

getSquare :: Board -> Position -> Square
getSquare board (Position charr y) = board !! (8 - y) !! x 
    where x = converteColunaEmInt charr - 1

getPiece :: Square -> Piece
getPiece (Occupied piece) = piece

getCor :: Piece -> Color
getCor (Pawn c)   = c
getCor (Bishop c) = c
getCor (Knight c) = c
getCor (Rook c)   = c
getCor (Queen c)  = c
getCor (King c)   = c

 -- Verifica se a peça que está no quadrado é da cor correspondente
verifyColorSquare :: Piece -> Color -> Bool
verifyColorSquare (Queen colorPiece) color =  colorPiece == color
verifyColorSquare (King colorPiece) color =  colorPiece == color
verifyColorSquare (Rook colorPiece) color =  colorPiece == color
verifyColorSquare (Bishop colorPiece) color =  colorPiece == color
verifyColorSquare (Knight colorPiece) color =  colorPiece == color
verifyColorSquare (Pawn colorPiece) color =  colorPiece == color


geraMovimentos :: Piece -> Position -> [Position]
geraMovimentos (Pawn cor)   p = geraMovimentosPeao p cor
geraMovimentos (Knight _)   p = geraMovimentosCavalo p
geraMovimentos (Bishop _)   p = geraMovimentosBispo p
geraMovimentos (Rook _)     p = geraMovimentosTorre p
geraMovimentos (Queen _)    p = geraMovimentosRainha p
geraMovimentos (King _)     p = geraMovimentosRei p


geraMovimentosPeao :: Position -> Color -> [Position]
geraMovimentosPeao (Position charr y) cor 
    = filter isInBoard (movimentosHorizontais ++ movimentosDiagonais)
    where 
        movimentosHorizontais = case cor of
                            White -> case y of
                                        2 -> [Position charr (y+1), Position charr (y+2)]
                                        _ -> [Position charr (y+1)]
                            Black -> case y of
                                        7 -> [Position charr (y-1), Position charr (y-2)]
                                        _ -> [Position charr (y-1)] 
        movimentosDiagonais = case cor of
                            White -> [Position (converteIntEmColuna (converteColunaEmInt charr + 1)) (y+1),
                                      Position (converteIntEmColuna (converteColunaEmInt charr - 1)) (y+1)]
                            Black -> [Position (converteIntEmColuna (converteColunaEmInt charr + 1)) (y-1),
                                      Position (converteIntEmColuna (converteColunaEmInt charr - 1)) (y-1)]


geraMovimentosCavalo :: Position -> [Position]
geraMovimentosCavalo (Position charr y')
    = filter isInBoard movimentos
    where 
        x' = converteColunaEmInt charr
        offsets = [ (2, -1),  (2, 1),
                    (1, -2),  (1, 2),
                    (-1, -2), (-1, 2),
                    (-2, -1), (-2, 1)]
        movimentos = map (\(x, y) -> Position (converteIntEmColuna (x+x')) (y+y')) offsets

geraMovimentosBispo :: Position -> [Position]
geraMovimentosBispo (Position charr y1) = filter isInBoard movimentos
    where
        x1 = converteColunaEmInt charr
        offsets = [(x, x) | x <- [-7..7], x /= 0] ++ [(-x, x) | x <- [-7..7], x /= 0]
        movimentos = map (\(x, y) -> Position (converteIntEmColuna (x1 + x)) (y1 + y)) offsets                       

geraMovimentosTorre :: Position -> [Position]
geraMovimentosTorre (Position charr y1) = filter isInBoard movimentos
    where
        x1 = converteColunaEmInt charr
        offsets = [(x, 0) | x <- [-7..7], x /= 0] ++ [(0, x) | x <- [-7..7], x /= 0]
        movimentos = map (\(x, y) -> Position (converteIntEmColuna (x1 + x)) (y1 + y)) offsets

geraMovimentosRainha :: Position -> [Position]
geraMovimentosRainha p = geraMovimentosBispo p ++ geraMovimentosTorre p

geraMovimentosRei :: Position -> [Position]
geraMovimentosRei (Position charr y1) = filter isInBoard movimentos
    where
        x1 = converteColunaEmInt charr
        offsets = [(-1,  1), (0,  1), (1,  1),
                     (-1,  0),          (1,  0),
                     (-1, -1), (0, -1), (1, -1) 
                    ]
        movimentos = map (\(x, y) -> Position (converteIntEmColuna (x1 + x)) (y1 + y)) offsets

validaMovimento :: Piece -> Position -> Position -> Bool
validaMovimento (Pawn cor) p1 p2 = isInBoard p2 && validaPeao cor p1 p2
validaMovimento (Knight _) p1 p2 = isInBoard p2 && validaCavalo p1 p2
validaMovimento (Bishop _) p1 p2 = isInBoard p2 && validaBispo p1 p2
validaMovimento (Rook _)   p1 p2 = isInBoard p2 && validaTorre p1 p2
validaMovimento (Queen _)  p1 p2 = isInBoard p2 && validaRainha p1 p2
validaMovimento (King _)   p1 p2 = isInBoard p2 && validaRei p1 p2

validaPeao :: Color -> Position -> Position -> Bool
validaPeao cor p1 p2 = p2 `elem` geraMovimentosPeao p1 cor

validaCavalo :: Position -> Position -> Bool
validaCavalo p1 p2 = p2 `elem` geraMovimentosCavalo p1

validaBispo :: Position -> Position -> Bool
validaBispo p1 p2 = p2 `elem` geraMovimentosBispo p1

validaTorre :: Position -> Position -> Bool
validaTorre p1 p2 = p2 `elem` geraMovimentosTorre p1
          
validaRainha :: Position -> Position -> Bool
validaRainha p1 p2 = p2 `elem` geraMovimentosRainha p1

validaRei :: Position -> Position -> Bool
validaRei p1 p2 = p2 `elem` geraMovimentosRei p1


validaInterposicao :: Board -> Position -> Position -> Bool
validaInterposicao board (Position char1 y1) (Position char2 y2) = (caminhoFiltrado == [p1, p2] 
                                 || caminhoFiltrado == [p2, p1])
                                 && validaDestinoPeao 
    where
        p1 =  (Position char1 y1)
        p2 = (Position char2 y2)
        pecaInicial = getPiece $ getSquare board p1
        quadradoDestino = getSquare board p2
        caminho = geraCaminho pecaInicial p1 p2
        caminhoFiltrado' = filter
                            (\pos -> getSquare board pos /= Empty) 
                            caminho
        caminhoFiltrado =
            if p2 `elem` caminhoFiltrado'
                then caminhoFiltrado'
            else
                caminhoFiltrado' ++ [p2]
        validaDestinoPeao = not (pecaInicial == Pawn White || pecaInicial == Pawn Black)
                            || if char1 == char2
                                    then quadradoDestino == Empty
                                    else quadradoDestino /= Empty

geraCaminho :: Piece -> Position -> Position -> [Position]
geraCaminho (Bishop _) = caminhoBispo
geraCaminho (Knight _) = caminhoCavalo
geraCaminho (Rook   _) = caminhoTorre
geraCaminho (Queen  _) = caminhoRainha
geraCaminho (King   _) = caminhoRei
geraCaminho (Pawn White) = caminhoPeaoBranco
geraCaminho (Pawn Black) = caminhoPeaoPreto


caminhoPeaoBranco :: Position -> Position -> [Position]
caminhoPeaoBranco (Position char1 y1) (Position char2 y2) = 
    if char1 == char2
        then
            case y1 of
                2 -> case y2 of
                    3 -> [Position char1 2, Position char1 3]
                    4 -> [Position char1 2, Position char1 3, Position char1 4]
                _ -> [Position char1 y1, Position char1 y2]

        else [Position char1 y1,Position char2 y2]

caminhoPeaoPreto :: Position -> Position -> [Position]
caminhoPeaoPreto (Position char y1) (Position char2 y2) = 
    if char == char2
        then
            case y1 of
                7 -> case y2 of
                    6 -> [Position char 7, Position char 6]
                    5 -> [Position char 7, Position char 6, Position char 5]
                _ -> [Position char y1, Position char y2]
        else [Position char y1, Position char2 y2]

caminhoBispo :: Position -> Position -> [Position]
caminhoBispo (Position char1 y1) (Position char2 y2) = 
    [Position (converteIntEmColuna x) y |
                  x <- [min x1 x2 .. max x1 x2]
                , y <- [min y1 y2 .. max y1 y2]
                , abs (x - x1) == abs (y - y1)]
    where
        x1 = converteColunaEmInt char1
        x2 = converteColunaEmInt char2

--movimentos ja validados... nada pode bloquear o cavalo
caminhoCavalo :: Position -> Position -> [Position]
caminhoCavalo p1 p2 = [p1, p2]

caminhoTorre :: Position -> Position -> [Position]
caminhoTorre (Position char1 y1) (Position char2 y2) = 
    [Position (converteIntEmColuna x) y | 
        x <- [min x1 x2 .. max x1 x2], 
        y <- [min y1 y2 .. max y1 y2], 
        (x == x1 || y == y1)]
    where
        x1 = converteColunaEmInt char1
        x2 = converteColunaEmInt char2

caminhoRainha :: Position -> Position -> [Position]
caminhoRainha (Position char1 y1) (Position char2 y2) = 
    if char1 == char2 || y1 == y2 --mesma linha ou mesma coluna
        then caminhoTorre (Position char1 y1) (Position char2 y2)
    else --nao eh nem mesma linha nem mesma coluna -> alguma diagonal
        caminhoBispo (Position char1 y1) (Position char2 y2)

--dado que a validade do movimento ja foi verificada
-- necessariamente o input eh alguma casa adjacente
-- portanto o caminho do rei eh diretamente sua posicao ++ posicao final
caminhoRei :: Position -> Position -> [Position]
caminhoRei p1 p2 = [p1, p2]

-- retorna TRUE -> OK se cores diferentes
validaComerPropriaPeca :: Board -> Position -> Position -> Bool
validaComerPropriaPeca board p1 p2 
    = quadrado2 == Empty || (getCor peca1 /= getCor peca2)
    where
        peca1 = getPiece $ quadrado1
        peca2 = getPiece $ quadrado2
        quadrado1 = getSquare board p1
        quadrado2 = getSquare board p2


-- Cheque?
-- Peca Encravada ?
--En Passant
validaCasosEspeciais :: Board -> Position -> Position -> Bool
validaCasosEspeciais _ _ _ = True 

-- TODO: validar se nao eh roque invalido como?
ehRoque :: Board -> Position -> Position -> Bool
ehRoque board (Position char1 y1) (Position char2 y2) 
    = char1 == 'E' && (piece == King White || piece == King Black ) && quadradoDestino /= Empty &&
        case cor of
            White -> y1 == 1 && y2 == 1 && pieceDestino == Rook White &&
                case char2 of
                    'A' -> foldr ((&&) . (== Empty)) True meioRoqueBrancoGrande
                    'H' -> foldr ((&&) . (== Empty)) True meioRoqueBrancoCurto
            Black -> y1 == 8 && y2 == 8 && pieceDestino == Rook Black &&
                case char2 of
                    'A' -> foldr ((&&) . (== Empty)) True meioRoquePretoGrande
                    'H' -> foldr ((&&) . (== Empty)) True meioRoquePretoCurto

    where
        x1 = converteColunaEmInt char1
        x2 = converteColunaEmInt char2
        piece = getPiece $ getSquare board (Position char1 y1)
        cor = getCor piece
        quadradoDestino = getSquare board (Position char2 y2)
        pieceDestino = getPiece $ getSquare board (Position char2 y2)
        meioRoqueBrancoGrande = [
            getSquare board (Position 'B' 1),
            getSquare board (Position 'C' 1),
            getSquare board (Position 'D' 1)
            ]
        meioRoqueBrancoCurto = [
            getSquare board (Position 'F' 1),
            getSquare board (Position 'G' 1)
            ]
        meioRoquePretoGrande = [
            getSquare board (Position 'B' 8),
            getSquare board (Position 'C' 8),
            getSquare board (Position 'D' 8)
            ]
        meioRoquePretoCurto = [
            getSquare board (Position 'F' 8),
            getSquare board (Position 'G' 8)
            ]


ehPromocao :: Board -> Position -> Position -> Bool
ehPromocao board (Position char1 y1) (Position char2 y2) 
    = (piece == Pawn White || piece == Pawn Black)
      && quadradoDestino == Empty
      && ((piece == Pawn White && y2 == 8) || (piece == Pawn Black && y2 == 1))
    where
        piece = getPiece $ getSquare board (Position char1 y1)
        quadradoDestino = getSquare board (Position char2 y2)

--Encontra o rei de uma certa cor
whereIsKing :: Board -> Color -> Position
whereIsKing board color = head [Position (converteIntEmColuna(y + 1))  (8 - x) | x <- [0..((length board) - 1)], y <- [0..((length (board!!x))-1)], board!!x!!y == Occupied (King color)]

-- Retorna se todos os movimentos que saem do check
runOutcheckMate :: Board -> Player -> [(Position, (Position, Piece))] -> [(Piece, String)]
runOutcheckMate _ _ [] = []
runOutcheckMate board player allMovies = [ (snd (snd x), (positionToInput  (fst (snd x)) (fst x))) | x <- allMovies, not (check (movePiece board (positionToInput  (fst (snd x)) (fst x))) player)]

check :: Board -> Player -> Bool
check board player = isCheck (whichGiveCheck board (whereIsKing board  (playerColor player)) (allPlayerPiecesPositions board (playerColor (nextPlayer player))))

movePiece :: Board -> String -> Board
movePiece board (x0:x1:x2:x3:x4) = [[if x == y3 && y == y2 then returnSquare board (x0:x1:x2:x3:x4) 
            else if x == y1 && y == y0 then Empty else board!!x!!y |
            y <- [0..((length (board!!x))-1)]]| x <- [0..((length board) - 1)]]
  where y0 = (digitToInt (chr ((ord (toUpper x0)) - 17)))
        y1 = 8 - (digitToInt x1)
        y2 = (digitToInt (chr ((ord (toUpper x2)) - 17))) 
        y3 = 8 - (digitToInt x3)

 -- Se tiver alguma peça que pode atacar o rei
isCheck :: [Piece] -> Bool
isCheck [] = False
isCheck pieces = True

--Retorna todas as peças que dao check
whichGiveCheck :: Board -> Position -> [(Position, Piece)] -> [Piece]
whichGiveCheck board kingPos otherPlayerPieces = [ snd x  | x <- otherPlayerPieces, validaMovimento (snd x) (fst x) kingPos && validaInterposicao board (fst x) kingPos && validaComerPropriaPeca board (fst x) kingPos ]

--Retorna uma lista de tuplas com a posição e peças de uma das cores
allPlayerPiecesPositions :: Board -> Color -> [(Position,  Piece)]
allPlayerPiecesPositions board color = [(Position (converteIntEmColuna(y + 1))  (8 - x), getPiece (board!!x!!y))  | x <- [0..((length board) - 1)], y <- [0..((length (board!!x))-1)], board!!x!!y /= Empty && verifyColorSquare (getPiece (board!!x!!y)) color]

-- Retorna todo od movimentos validos do usuário
allMoviesFromUser :: Board -> [(Position,  Piece)] -> [(Position, (Position,  Piece))]
allMoviesFromUser _ [] = []
allMoviesFromUser board (x0:xs) = allMoviesFromPiece board x0 ++ allMoviesFromUser board xs

-- Todos os movimentos validos de uma peça
allMoviesFromPiece :: Board -> (Position,  Piece) -> [(Position, (Position,  Piece))]
allMoviesFromPiece board (position, piece)  = [ (x, (position, piece)) | x <- geraMovimentos piece position, validaMovimento piece position x 
                                                                                                            && validaInterposicao board position x && validaComerPropriaPeca board position x]
  
