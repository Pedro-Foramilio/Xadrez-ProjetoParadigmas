module TiposBase( Piece(..), Color(..), Square(..), Board(..), Position(..)
    ,validaMovimento, getSquare, getPiece, getCor, geraCaminho, validaInterposicao, 
     validaComerPropriaPeca, validaCasosEspeciais, ehRoque ) where

import Prelude

data PieceState = Piece Bool

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

geraMovimentos :: Piece -> Position -> [Position]
geraMovimentos (Pawn cor)   p = geraMovimentosPeao p cor
geraMovimentos (Knight _)   p = geraMovimentosCavalo p
geraMovimentos (Bishop _)   p = geraMovimentosBispo p
geraMovimentos (Rook _)     p = geraMovimentosTorre p
geraMovimentos (Queen _)    p = geraMovimentosRainha p
geraMovimentos (King _)     p = geraMovimentosRei p

-- TODO? considerar captura diagnoal??
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


--Nao deixar peao comer na frente
-- Peca Encravada
--En Passant
validaCasosEspeciais :: Board -> Position -> Position -> Bool
validaCasosEspeciais _ _ _ = True 

-- TODO: validar se nao eh roque invalido como?
ehRoque :: Board -> Position -> Position -> Bool
ehRoque board (Position char1 y1) (Position char2 y2) 
    = char1 == 'E' && (piece == King White || piece == King Black ) &&
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


        
