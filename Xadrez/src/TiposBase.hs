module TiposBase( Piece(..), Color(..), Square(..), Board(..), Position(..), validaMovimento ) where
import Prelude

data PieceState = Piece Bool

data Piece = King Color | Queen Color | Rook Color
            | Bishop Color | Knight Color | Pawn Color deriving (Show)

data Color = Black | White deriving (Show)
data Position = Position Char Int
data Square = Empty | Occupied Piece deriving (Show)
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

isInBoard :: Position -> Bool
isInBoard (Position i y) =  x >= 1 && x <= 8 && y >= 1 && y <= 8 
    where x = converteColunaEmInt i

validaMovimento :: Piece -> Position -> Position -> Bool
validaMovimento (Pawn cor) p1 p2 = isInBoard p2 && validaPeao cor p1 p2
validaMovimento (Knight _) p1 p2 = isInBoard p2 && validaCavalo p1 p2
validaMovimento (Bishop _) p1 p2 = isInBoard p2 && validaBispo p1 p2
validaMovimento (Rook _)   p1 p2 = isInBoard p2 && validaTorre p1 p2
validaMovimento (Queen _)  p1 p2 = isInBoard p2 && validaRainha p1 p2
validaMovimento (King _)   p1 p2 = isInBoard p2 && validaRei p1 p2

validaPeao :: Color -> Position -> Position -> Bool
validaPeao cor (Position char1 y1) (Position char2 y2) 
    | x1 == -1 || x2 == -1 = False
    | otherwise = case cor of
                    White -> if y1 == 2
                                then x2 == x1 && (y2 == 3 || y2 == 4)
                                else x2 == x1 && y2 == (y1 + 1)
                    Black -> if y1 == 7
                                then x2 == x1 && (y2 == 6 || y2 == 5)
                                else x2 == x1 && y2 == (y1-1)
    where x1 = converteColunaEmInt char1
          x2 = converteColunaEmInt char2

validaCavalo :: Position -> Position -> Bool
validaCavalo (Position char1 y1) (Position char2 y2) 
    = (converteColunaEmInt char2, y2) `elem` movimentosPossiveis
    where
        offsets = [ (2, -1),  (2, 1),
                      (1, -2),  (1, 2),
                     (-1, -2), (-1, 2),
                     (-2, -1), (-2, 1)]
        x1 = converteColunaEmInt char1
        movimentos = map (\(x, y) -> (x + x1, y + y1)) offsets
        movimentosPossiveis = filter 
                                    (\(x, y) -> x >= 1 && x <=8 && y >= 1 && y <= 8)
                                    movimentos

validaBispo :: Position -> Position -> Bool
validaBispo (Position char1 y1) (Position char2 y2)
    = (x2, y2) `elem` movimentosPossiveis
    where
        x1 = converteColunaEmInt char1
        x2 = converteColunaEmInt char2
        offsets = [(x, x) | x <- [-7..7], x /= 0] ++ [(-x, x) | x <- [-7..7], x /= 0]
        movimentos = map (\(x, y) -> (x1 + x, y1 + y)) offsets                       
        movimentosPossiveis = filter 
                                (\(x, y) -> x >= 1 && x <=8 && y >= 1 && y <= 8)
                                movimentos

validaTorre :: Position -> Position -> Bool
validaTorre (Position char1 y1) (Position char2 y2)
    = char1 == char2 || y2 == y1 --ou varia coluna com linha fixa, ou varia linha com coluna fixa
          
validaRainha :: Position -> Position -> Bool
validaRainha p1 p2 = validaBispo p1 p2 || validaTorre p1 p2

validaRei :: Position -> Position -> Bool
validaRei (Position char1 y1) (Position char2 y2) = (x2, y2) `elem` movimentosPossiveis 
    where x1 = converteColunaEmInt char1
          x2 = converteColunaEmInt char2
          offsets = [(-1,  1), (0,  1), (1,  1),
                     (-1,  0),          (1,  0),
                     (-1, -1), (0, -1), (1, -1) 
                    ]
          movimentos = map (\(x, y) -> (x1 + x, y1 + y)) offsets
          movimentosPossiveis = filter 
                                    (\(x, y) -> x >= 1 && x <=8 && y >= 1 && y <= 8)
                                    movimentos

