module TiposBase( Piece(..), Color(..), Square(..), Board(..), validaMovimento ) where

data PieceState = Piece Bool

data Piece = King Color | Queen Color | Rook Color
            | Bishop Color | Knight Color | Pawn Color deriving (Show)


data Color = Black | White deriving (Show)
data Position = Position Char Int
data Square = Empty | Occupied Piece deriving (Show)
type Board = [[Square]]

validaMovimento :: Piece -> Position -> Position -> Bool
validaMovimento peca p1 p2 = True

validaPeao :: Color -> Position -> Position -> Bool
validaPeao = undefined
