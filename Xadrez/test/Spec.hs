import           Test.Tasty
import           Test.Tasty.HUnit

import TiposBase
import IA

pBranco = Pawn White
pPreto  = Pawn Black
nBranco = Knight White
nPreto = Knight Black
bBranco = Bishop White
bPreto = Bishop Black
rBranco = Rook White
rPreto = Rook Black
qBranco = Queen White
qPreto = Queen Black
kBranco = King White
kPreto = King Black

initialBoard :: Board
initialBoard =
  [ [ Occupied (Rook Black), Occupied (Knight Black), Occupied (Bishop Black), Occupied (Queen Black), Occupied (King Black), Occupied (Bishop Black), Occupied (Knight Black), Occupied (Rook Black) ]
  , [ Occupied (Pawn Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black),  Occupied (Pawn Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black) ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [ Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),   Occupied (Pawn White),  Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),   Occupied (Pawn White) ]
  , [ Occupied (Rook White), Occupied (Knight White), Occupied (Bishop White), Occupied (Queen White), Occupied (King White), Occupied (Bishop White), Occupied (Knight White), Occupied (Rook White) ]
  ]

main :: IO ()
main = do
    gerarMovimentoPretas initialBoard
    --defaultMain tests
{-
tests :: TestTree
tests = testGroup "Testes Relevantes" [ testePeaoInicioFim, testeCavaloInicioFim, testeBispoInicioFim
                                      ,testeTorreInicioFim, testeRainhaInicioFim, testeReiInicioFim  
                                      , testesGetPieceFromSquare, testesGeraCaminho, testesInterposicao]

testePeaoInicioFim = testGroup
            "Jogadas Peoes - Coordenadas"
            [
                testCase
                    "Peao Branco B2 B4 == True"
                    (assertEqual "" True (validaMovimento pBranco (Position 'B' 2) (Position 'B' 4)))
                , testCase
                    "Peao Branco C2 C3 == True"
                    (assertEqual "" True (validaMovimento pBranco (Position 'C' 2) (Position 'C' 3)))
                , testCase
                    "Peao Branco C8 C7 == False"
                    (assertEqual "" False (validaMovimento pBranco (Position 'C' 8) (Position 'C' 7)))
                , testCase
                    "Peao Branco C4 B5 == True"
                    (assertEqual "" True (validaMovimento pBranco (Position 'C' 4) (Position 'B' 5)))
                , testCase
                    "Peao Preto D7 D5 == True"
                    (assertEqual "" True (validaMovimento pPreto (Position 'D' 7) (Position 'D' 5)))
                , testCase
                    "Peao Preto E7 E6 == True"
                    (assertEqual "" True (validaMovimento pPreto (Position 'E' 7) (Position 'E' 6)))
                , testCase
                    "Peao Branco A2 A3 == False"
                    (assertEqual "" False (validaMovimento pPreto (Position 'A' 2) (Position 'A' 3)))
                , testCase
                    "Peao Branco C5 B4 == True"
                    (assertEqual "" True (validaMovimento pPreto (Position 'C' 5) (Position 'B' 4)))
            ]

testeCavaloInicioFim = testGroup
            "Jogadas Cavalo - Coordenadas"
            [
                testCase
                    "Cavalo C2 D4 == True"
                    (assertEqual "" True (validaMovimento nBranco (Position 'C' 2) (Position 'D' 4)))
                , testCase
                    "Cavalo D2 F3 == True"
                    (assertEqual "" True (validaMovimento nPreto (Position 'D' 2) (Position 'F' 3)))
                , testCase
                    "Cavalo G7 H9 == False"
                    (assertEqual "" False (validaMovimento nBranco (Position 'G' 7) (Position 'H' 9)))
                , testCase
                    "Cavalo F4 G2 == True"
                    (assertEqual "" True (validaMovimento nPreto (Position 'F' 4) (Position 'G' 2)))
                , testCase
                    "Cavalo E3 C2 == True"
                    (assertEqual "" True (validaMovimento nBranco (Position 'E' 3) (Position 'C' 2)))
                , testCase
                    "Cavalo G7 H6 == False"
                    (assertEqual "" False (validaMovimento nPreto (Position 'G' 7) (Position 'H' 6)))
                , testCase
                    "Cavalo F5 G4 == False"
                    (assertEqual "" False (validaMovimento nBranco (Position 'F' 5) (Position 'G' 4)))
            ]

testeBispoInicioFim = testGroup
            "Jogadas Bispo - Coordenadas"
            [
                testCase
                    "Bispo D4 F6 == True"
                    (assertEqual "" True (validaMovimento bBranco (Position 'D' 4) (Position 'F' 6)))
                , testCase
                    "Bispo E4 B1 == True"
                    (assertEqual "" True (validaMovimento bPreto (Position 'E' 4) (Position 'B' 1)))
                , testCase
                    "Bispo F1 A6 == True"
                    (assertEqual "" True (validaMovimento bBranco (Position 'F' 1) (Position 'A' 6)))
                , testCase
                    "Bispo A8 H1 == True"
                    (assertEqual "" True (validaMovimento bPreto (Position 'A' 8) (Position 'H' 1)))
                , testCase
                    "Bispo A8 H2 == False"
                    (assertEqual "" False (validaMovimento bPreto (Position 'A' 8) (Position 'H' 2)))
                , testCase
                    "Bispo F1 G3 == False"
                    (assertEqual "" False (validaMovimento bPreto (Position 'F' 1) (Position 'G' 3)))
            ]

testeTorreInicioFim = testGroup
            "Jogadas Torre - Coordenadas"
            [
                testCase
                    "Torre D4 D8 == True"
                    (assertEqual "" True (validaMovimento rBranco (Position 'D' 4) (Position 'D' 8)))
                , testCase
                    "Torre F4 H4 == True"
                    (assertEqual "" True (validaMovimento rPreto (Position 'F' 4) (Position 'H' 4)))
                , testCase
                    "Torre B7 A7 == True"
                    (assertEqual "" True (validaMovimento rBranco (Position 'B' 7) (Position 'A' 7)))
                , testCase
                    "Torre C5 C2 == True"
                    (assertEqual "" True (validaMovimento rPreto (Position 'C' 5) (Position 'C' 2)))
                , testCase
                    "Torre C5 C3 == True"
                    (assertEqual "" True (validaMovimento rBranco (Position 'C' 5) (Position 'C' 3)))
            ]

testeRainhaInicioFim = testGroup
            "Jogadas Rainha - Coordenadas"
            [
                testCase
                    "Rainha D4 B6 == True"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'B' 6)))
                , testCase
                    "Rainha D4 A4 == True"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'A' 4)))
                , testCase
                    "Rainha D4 B2 == True"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'B' 2)))
                , testCase
                    "Rainha D4 D3 == True"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'D' 3)))
                , testCase
                    "Rainha D4 F2 == True"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'F' 2)))
                , testCase
                    "Rainha D4 G4 == True"
                    (assertEqual "" True (validaMovimento qPreto (Position 'D' 4) (Position 'G' 4)))
                , testCase
                    "Rainha D4 E5 == True"
                    (assertEqual "" True (validaMovimento qPreto (Position 'D' 4) (Position 'E' 5)))
                , testCase
                    "Rainha D4 D8 == True"
                    (assertEqual "" True (validaMovimento qPreto (Position 'D' 4) (Position 'D' 8)))
                , testCase
                    "Rainha D4 E6 == False"
                    (assertEqual "" False (validaMovimento qPreto (Position 'D' 4) (Position 'E' 6)))
            ]

testeReiInicioFim = testGroup
            "Jogadas Rei - Coordenadas"
            [
                testCase
                    "Rei D7 D6 == True"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'D' 6)))
                , testCase
                    "Rei D7 E6 == True"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'E' 6)))
                , testCase
                    "Rei D7 C6 == True"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'C' 6)))
                , testCase
                    "Rei D7 C7 == True"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'C' 7)))
                , testCase
                    "Rei D7 C8 == True"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'C' 8)))
                , testCase
                    "Rei D7 D8 == True"
                    (assertEqual "" True (validaMovimento kPreto (Position 'D' 7) (Position 'D' 8)))
                , testCase
                    "Rei D7 E7 == True"
                    (assertEqual "" True (validaMovimento kPreto (Position 'D' 7) (Position 'E' 7)))
                , testCase
                    "Rei D7 E8 == True"
                    (assertEqual "" True (validaMovimento kPreto (Position 'D' 7) (Position 'E' 8)))
                , testCase
                    "Rei D7 E5 == False"
                    (assertEqual "" False (validaMovimento kPreto (Position 'D' 7) (Position 'E' 5)))
                , testCase
                    "Rei D7 B7 == False"
                    (assertEqual "" False (validaMovimento kPreto (Position 'D' 7) (Position 'B' 7)))
                , testCase
                    "Rei D7 F5 == False"
                    (assertEqual "" False (validaMovimento kPreto (Position 'D' 7) (Position 'F' 5)))
            ]


initialBoard :: Board
initialBoard =
  [ [ Occupied (Rook Black), Occupied (Knight Black), Occupied (Bishop Black), Occupied (Queen Black), Occupied (King Black), Occupied (Bishop Black), Occupied (Knight Black), Occupied (Rook Black) ]
  , [ Occupied (Pawn Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black),  Occupied (Pawn Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black) ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [ Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),   Occupied (Pawn White),  Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),   Occupied (Pawn White) ]
  , [ Occupied (Rook White), Occupied (Knight White), Occupied (Bishop White), Occupied (Queen White), Occupied (King White), Occupied (Bishop White), Occupied (Knight White), Occupied (Rook White) ]
  ]

testesGetPieceFromSquare = testGroup
            "Testes Get Piece From Board"
            [
                  testCase
                    "Get initialBoard Position A 2 == Pawn White"
                    (assertEqual "" (Pawn White) 
                        (getPiece $ getSquare initialBoard (Position 'A' 2)))
                , testCase
                    "Get initialBoard Position A 1 == Rook White"
                    (assertEqual "" (Rook White) 
                        (getPiece $ getSquare initialBoard (Position 'A' 1)))
                , testCase
                    "Get initialBoard Position A 8 == Rook Black"
                    (assertEqual "" (Rook Black) 
                        (getPiece $ getSquare initialBoard (Position 'A' 8)))
                , testCase
                    "Get initialBoard Position H 1 == Rook White"
                    (assertEqual "" (Rook White) 
                        (getPiece $ getSquare initialBoard (Position 'H' 1)))
                , testCase
                    "Get initialBoard Position H 8 == Rook Black"
                    (assertEqual "" (Rook Black) 
                        (getPiece $ getSquare initialBoard (Position 'H' 8)))
                , testCase
                    "Get initialBoard Position C 1 == Bishop White"
                    (assertEqual "" (Bishop White) 
                        (getPiece $ getSquare initialBoard (Position 'C' 1)))
                , testCase
                    "Get initialBoard Position E 8 == Black King"
                    (assertEqual "" (King Black) 
                        (getPiece $ getSquare initialBoard (Position 'E' 8)))
            ]

someBoard :: Board
someBoard = 
  [ [ Occupied (Rook Black), Occupied (Knight Black), Occupied (Bishop Black), Occupied (Queen Black), Occupied (King Black), Occupied (Bishop Black), Occupied (Knight Black), Occupied (Rook Black) ]
  , [Occupied (Queen Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black),  Occupied (Pawn Black), Occupied (Pawn Black),   Occupied (Pawn Black),   Occupied (Pawn Black) ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,                   Empty          ]
  , [        Empty,                 Empty,                   Empty,                   Empty,                  Empty,                 Empty,                   Empty,            Occupied (Pawn Black) ]
  , [Occupied (Bishop White), Occupied (Rook White),   Occupied (Pawn White),   Occupied (Pawn White),  Occupied (Pawn White), Occupied (Pawn White),   Occupied (Pawn White),  Occupied (Pawn White) ]
  , [ Occupied (Rook White), Occupied (Knight White), Occupied (Bishop White), Occupied (Queen White), Occupied (King White), Occupied (Bishop White), Occupied (Knight White), Occupied (Rook White) ]
  ]

testesGeraCaminho = testGroup
            "Testes Gerador de Caminho"
            [
                  testCase
                    "Caminho Bispo A2 <-> F7 == [A2 B3 C4 D5 E6 F7]"
                    (assertEqual "" [Position 'A' 2, Position 'B' 3, Position 'C' 4,
                                     Position 'D' 5, Position 'E' 6, Position 'F' 7] 
                        (geraCaminho (Bishop White) (Position 'A' 2) (Position 'F' 7)))
                , testCase
                    "Caminho Bispo A2 <-> E6 == [A2 B3 C4 D5 E6]"
                    (assertEqual "" [Position 'A' 2, Position 'B' 3, Position 'C' 4,
                                     Position 'D' 5, Position 'E' 6] 
                        (geraCaminho (Bishop White) (Position 'A' 2) (Position 'E' 6)))
                , testCase
                    "Caminho Torre B2 <-> B7 == [B2 B3 B4 B5 B6 B7]"
                    (assertEqual "" [Position 'B' 2, Position 'B' 3, Position 'B' 4,
                                     Position 'B' 5, Position 'B' 6, Position 'B' 7] 
                        (geraCaminho (Rook White) (Position 'B' 2) (Position 'B' 7)))
                , testCase
                    "Caminho Torre B2 <-> B6 == [B2 B3 B4 B5 B6]"
                    (assertEqual "" [Position 'B' 2, Position 'B' 3, Position 'B' 4,
                                     Position 'B' 5, Position 'B' 6] 
                        (geraCaminho (Rook White) (Position 'B' 2) (Position 'B' 6)))
                , testCase
                    "Caminho Dama A7 <-> A2 == [A2 A3 A4 A5 A6 A7]"
                    (assertEqual "" [Position 'A' 2, Position 'A' 3, Position 'A' 4,
                                     Position 'A' 5, Position 'A' 6, Position 'A' 7] 
                        (geraCaminho (Queen White) (Position 'A' 2) (Position 'A' 7)))
                , testCase
                    "Caminho Dama A7 <-> C5 == [A7 B6 C5]"
                    (assertEqual "" [Position 'A' 7, Position 'B' 6, Position 'C' 5] 
                        (geraCaminho (Queen White) (Position 'A' 7) (Position 'C' 5)))
                , testCase
                    "Caminho Peao B2 <-> B4 == [B2 B3 B4]"
                    (assertEqual "" [Position 'B' 2, Position 'B' 3, Position 'B' 4] 
                        (geraCaminho (Pawn White) (Position 'B' 2) (Position 'B' 4)))
                , testCase
                    "Caminho Peao B2 <-> B3 == [B2 B3]"
                    (assertEqual "" [Position 'B' 2, Position 'B' 3] 
                        (geraCaminho (Pawn White) (Position 'B' 2) (Position 'B' 3)))
                , testCase
                    "Caminho Peao B7 <-> B5 == [B7 B6 B5]"
                    (assertEqual "" [Position 'B' 7, Position 'B' 6, Position 'B' 5] 
                        (geraCaminho (Pawn Black) (Position 'B' 7) (Position 'B' 5)))
                , testCase
                    "Caminho Peao B7 <-> B6 == [B7 B6]"
                    (assertEqual "" [Position 'B' 7, Position 'B' 6] 
                        (geraCaminho (Pawn Black) (Position 'B' 7) (Position 'B' 6)))
                , testCase
                    "Caminho Peao C5 <-> C6 == [C5 C6]"
                    (assertEqual "" [Position 'C' 5, Position 'C' 6] 
                        (geraCaminho (Pawn White) (Position 'C' 5) (Position 'C' 6)))
            ]
testesInterposicao = testGroup
            "Testes Interposicao"
            [
                  testCase
                    "Valida Bispo A2 <-> F7 == True"
                    (assertEqual "" True (validaInterposicao someBoard (Position 'A' 2) (Position 'F' 7)))
                , testCase
                    "Valida Bispo A2 <-> E6 == True"
                    (assertEqual "" True (validaInterposicao someBoard (Position 'A' 2) (Position 'E' 6)))
                , testCase
                    "Valida Torre B2 <-> B7 == True"
                    (assertEqual "" True (validaInterposicao someBoard (Position 'B' 2) (Position 'B' 7)))
                , testCase
                    "Valida Torre B2 <-> B6 == True"
                    (assertEqual "" True (validaInterposicao someBoard (Position 'B' 2) (Position 'B' 6)))
                , testCase
                    "Valida Dama A7 <-> A2 == True"
                    (assertEqual "" True (validaInterposicao someBoard (Position 'A' 2) (Position 'A' 7)))
                , testCase
                    "Valida Dama A7 <-> C5 == True"
                    (assertEqual "" True (validaInterposicao someBoard (Position 'A' 7) (Position 'C' 5)))
                , testCase
                    "Valida Torre A1 <-> A3 == False"
                    (assertEqual "" False (validaInterposicao someBoard (Position 'A' 1) (Position 'A' 3)))
                , testCase
                    "Valida Bispo A2 <-> G8 == False"
                    (assertEqual "" False (validaInterposicao someBoard (Position 'A' 2) (Position 'G' 8)))
                , testCase
                    "Valida Peao B2 <-> B4 == True"
                    (assertEqual "" True (validaInterposicao someBoard (Position 'B' 2) (Position 'B' 4)))
                , testCase
                    "Valida Peao H2 <-> H4 == False"
                    (assertEqual "" False (validaInterposicao someBoard (Position 'H' 2) (Position 'H' 4)))
            ]
-}