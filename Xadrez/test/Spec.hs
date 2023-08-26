import           Test.Tasty
import           Test.Tasty.HUnit

import TiposBase

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


main :: IO ()
main = do
    defaultMain tests

tests :: TestTree
tests = testGroup "Testes Relevantes" [ testePeaoInicioFim, testeCavaloInicioFim, testeBispoInicioFim
                                      ,testeTorreInicioFim, testeRainhaInicioFim, testeReiInicioFim  ]

testePeaoInicioFim = testGroup
            "Jogadas Peoes - Coordenadas"
            [
                testCase
                    "Peao Branco B2 B4"
                    (assertEqual "" True (validaMovimento pBranco (Position 'B' 2) (Position 'B' 4)))
                , testCase
                    "Peao Branco C2 C3"
                    (assertEqual "" True (validaMovimento pBranco (Position 'C' 2) (Position 'C' 3)))
                , testCase
                    "Peao Branco C8 C7"
                    (assertEqual "" False (validaMovimento pBranco (Position 'C' 8) (Position 'C' 7)))
                , testCase
                    "Peao Branco C4 B5"
                    (assertEqual "" False (validaMovimento pBranco (Position 'C' 4) (Position 'B' 5)))
                , testCase
                    "Peao Preto D7 D5"
                    (assertEqual "" True (validaMovimento pPreto (Position 'D' 7) (Position 'D' 5)))
                , testCase
                    "Peao Preto E7 E6"
                    (assertEqual "" True (validaMovimento pPreto (Position 'E' 7) (Position 'E' 6)))
                , testCase
                    "Peao Branco A2 A3"
                    (assertEqual "" False (validaMovimento pPreto (Position 'A' 2) (Position 'A' 3)))
                , testCase
                    "Peao Branco C5 B4"
                    (assertEqual "" False (validaMovimento pPreto (Position 'C' 5) (Position 'B' 4)))
            ]

testeCavaloInicioFim = testGroup
            "Jogadas Cavalo - Coordenadas"
            [
                testCase
                    "Cavalo C2 D4"
                    (assertEqual "" True (validaMovimento nBranco (Position 'C' 2) (Position 'D' 4)))
                , testCase
                    "Cavalo D2 F3"
                    (assertEqual "" True (validaMovimento nPreto (Position 'D' 2) (Position 'F' 3)))
                , testCase
                    "Cavalo G7 H9"
                    (assertEqual "" False (validaMovimento nBranco (Position 'G' 7) (Position 'H' 9)))
                , testCase
                    "Cavalo F4 G2"
                    (assertEqual "" True (validaMovimento nPreto (Position 'F' 4) (Position 'G' 2)))
                , testCase
                    "Cavalo E3 C2"
                    (assertEqual "" True (validaMovimento nBranco (Position 'E' 3) (Position 'C' 2)))
                , testCase
                    "Cavalo G7 H6"
                    (assertEqual "" False (validaMovimento nPreto (Position 'G' 7) (Position 'H' 6)))
                , testCase
                    "Cavalo F5 G4"
                    (assertEqual "" False (validaMovimento nBranco (Position 'F' 5) (Position 'G' 4)))
            ]

testeBispoInicioFim = testGroup
            "Jogadas Bispo - Coordenadas"
            [
                testCase
                    "Bispo D4 F6"
                    (assertEqual "" True (validaMovimento bBranco (Position 'D' 4) (Position 'F' 6)))
                , testCase
                    "Bispo E4 B1"
                    (assertEqual "" True (validaMovimento bPreto (Position 'E' 4) (Position 'B' 1)))
                , testCase
                    "Bispo F1 A6"
                    (assertEqual "" True (validaMovimento bBranco (Position 'F' 1) (Position 'A' 6)))
                , testCase
                    "Bispo A8 H1"
                    (assertEqual "" True (validaMovimento bPreto (Position 'A' 8) (Position 'H' 1)))
                , testCase
                    "Bispo A8 H2"
                    (assertEqual "" False (validaMovimento bPreto (Position 'A' 8) (Position 'H' 2)))
                , testCase
                    "Bispo F1 G3"
                    (assertEqual "" False (validaMovimento bPreto (Position 'F' 1) (Position 'G' 3)))
            ]

testeTorreInicioFim = testGroup
            "Jogadas Torre - Coordenadas"
            [
                testCase
                    "Torre D4 D8"
                    (assertEqual "" True (validaMovimento rBranco (Position 'D' 4) (Position 'D' 8)))
                , testCase
                    "Torre F4 H4"
                    (assertEqual "" True (validaMovimento rPreto (Position 'F' 4) (Position 'H' 4)))
                , testCase
                    "Torre B7 A7"
                    (assertEqual "" True (validaMovimento rBranco (Position 'B' 7) (Position 'A' 7)))
                , testCase
                    "Torre C5 C2"
                    (assertEqual "" True (validaMovimento rPreto (Position 'C' 5) (Position 'C' 2)))
                , testCase
                    "Torre C5 C3"
                    (assertEqual "" True (validaMovimento rBranco (Position 'C' 5) (Position 'C' 3)))
            ]

testeRainhaInicioFim = testGroup
            "Jogadas Rainha - Coordenadas"
            [
                testCase
                    "Rainha D4 B6"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'B' 6)))
                , testCase
                    "Rainha D4 A4"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'A' 4)))
                , testCase
                    "Rainha D4 B2"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'B' 2)))
                , testCase
                    "Rainha D4 D3"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'D' 3)))
                , testCase
                    "Rainha D4 F2"
                    (assertEqual "" True (validaMovimento qBranco (Position 'D' 4) (Position 'F' 2)))
                , testCase
                    "Rainha D4 G4"
                    (assertEqual "" True (validaMovimento qPreto (Position 'D' 4) (Position 'G' 4)))
                , testCase
                    "Rainha D4 E5"
                    (assertEqual "" True (validaMovimento qPreto (Position 'D' 4) (Position 'E' 5)))
                , testCase
                    "Rainha D4 D8"
                    (assertEqual "" True (validaMovimento qPreto (Position 'D' 4) (Position 'D' 8)))
                , testCase
                    "Rainha D4 E6"
                    (assertEqual "" False (validaMovimento qPreto (Position 'D' 4) (Position 'E' 6)))
            ]

testeReiInicioFim = testGroup
            "Jogadas Rei - Coordenadas"
            [
                testCase
                    "Rei D7 D6"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'D' 6)))
                , testCase
                    "Rei D7 E6"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'E' 6)))
                , testCase
                    "Rei D7 C6"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'C' 6)))
                , testCase
                    "Rei D7 C7"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'C' 7)))
                , testCase
                    "Rei D7 C8"
                    (assertEqual "" True (validaMovimento kBranco (Position 'D' 7) (Position 'C' 8)))
                , testCase
                    "Rei D7 D8"
                    (assertEqual "" True (validaMovimento kPreto (Position 'D' 7) (Position 'D' 8)))
                , testCase
                    "Rei D7 E7"
                    (assertEqual "" True (validaMovimento kPreto (Position 'D' 7) (Position 'E' 7)))
                , testCase
                    "Rei D7 E8"
                    (assertEqual "" True (validaMovimento kPreto (Position 'D' 7) (Position 'E' 8)))
                , testCase
                    "Rei D7 E5"
                    (assertEqual "" False (validaMovimento kPreto (Position 'D' 7) (Position 'E' 5)))
                , testCase
                    "Rei D7 B7"
                    (assertEqual "" False (validaMovimento kPreto (Position 'D' 7) (Position 'B' 7)))
                , testCase
                    "Rei D7 F5"
                    (assertEqual "" False (validaMovimento kPreto (Position 'D' 7) (Position 'F' 5)))
            ]

