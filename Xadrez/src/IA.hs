module IA(gerarMovimentoPretas) where

import TiposBase


gerarMovimentoPretas :: Board -> IO String
gerarMovimentoPretas board = do
        let pecasAtuais = getPecasAtuais board Black
        if check board Player2
            then do
                let movimentosPossiveis = runOutcheckMate board Player2 (allMoviesFromUser board (allPlayerPiecesPositions board Black))
                return $ snd $ head movimentosPossiveis
        else do
            let a = runOutcheckMate board Player2 (allMoviesFromUser board (allPlayerPiecesPositions board Black))
            let movimentosPossiveis = geraMovimentosParaPecasPretas board pecasAtuais
            let pecasQueAlcancamMaiorPeso = maiorPesoAlcansavelAbsoluto movimentosPossiveis
            let primeiraPecaQueAlcancaMaiorPeso = head pecasQueAlcancamMaiorPeso
            let (peca, _, _) = primeiraPecaQueAlcancaMaiorPeso
            let pecaDeMenorValor = last $ pecasDeMenorValor pecasQueAlcancamMaiorPeso peca
            let (Position char1 y1, Position char2 y2) = (\(piece, p, ps) -> (p, head ps)) pecaDeMenorValor 
            let final = [char1] ++ show y1 ++ [char2] ++ show y2
            if (check (movePiece board final) Player2)
                then return $ snd $ head a
            else return final


converterPecaEmPontos :: Piece -> Float
converterPecaEmPontos (King   _) = 100
converterPecaEmPontos (Queen  _) = 7
converterPecaEmPontos (Rook   _) = 5
converterPecaEmPontos (Bishop _) = 3
converterPecaEmPontos (Knight _) = 3
converterPecaEmPontos (Pawn   _) = 1

calculaPesoPosicao :: Position -> Float
calculaPesoPosicao (Position charr y) = pesos !! (8 - y) !! (converteColunaEmInt charr - 1)
    where
        x = converteColunaEmInt charr
        pesos = 
            [
                [1.10, 1.15, 1.20, 1.25, 1.25, 1.20, 1.15, 1.10],
                [1.20, 1.25, 1.30, 1.35, 1.35, 1.30, 1.25, 1.20],
                [1.25, 1.30, 1.35, 1.45, 1.45, 1.35, 1.30, 1.25],
                [1.30, 1.35, 1.40, 1.55, 1.55, 1.40, 1.35, 1.30],
                [1.30, 1.35, 1.40, 1.55, 1.55, 1.40, 1.35, 1.30],
                [1.25, 1.30, 1.35, 1.45, 1.45, 1.35, 1.30, 1.25],
                [1.20, 1.25, 1.30, 1.35, 1.35, 1.30, 1.25, 1.20],
                [1.10, 1.15, 1.20, 1.25, 1.25, 1.20, 1.15, 1.10]
            ]

getPecasAtuais :: Board -> Color -> [(Piece, Position)]
getPecasAtuais board color 
    =  [(getPiece $ getSquare board (Position (converteIntEmColuna $ j+1) (8-i)), Position (converteIntEmColuna $ j+1) (8-i) ) 
                | i <- [0..7], j <- [0..7]
                , board!!i!!j /= Empty
                ,getPiece (getSquare board (Position (converteIntEmColuna $ j+1) (8-i))) 
                    `elem` [King color, Queen color, Rook color, Bishop color, Knight color, Pawn color]]


calculaPontuacaoPeca :: (Piece, Position) -> (Float, Float)
calculaPontuacaoPeca (peca, posicao) = ( converterPecaEmPontos peca, calculaPesoPosicao posicao)

geraMovimentosParaPecasPretas :: Board -> [(Piece, Position)] -> [(Piece, Position, [Position])]
geraMovimentosParaPecasPretas board [] = []
geraMovimentosParaPecasPretas board (x:xs) = (peca, pos, movimentosPossiveis) : geraMovimentosParaPecasPretas board xs
    where
        peca = fst x
        pos  = snd x
        movimentosTotais = geraMovimentos peca pos
        movimentosPossiveis = filter (\x -> isInBoard x && validaInterposicao board pos x && not (verificaSeEstaEmCheque board (whereIsKing board Black))) movimentosTotais

retornaPosicaoMaiorPeso :: [Position] -> [Position]
retornaPosicaoMaiorPeso ps = [x | x <- ps, calculaPesoPosicao x == pesoMaximo]
    where
        pesos = map calculaPesoPosicao ps
        pesoMaximo = if null pesos then 0 else maximum pesos

determinaMaiorePesoAlcansavelRelativo :: [(Piece, Position, [Position])] -> [Position]
determinaMaiorePesoAlcansavelRelativo [] = []
determinaMaiorePesoAlcansavelRelativo (x:xs) = posicaoMaiorPeso ++ determinaMaiorePesoAlcansavelRelativo xs
    where
        (peca, p1, ps) = x
        posicaoMaiorPeso = retornaPosicaoMaiorPeso ps

maiorPesoAlcansavelAbsoluto :: [(Piece, Position, [Position])] -> [(Piece, Position, [Position])]
maiorPesoAlcansavelAbsoluto xs = [(piece, pos, filtrar ps) | (piece, pos, ps) <- xs, pesoMaximo `elem` map calculaPesoPosicao ps]
    where
        posicoes = determinaMaiorePesoAlcansavelRelativo xs
        pesos = map calculaPesoPosicao posicoes
        pesoMaximo = if null pesos then 0 else maximum pesos
        filtrar = filter (\p -> calculaPesoPosicao p == pesoMaximo)

pecasDeMenorValor :: [(Piece, Position, [Position])] -> Piece -> [(Piece, Position, [Position])]
pecasDeMenorValor [] _ = []
pecasDeMenorValor (x:xs) p 
    | converterPecaEmPontos x' <= converterPecaEmPontos p = x : pecasDeMenorValor xs x'
    | otherwise = pecasDeMenorValor xs p
    where
        (x', _, _) = x


verificaSeEstaEmCheque :: Board -> Position -> Bool
verificaSeEstaEmCheque board x = isCheck(whichGiveCheck board x (allPlayerPiecesPositions board Black))
