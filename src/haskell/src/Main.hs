module Main where

import System.Directory
import TetrisGrafico

main:: IO()
main = do
  pontuacao <- getPontuacao
  pontuacaoNova <- iniciarJogo pontuacao
  guardarPontuacao pontuacaoNova

-- pega a pontuação do arquivo onde ela fica
getPontuacao:: IO [Int]
getPontuacao = do
  temArquivo <- doesFileExist arquivoPontuacao
  if not temArquivo then return [] else calcularPontuacao <$> readFile arquivoPontuacao

-- escreve a pontuação em um arquivo para guardá-la
guardarPontuacao:: [Int] -> IO()
guardarPontuacao = writeFile arquivoPontuacao . show . take pontuacaoMaxima . filter (> 0)

-- filepath do arquivo que guarda a pontuação
arquivoPontuacao:: FilePath
arquivoPontuacao = "pontuacao"

-- definição da pontuação máxima
pontuacaoMaxima:: Int
pontuacaoMaxima = 10

-- Calcula a pontuação do jogo 
calcularPontuacao:: String -> [Int]
calcularPontuacao pont
  | null lerPontuacao = []
  | otherwise = fst . head $ lerPontuacao
  where
    lerPontuacao = reads pont
