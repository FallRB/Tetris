import System.Directory
import TetrisGrafico

main:: IO()
main = do
  pontuacao <- getPontuacao
  pontuacaoNova <- playGame pontuacao
  guardarPontuacao pontuacaoNova

getPontuacao:: IO [Int]
getPontuacao = do
  temArquivo <- doesFileExist arquivoPontuacao
  if not temArquivo then return [] else calcularPontuacao <$> readFile arquivoPontuacao

guardarPontuacao:: [Int] -> IO()
guardarPontuacao = writeFile arquivoPontuacao . show . take pontuacaoMaxima . filter (> 0)

arquivoPontuacao:: FilePath
arquivoPontuacao = "pontuacao"

pontuacaoMaxima:: Int
pontuacaoMaxima = 10

calcularPontuacao:: String -> [Int]
calcularPontuacao pont
  | null lerPontuacao = []
  | otherwise = fst . head $ lerPontuacao
  where
    lerPontuacao = reads pont
