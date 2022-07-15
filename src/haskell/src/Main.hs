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
  if not temArquivo then return [] else converterPontuacao <$> readFile arquivoPontuacao

-- escreve a pontuação em um arquivo para guardá-la
guardarPontuacao:: [Int] -> IO()
guardarPontuacao = writeFile arquivoPontuacao . show . take limiteRanking . filter (> 0)

-- filepath do arquivo que guarda a pontuação
arquivoPontuacao:: FilePath
arquivoPontuacao = "pontuacao"

-- limite do ranking
limiteRanking:: Int
limiteRanking = 10

-- converte a pontuação do arquivo para uma lista de inteiro
converterPontuacao:: String -> [Int]
converterPontuacao pont
  | null lerPontuacao = []
  | otherwise = fst . head $ lerPontuacao
  where
    lerPontuacao = reads pont
