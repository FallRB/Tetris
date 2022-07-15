module TetrisGrafico where

-- permite mapear valores de a em b respeitando a estrutura da função geradora
import Control.Monad
import Data.List
import System.Random
-- importando a parte lógica do jogo
import Tetris
-- permite a manipulação de strings com maior facilidade
import Text.Printf
-- permite a criação de uma interface pseudo-gráfica, mais agradável ao usuário
import UI.NCurses

-- definições a serem utilizadas no código
bloco :: String
bloco = " ."

gradeX :: Integer
gradeX = 50

gradeY :: Integer
gradeY = 4

fileiras :: Integer
fileiras = toInteger (length novoJogo - 4)

colunas :: Integer
colunas = toInteger (length (head novoJogo))

-- função que inicia o jogo, faz conexão com o main
iniciarJogo:: [Int] -> IO [Int]
iniciarJogo placar = newStdGen >>= \g -> runCurses $ do
  janela <- defaultWindow
  corGrade <- newColorID ColorBlue ColorDefault 1
  vermelho <- newColorID ColorRed ColorRed 2
  verde <- newColorID ColorGreen ColorGreen 3
  azul <- newColorID ColorBlue ColorBlue 4
  amarelo <- newColorID ColorYellow ColorYellow 5
  ciano <- newColorID ColorCyan ColorCyan 6
  branco <- newColorID ColorWhite ColorWhite 7
  magenta <- newColorID ColorMagenta ColorMagenta 8
  corTexto <- newColorID ColorRed ColorDefault 9
  let
      -- desenha os formatos de blocos e define as cores dos blocos
      desenhar:: Maybe Bloco -> Update()
      desenhar (Just (Bloco I _ _)) = desenharBloco vermelho
      desenhar (Just (Bloco S _ _)) = desenharBloco verde
      desenhar (Just (Bloco O _ _)) = desenharBloco azul
      desenhar (Just (Bloco T _ _)) = desenharBloco amarelo
      desenhar (Just (Bloco Z _ _)) = desenharBloco ciano
      desenhar (Just (Bloco J _ _)) = desenharBloco branco
      desenhar (Just (Bloco L _ _)) = desenharBloco magenta
      desenhar Nothing = desenharBloco corGrade

      -- desenha um bloco
      desenharBlocos:: Grade -> Update()
      desenharBlocos [] = return ()
      desenharBlocos l@(cabeca:cauda) = do
        when (length l <= fromIntegral fileiras) $ desenharLinha cabeca y
        desenharBlocos cauda
        where
          y = (gradeY+fileiras) - toInteger (length cauda)

      -- desenha em uma fileira recebida
      desenharLinha :: Fileira -> Integer -> Update()
      desenharLinha [] _ = return ()
      desenharLinha (cabeca:cauda) y = do
        let x = colunas - (toInteger (length bloco) * toInteger (length cauda))
        moveCursor y $ gradeX + x + colunas
        desenhar cabeca
        desenharLinha cauda y

      -- mostra a mensagem de fim de jogo
      desenharFimDeJogo:: Update()
      desenharFimDeJogo = do
        moveCursor (gradeY + quot fileiras 2) (gradeX + 8)
        setColor corTexto
        drawString "         "
        moveCursor (gradeY + quot fileiras 2 + 1) (gradeX + 2)
        drawString "    FIM DO JOGO!   "
        moveCursor (gradeY + quot fileiras 2 + 2) (gradeX + 2)
        drawString " 'r' para reiniciar "

      -- desenha a mensagem de pontuação acima do jogo
      mostrarPontuacao:: Int -> Update()
      mostrarPontuacao scoreValue = do
        moveCursor (gradeY - 1) (gradeX + 1)
        setColor corTexto
        let scorestr = show scoreValue
        drawString ("Pontuação: " ++ scorestr)

      -- recebe o placar e o coloca no formato certo para ser impresso na tela
      desenharPlacar:: [Int] -> Update ()
      desenharPlacar scores = setColor corTexto >> forM_ (zip [1..] scores) mostrarPlacar

      menuInicial = do
        setColor corTexto
        drawString "                    "
        moveCursor (gradeY + quot fileiras 2 + 1) (gradeX + 2)
        drawString " 's' para começar"

      limparPontuacao = do
        moveCursor (gradeY - 1) (gradeX + 1)
        setColor corGrade
        drawString "                      "

      -- atualiza os componentes da tela a cada tecla apertada ou segundo passado
      atualizaTela :: Grade -> Int -> StdGen -> [Int] -> Bool -> Curses [Int]
      atualizaTela gameState currentScore gen highScores updatable = do
        let
          gameEnded = fimDeJogo gameState
          newHighScores
            | gameEnded && updatable = take 5 . reverse . sort $ currentScore : highScores
            | otherwise = highScores
          newUpd = not gameEnded
        updateWindow janela $ do
          desenharBlocos gameState
          mostrarPontuacao currentScore
          when gameEnded desenharFimDeJogo
          desenharPlacar newHighScores
        render
        ev <- getEvent janela (Just (1000)) -- timeout: 1000ms = 1 seg
        case ev of
          Nothing -> atualizaTela state newScore gen' newHighScores newUpd
          Just ev'
            | ev' == EventCharacter 'q' -> return newHighScores
            | ev' == EventCharacter 'a' -> atualizaTela (moverEsquerda state) newScore gen'  newHighScores newUpd
            | ev' == EventCharacter 'd' -> atualizaTela (moverDireita state) newScore gen'  newHighScores newUpd
            | ev' == EventCharacter 's' -> atualizaTela (acelerar state) newScore gen'  newHighScores newUpd
            | ev' == EventCharacter 'w' -> atualizaTela (rotacionar state) newScore gen'  newHighScores newUpd
            | ev' == EventCharacter ' ' -> atualizaTela (descerBloco state) newScore gen'  newHighScores newUpd
            | ev' == EventCharacter 'r' -> game newHighScores
            | otherwise -> atualizaTela state newScore gen' newHighScores newUpd
        where
          (nextshape, gen') = formaAleatoria gen
          state = atualizar gameState nextshape
          newScore = currentScore + (pontuacao gameState)

      -- executa o jogo
      game :: [Int] -> Curses [Int]
      game scores = do
        updateWindow janela $ desenharGrade gradeY gradeX corGrade
        updateWindow janela menuInicial
        updateWindow janela limparPontuacao
        updateWindow janela $ desenharPlacar scores
        render
        ev <- getEvent janela Nothing
        case ev of
          Nothing -> game scores
          Just (EventCharacter c)
            | c == 's' -> atualizaTela novoJogo 0 g scores True
            | c == 'q' -> return scores
          Just _ -> game scores

  _ <- setCursorMode CursorInvisible
  setEcho False
  game placar

-- desenha o bloco preenchido na cor recebida como parametro
desenharBloco :: ColorID -> Update()
desenharBloco color = do
  setColor color
  drawString bloco

-- desenha a grade (tela/background) do jogo
desenharGrade :: Integer -> Integer -> ColorID -> Update()
desenharGrade y x c = do
  setColor c
  moveCursor y (x+1)
  drawString gradeTopo
  desenharLinhas (y+1) (x+1)
  moveCursor (fileiras+y+1) (x+1)
  drawString gradeBaixo

-- usa a função auxiliar desenharLinhas' para desenhar as laterais da 'tela' do jogo
desenharLinhas :: Integer -> Integer -> Update()
desenharLinhas y x = desenharLinhas' y x fileiras

-- desenha as laterais da 'tela' linha por linha
desenharLinhas' :: Integer -> Integer -> Integer -> Update()
desenharLinhas' y x n
  | n < 1 = return()
  | otherwise = do
      moveCursor y x
      drawString gradeMeio
      desenharLinhas' (y+1) x (n-1)

-- imprime na tela o placar do jogo
mostrarPlacar:: (Integer, Int) -> Update ()
mostrarPlacar (i, s) = do
  moveCursor (gradeY + fileiras + 1 + i) (gradeX + 6)
  drawString $ printf "%d.%10d" i s

-- criando o formato da 'tela' do jogo
gradeTopo, gradeMeio, gradeBaixo :: String
gradeTopo    = " _______TETRIS_______ "
gradeMeio = "!                    !"
gradeBaixo = " -------------------- "
