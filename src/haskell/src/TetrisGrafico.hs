module TetrisGrafico where

import Control.Monad
import Data.Char
import Data.List
import System.Random
import Tetris
import Text.Printf
import UI.NCurses

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
      desenhar:: Maybe Bloco -> Update()
      desenhar (Just (Bloco I _ _)) = desenharBloco vermelho
      desenhar (Just (Bloco S _ _)) = desenharBloco verde
      desenhar (Just (Bloco O _ _)) = desenharBloco azul
      desenhar (Just (Bloco T _ _)) = desenharBloco amarelo
      desenhar (Just (Bloco Z _ _)) = desenharBloco ciano
      desenhar (Just (Bloco J _ _)) = desenharBloco branco
      desenhar (Just (Bloco L _ _)) = desenharBloco magenta
      desenhar Nothing = desenharBloco corGrade

      desenharBlocos :: Grade -> Update()
      desenharBlocos [] = return ()
      desenharBlocos l@(h:t) = do
        when (length l <= fromIntegral fileiras) $ desenharLinha h y
        desenharBlocos t
        where
          y = (gradeY+fileiras)- toInteger (length t)

      desenharLinha :: Fileira -> Integer -> Update()
      desenharLinha [] _ = return ()
      desenharLinha (h:t) y = do
        let x = colunas - (toInteger (length bloco) * toInteger (length t))
        moveCursor y $ gradeX + x + colunas
        desenhar h
        desenharLinha t y

      desenharFimDeJogo  :: Update()
      desenharFimDeJogo  = do
        moveCursor (gradeY + quot fileiras 2) (gradeX + 8)
        setColor corTexto
        drawString "         "
        moveCursor (gradeY + quot fileiras 2 + 1) (gradeX + 2)
        drawString "    FIM DO JOGO!   "
        moveCursor (gradeY + quot fileiras 2 + 2) (gradeX + 2)
        drawString " 'r' para reiniciar "

      mostrarPontuacao :: Int -> Update()
      mostrarPontuacao scoreValue = do
        moveCursor (gradeY - 1) (gradeX + 1)
        setColor corTexto
        let scorestr = show scoreValue
        drawString ("Pontuação: " ++ scorestr)

      drawHighScores :: [Int] -> Update ()
      drawHighScores scores = setColor corTexto >> forM_ (zip [1..] scores) mostrarMaiorPontuacao

      drawLevel :: Int -> Update()
      drawLevel level = do
        moveCursor (gradeY - 1) (gradeX + 15)
        setColor corTexto
        drawString ("Nível: " ++ show level)

      levelMenu = do
        setColor corTexto
        drawString "                    "
        moveCursor (gradeY + quot fileiras 2 + 1) (gradeX + 2)
        drawString "    Choose level:   "
        moveCursor (gradeY + quot fileiras 2 + 2) (gradeX + 2)
        drawString "        0-9         "

      clearStats = do
        moveCursor (gradeY - 1) (gradeX + 1)
        setColor corGrade
        drawString "                      "

      atualizaTela :: Grade -> Int -> StdGen -> Int -> [Int] -> Bool -> Curses [Int]
      atualizaTela gameState currentScore gen lvl highScores updatable = do
        let
          gameEnded = fimDeJogo gameState
          newHighScores
            | gameEnded && updatable = take 5 . reverse . sort $ currentScore : highScores
            | otherwise = highScores
          newUpd = not gameEnded
        updateWindow janela $ do
          desenharBlocos gameState
          mostrarPontuacao currentScore
          drawLevel lvl
          when gameEnded desenharFimDeJogo 
          drawHighScores newHighScores
        render
        ev <- getEvent janela (Just ((1+(9-toInteger lvl))*100))
        case ev of
          Nothing -> atualizaTela state newScore gen' lvl newHighScores newUpd
          Just ev'
            | ev' == EventCharacter 'q' -> return newHighScores
            | ev' == EventCharacter 'a' -> atualizaTela (moverEsquerda state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 'd' -> atualizaTela (moverDireita state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 's' -> atualizaTela (acelerar state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 'w' -> atualizaTela (rotacionar state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter ' ' -> atualizaTela (descerBloco state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 'r' -> game newHighScores
            | otherwise -> atualizaTela state newScore gen' lvl newHighScores newUpd
        where
          (nextshape, gen') = formaAleatoria gen
          state = atualizarTela gameState nextshape
          newScore = currentScore + (pontuacao gameState*(1+lvl))

      game :: [Int] -> Curses [Int]
      game scores = do
        updateWindow janela $ desenharGrade gradeY gradeX corGrade
        updateWindow janela levelMenu
        updateWindow janela clearStats
        updateWindow janela $ drawHighScores scores
        render
        ev <- getEvent janela Nothing
        case ev of
          Nothing -> game scores
          Just (EventCharacter c)
            | isNumber c -> atualizaTela novoJogo 0 g (digitToInt c) scores True
            | c == 'q' -> return scores
          Just _ -> game scores

  _ <- setCursorMode CursorInvisible
  setEcho False
  game placar

desenharBloco :: ColorID -> Update()
desenharBloco color = do
  setColor color
  drawString bloco

desenharGrade :: Integer -> Integer -> ColorID -> Update()
desenharGrade y x c = do
  setColor c
  moveCursor y (x+1)
  drawString gradeTopo
  desenharLinhas (y+1) (x+1)
  moveCursor (fileiras+y+1) (x+1)
  drawString gradeBaixo

desenharLinhas :: Integer -> Integer -> Update()
desenharLinhas y x = desenharLinhas' y x fileiras

desenharLinhas' :: Integer -> Integer -> Integer -> Update()
desenharLinhas' y x n
  | n < 1 = return()
  | otherwise = do
      moveCursor y x
      drawString gradeMeio
      desenharLinhas' (y+1) x (n-1)

mostrarMaiorPontuacao :: (Integer, Int) -> Update ()
mostrarMaiorPontuacao (i, s) = do
  moveCursor (gradeY + fileiras + 1 + i) (gradeX + 6)
  drawString $ printf "%d.%10d" i s

gradeTopo, gradeMeio, gradeBaixo :: String
gradeTopo    = " ____________________ "
gradeMeio = "|                    |"
gradeBaixo = " -------------------- "

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
