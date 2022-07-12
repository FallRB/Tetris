module TetrisGrafico where

import Control.Monad
--import Data.Char
import Data.List
import System.Random
import Tetris
import Text.Printf
import UI.NCurses

iniciarJogo:: [Int] -> IO [Int]
iniciarJogo placar = newStdGen >>= \g -> runCurses $ do
  janela <- defaultWindow
  corGrade <- newColorID ColorBlue ColorDefault 1
  azul <- newColorID ColorBlue ColorBlue 2
  vermelho <- newColorID ColorRed ColorRed 3
  amarelo <- newColorID ColorYellow ColorYellow 4
  ciano <- newColorID ColorCyan ColorCyan 5
  verde <- newColorID ColorGreen ColorGreen 6
  rosa <- newColorID ColorMagenta ColorMagenta 7
  branco <- newColorID ColorWhite ColorWhite 8
  corTexto <- newColorID ColorRed ColorDefault 9
  let
      desenhar:: Maybe Bloco -> Update()
      desenhar (Just (Bloco I _ _)) = desenharBloco azul
      desenhar (Just (Bloco S _ _)) = desenharBloco vermelho
      desenhar (Just (Bloco O _ _)) = desenharBloco amarelo
      desenhar (Just (Bloco T _ _)) = desenharBloco ciano
      desenhar (Just (Bloco Z _ _)) = desenharBloco verde
      desenhar (Just (Bloco J _ _)) = desenharBloco rosa
      desenhar (Just (Bloco L _ _)) = desenharBloco branco
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

      mostrarFimDeJogo :: Update()
      mostrarFimDeJogo = do
        moveCursor (gradeY + quot fileiras 2) (gradeX + 8)
        setColor corTexto
        drawString "         "
        moveCursor (gradeY + quot fileiras 2 + 1) (gradeX + 2)
        drawString "     FIM DE JOGO!    "
        moveCursor (gradeY + quot fileiras 2 + 2) (gradeX + 2)
        drawString " 'r' para recomeçar "

      mostrarPontos :: Int -> Update()
      mostrarPontos scoreValue = do
        moveCursor (gradeY - 1) (gradeX + 1)
        setColor corTexto
        let scorestr = show scoreValue
        drawString ("Pontuação: " ++ scorestr)

      mostrarMaiorPontuacao :: [Int] -> Update ()
      mostrarMaiorPontuacao pontos = setColor corTexto >> forM_ (zip [1..] pontos) mostrarMaiorPontuacao

      mostrarNivel :: Int -> Update()
      mostrarNivel level = do
        moveCursor (gradeY - 1) (gradeX + 15)
        setColor corTexto

      levelMenu = do
        setColor corTexto
        drawString "                    "
        moveCursor (gradeY + quot fileiras 2 + 1) (gradeX + 2)
        drawString " 's' para começar"

      limparStatus = do
        moveCursor (gradeY - 1) (gradeX + 1)
        setColor corGrade
        drawString "                      "

      atualizarTela :: Grade -> Int -> StdGen -> Int -> [Int] -> Bool -> Curses [Int]
      atualizarTela estadoJogo pontuacaoAtual gen highScores updatable = do
        let
          finalDeJogo = fimDeJogo estadoJogo
          novoMaxPontos
            | finalDeJogo && updatable = take 5 . reverse . sort $ pontuacaoAtual : highScores
            | otherwise = highScores
          novaAtualizacao = not finalDeJogo
        atualizarTela janela $ do
          desenharBlocos estadoJogo
          mostrarPontos pontuacaoAtual
          mostrarNivel
          when finalDeJogo mostrarFimDeJogo
          mostrarMaiorPontuacao novoMaxPontos
        render
        ev <- getEvent janela (Just ((10)*100))
        case ev of
          Nothing -> atualizarTela estado novaPontuacao gen' novoMaxPontos novaAtualizacao
          Just ev'
            | ev' == EventCharacter 'q' -> return novoMaxPontos
            | ev' == EventCharacter 'a' -> atualizarTela (moverEsquerda estado) novaPontuacao gen' novoMaxPontos novaAtualizacao
            | ev' == EventCharacter 'd' -> atualizarTela (moverDireita estado) novaPontuacao gen' novoMaxPontos novaAtualizacao
            | ev' == EventCharacter 's' -> atualizarTela (acelerar estado) novaPontuacao gen' novoMaxPontos novaAtualizacao
            | ev' == EventCharacter 'w' -> atualizarTela (rotacionar estado) novaPontuacao gen' novoMaxPontos novaAtualizacao
            | ev' == EventCharacter ' ' -> atualizarTela (descerBloco estado) novaPontuacao gen' novoMaxPontos novaAtualizacao
            | ev' == EventCharacter 'r' -> jogo novoMaxPontos
            | otherwise -> atualizarTela estado novaPontuacao gen' novoMaxPontos novaAtualizacao
        where
          (nextshape, gen') = formaAleatoria gen
          estado = atualizarTela estadoJogo nextshape
          novaPontuacao = pontuacaoAtual + (pontuacao estadoJogo)

      jogo :: [Int] -> Curses [Int]
      jogo pontos = do
        atualizarTela janela $ desenharGrade gradeY gradeX corGrade
        atualizarTela janela levelMenu
        atualizarTela janela limparStatus
        atualizarTela janela $ mostrarMaiorPontuacao pontos
        render
        ev <- getEvent janela Nothing
        case ev of
          Nothing -> jogo pontos
          Just (EventCharacter c)
            | c == 's' -> atualizarTela novoJogo 0 g pontos True
            | c == 'q' -> return pontos
          Just _ -> jogo pontos

  _ <- setCursorMode CursorInvisible
  setEcho False
  jogo placar

desenharBloco :: ColorID -> Update()
desenharBloco cor = do
  setColor cor
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
gradeTopo = " _______TETRIS_______ "
gradeMeio = "!                    !"
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
