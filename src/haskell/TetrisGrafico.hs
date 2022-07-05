module TetrisGrafico where

import Control.Monad
import Data.Char
import Data.List
import System.Random
import Tetris
import Text.Printf
import UI.NCurses

iniciarUmJogo :: [Int] -> IO [Int]
iniciarUmJogo pontuaçãoMaisAlta = newStdGen >>= \g -> executar $ do
  w <- defaultWindow
  gridcolor <- corId ColorBlue ColorDefault 1
  vermelho <- corId ColorRed ColorRed 2
  verde <- corId ColorGreen ColorGreen 3
  azul <- corId ColorBlue ColorBlue 4
  amarelo <- corId ColorYellow ColorYellow 5
  roxo <- corId ColorPurple ColorPurple 6
  branco <- corId ColorWhite ColorWhite 7
  rosa <- corId ColorMagenta ColorMagenta 8
  txtVermelho <- corId ColorRed ColorDefault 9
  let
      draw :: Maybe Block -> Update()
      draw (Just (Block I _ _)) = construirBlocos vermelho
      draw (Just (Block S _ _)) = construirBlocos verde
      draw (Just (Block O _ _)) = construirBlocos azul
      draw (Just (Block T _ _)) = construirBlocos amarelo
      draw (Just (Block Z _ _)) = construirBlocos roxo
      draw (Just (Block J _ _)) = construirBlocos branco
      draw (Just (Block L _ _)) = construirBlocos rosa
      draw Nothing = construirBlocos gridcolor

      construirBlocos :: Grid -> Update()
      construirBlocos [] = return ()
      construirBlocos l@(h:t) = do
        when (length l <= fromIntegral rows) $ construirLinhas h y
        construirBlocos t
        where
          y = (gridY+rows)- toInteger (length t)

      construirLinhas :: Row -> Integer -> Update()
      construirLinhas [] _ = return ()
      construirLinhas (h:t) y = do
        let x = coluna - (toInteger (length block) * toInteger (length t))
        moveCursor y $ gridX + x + coluna
        draw h
        construirLinhas t y

      fimDeJogo :: Update()
      fimDeJogo = do
        moveCursor (gridY + quot rows 2) (gridX + 8)
        setColor txtVermelho
        drawString "         "
        moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
        drawString "     FIM DE JOGO!     "
        moveCursor (gridY + quot rows 2 + 2) (gridX + 2)
        drawString " pressione r para tentar novamente "

      pontuacao :: Int -> Update()
      pontuacao numPomtuacao = do
        moveCursor (gridY - 1) (gridX + 1)
        setColor gridcolor
        let scorestr = show numPontuacao
        drawString ("Pontuacao: " ++ scorestr)

      drawHighScores :: [Int] -> Update ()
      drawHighScores pontuacoes = setColor gridcolor >> forM_ (zip [1..] pontuacoes) drawHighScore
      drawLevel :: Int -> Update()
      drawLevel nivel = do
        moveCursor (gridY - 1) (gridX + 15)
        setColor gridcolor
        drawString ("Nivel: " ++ show level)

      levelMenu = do
        setColor txtVermelho
        drawString "                    "
        moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
        drawString "    Escolha o nivel:   "
        moveCursor (gridY + quot rows 2 + 2) (gridX + 2)
        drawString "        0-9         "

      clearStats = do
        moveCursor (gridY - 1) (gridX + 1)
        setColor gridcolor
        drawString "                      "
