module TetrisCurses where

import Control.Monad
import Data.Char
import Data.List
import System.Random
import Tetris
import Text.Printf
import UI.NCurses

jogarOJogo :: [Int] -> IO [Int]
jogarOJogo theHighScores = newStdGen >>= \g -> runCurses $ do
  w <- defaultWindow
  gridcolor <- newColorID ColorBlue ColorDefault 1
  azul <- newColorID ColorBlue ColorBlue 2
  vermelho <- newColorID ColorRed ColorRed 3
  amarelo <- newColorID ColorYellow ColorYellow 4
  ciano <- newColorID ColorCyan ColorCyan 5
  verde <- newColorID ColorGreen ColorGreenn 6
  magenta <- newColorID ColorMagenta ColorMagenta 7
  branco <- newColorID ColorWhite ColorWhite 8
  textoEmVermelho <- newColorID ColorRed ColorDefault 9
  let
      draw :: Maybe Bloco -> Update()
      draw (Just (Bloco I _ _)) = drawBlock azul
      draw (Just (Bloco S _ _)) = drawBlock vermelho
      draw (Just (Bloco O _ _)) = drawBlock amarelo
      draw (Just (Bloco T _ _)) = drawBlock ciano
      draw (Just (Bloco Z _ _)) = drawBlock verde
      draw (Just (Bloco J _ _)) = drawBlock magenta
      draw (Just (Bloco L _ _)) = drawBlock branco
      draw Nothing = drawBlock gridcolor

      drawBlocks :: Grade -> Update()
      drawBlocks [] = return ()
      drawBlocks l@(h:t) = do
        when (length l <= fromIntegral rows) $ drawLine h y
        drawBlocks t
        where
          y = (gridY+rows)- toInteger (length t)

      drawLine :: Fileira -> Integer -> Update()
      drawLine [] _ = return ()
      drawLine (h:t) y = do
        let x = columns - (toInteger (length block) * toInteger (length t))
        moveCursor y $ gridX + x + columns
        draw h
        drawLine t y

      drawGameOver :: Update()
      drawGameOver = do
        moveCursor (gridY + quot rows 2) (gridX + 8)
        setColor textoEmVermelho
        drawString "         "
        moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
        drawString "     GAME OVER!     "
        moveCursor (gridY + quot rows 2 + 2) (gridX + 2)
        drawString " press 'r' to retry "

      drawScore :: Int -> Update()
      drawScore scoreValue = do
        moveCursor (gridY - 1) (gridX + 1)
        setColor gridcolor
        let scorestr = show scoreValue
        drawString ("Score: " ++ scorestr)

      drawHighScores :: [Int] -> Update ()
      drawHighScores scores = setColor gridcolor >> forM_ (zip [1..] scores) drawHighScore

      drawLevel :: Update()
      drawLevel = do
        moveCursor (gridY - 1) (gridX + 15)
        setColor gridcolor

      levelMenu = do
        setColor gridcolor
        drawString "                    "
        moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
        drawString "    Press 'S' to Start Game:   "

      clearStats = do
        moveCursor (gridY - 1) (gridX + 1)
        setColor gridcolor
        drawString "                      "

      updateScreen :: Grade -> Int -> StdGen -> [Int] -> Bool -> Curses [Int]
      updateScreen gameState currentScore gen highScores updatable = do
        let
          gameEnded = fimDeJogo gameState
          newHighScores
            | gameEnded && updatable = take 5 . reverse . sort $ currentScore : highScores
            | otherwise = highScores
          newUpd = not gameEnded
        updateWindow w $ do
          drawBlocks gameState
          drawScore currentScore
          drawLevel
          when gameEnded drawGameOver
          drawHighScores newHighScores
        render
        ev <- getEvent w (Just ((1+(9-toInteger lvl))*100))
        case ev of
          Nothing -> updateScreen state newScore gen' newHighScores newUpd
          Just ev'
            | ev' == EventCharacter 'q' -> return newHighScores
            | ev' == EventSpecialKey KeyLeftArrow -> updateScreen (moverEsquerda state) newScore gen' newHighScores newUpd
            | ev' == EventSpecialKey KeyRightArrow -> updateScreen (moverDireita state) newScore gen' newHighScores newUpd
            | ev' == EventSpecialKey KeyDownArrow -> updateScreen (acelerar state) newScore gen' newHighScores newUpd
            | ev' == EventSpecialKey KeyUpArrow -> updateScreen (rotacionar state) newScore gen' newHighScores newUpd
            | ev' == EventCharacter ' ' -> updateScreen (descerBloco state) newScore gen' newHighScores newUpd
            | ev' == EventCharacter 'r' -> game newHighScores
            | otherwise -> updateScreen state newScore gen' newHighScores newUpd
        where
          (nextshape, gen') = formaAleatoria gen
          state = atualizarTela gameState nextshape
          newScore = currentScore + (pontuacao gameState*(1+lvl))

      game :: [Int] -> Curses [Int]
      game scores = do
        updateWindow w $ drawGrid gridY gridX gridcolor
        updateWindow w levelMenu
        updateWindow w clearStats
        updateWindow w $ drawHighScores scores
        render
        ev <- getEvent w Nothing
        case ev of
          Nothing -> game scores
          Just (EventCharacter c) --continuar
            | c == 's' -> updateScreen novoJogo 0 g (digitToInt c) scores True
            | c == 'q' -> return scores
          Just _ -> game scores

  _ <- setCursorMode CursorInvisible
  setEcho False
  game theHighScores

drawBlock :: ColorID -> Update()
drawBlock color = do
  setColor color
  drawString block

drawGrid :: Integer -> Integer -> ColorID -> Update()
drawGrid y x c = do
  setColor c
  moveCursor y (x+1)
  drawString gridTop
  drawLines (y+1) (x+1)
  moveCursor (rows+y+1) (x+1)
  drawString gridBottom

drawLines :: Integer -> Integer -> Update()
drawLines y x = drawLines' y x rows

drawLines' :: Integer -> Integer -> Integer -> Update()
drawLines' y x n
  | n < 1 = return()
  | otherwise = do
      moveCursor y x
      drawString gridMiddle
      drawLines' (y+1) x (n-1)

drawHighScore :: (Integer, Int) -> Update ()
drawHighScore (i, s) = do
  moveCursor (gridY + rows + 1 + i) (gridX + 6)
  drawString $ printf "%d.%10d" i s

gridTop, gridMiddle, gridBottom :: String
gridTop    = " ******************** "
gridMiddle = "*                    *"
gridBottom = " ******************** "

block :: String
block = " ."

gridX :: Integer
gridX = 50

gridY :: Integer
gridY = 4

rows :: Integer
rows = toInteger (length novoJogo - 4)

columns :: Integer
columns = toInteger (length (head novoJogo))
