module TetrisGrafico where

import Control.Monad
import Data.Char
import Data.List
import System.Random
import Tetris
import Text.Printf
import UI.NCurses

iniciarJogo :: [Int] -> IO [Int]
iniciarJogo placar = newStdGen >>= \g -> runCurses $ do
  janela <- defaultWindow
  gridcolor <- newColorID ColorBlue ColorDefault 1
  red <- newColorID ColorRed ColorRed 2
  green <- newColorID ColorGreen ColorGreen 3
  blue <- newColorID ColorBlue ColorBlue 4
  yellow <- newColorID ColorYellow ColorYellow 5
  cyan <- newColorID ColorCyan ColorCyan 6
  white <- newColorID ColorWhite ColorWhite 7
  magenta <- newColorID ColorMagenta ColorMagenta 8
  redtext <- newColorID ColorRed ColorDefault 9
  let
      draw :: Maybe Bloco -> Update()
      draw (Just (Bloco I _ _)) = drawBlock red
      draw (Just (Bloco S _ _)) = drawBlock green
      draw (Just (Bloco O _ _)) = drawBlock blue
      draw (Just (Bloco T _ _)) = drawBlock yellow
      draw (Just (Bloco Z _ _)) = drawBlock cyan
      draw (Just (Bloco J _ _)) = drawBlock white
      draw (Just (Bloco L _ _)) = drawBlock magenta
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
        setColor redtext
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

      drawLevel :: Int -> Update()
      drawLevel level = do
        moveCursor (gridY - 1) (gridX + 15)
        setColor gridcolor
        drawString ("Level: " ++ show level)

      levelMenu = do
        setColor redtext
        drawString "                    "
        moveCursor (gridY + quot rows 2 + 1) (gridX + 2)
        drawString "    Choose level:   "
        moveCursor (gridY + quot rows 2 + 2) (gridX + 2)
        drawString "        0-9         "

      clearStats = do
        moveCursor (gridY - 1) (gridX + 1)
        setColor gridcolor
        drawString "                      "

      updateScreen :: Grade -> Int -> StdGen -> Int -> [Int] -> Bool -> Curses [Int]
      updateScreen gameState currentScore gen lvl highScores updatable = do
        let
          gameEnded = fimDeJogo gameState
          newHighScores
            | gameEnded && updatable = take 5 . reverse . sort $ currentScore : highScores
            | otherwise = highScores
          newUpd = not gameEnded
        updateWindow janela $ do
          drawBlocks gameState
          drawScore currentScore
          drawLevel lvl
          when gameEnded drawGameOver
          drawHighScores newHighScores
        render
        ev <- getEvent janela (Just ((1+(9-toInteger lvl))*100))
        case ev of
          Nothing -> updateScreen state newScore gen' lvl newHighScores newUpd
          Just ev'
            | ev' == EventCharacter 'q' -> return newHighScores
            | ev' == EventCharacter 'a' -> updateScreen (moverEsquerda state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 'd' -> updateScreen (moverDireita state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 's' -> updateScreen (acelerar state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 'w' -> updateScreen (rotacionar state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter ' ' -> updateScreen (descerBloco state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 'r' -> game newHighScores
            | otherwise -> updateScreen state newScore gen' lvl newHighScores newUpd
        where
          (nextshape, gen') = formaAleatoria gen
          state = atualizarTela gameState nextshape
          newScore = currentScore + (pontuacao gameState*(1+lvl))

      game :: [Int] -> Curses [Int]
      game scores = do
        updateWindow janela $ drawGrid gridY gridX gridcolor
        updateWindow janela levelMenu
        updateWindow janela clearStats
        updateWindow janela $ drawHighScores scores
        render
        ev <- getEvent janela Nothing
        case ev of
          Nothing -> game scores
          Just (EventCharacter c)
            | isNumber c -> updateScreen novoJogo 0 g (digitToInt c) scores True
            | c == 'q' -> return scores
          Just _ -> game scores

  _ <- setCursorMode CursorInvisible
  setEcho False
  game placar

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
gridTop    = " ____________________ "
gridMiddle = "|                    |"
gridBottom = " -------------------- "

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
