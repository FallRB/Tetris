-- funções a serem criadas
module Tetris(
    novoJogo,
    formaAleatoria,
    atualizarTela,
    adicionarBloco,
    descerBloco,
    acelerar,
    moverDireita,
    moverEsquerda,
    rotacionar,
    pontuacao,
    fimDeJogo,
    Grade,
    Fileira,
    Bloco(..),
    Formato(..)
) where

-- importando bibliotecas
import Data.List
import Data.Maybe
import System.Random

-- dados para formato do bloco
data Formato = J | L | I | S | Z | O | T
            deriving (Eq, Show, Enum)

-- dados para gerar o bloco baseado no formato
data Bloco = Bloco { formato :: Formato, movimento::Bool, origem::Bool}
            deriving (Eq, Show)

-- fileira formada por blocos
type Fileira = [Maybe Bloco]

-- grade formada por fileiras
type Grade = [Fileira]

-- retorna uma grade vazia
novoJogo:: Grade
novoJogo = replicate alturaGrade (replicate larguraGrade Nothing)

-- retorna uma tupla contendo um formato aleatório e um gerador
formaAleatoria:: RandomGen g => g -> (Shape, g)
formaAleatoria g = case randomR(0, length [J ..]-1) g of (r,g') -> (toEnum r, g')

-- atualiza o estado atual da grade pela "gravidade" limpando as linhas e parando os blocos
atualizarTela:: Grade -> Formato -> Grade
atualizarTela = adicionarBloco . gravidade . limparLinhas . pararBlocos

-- adiciona um bloco no topo da grade
adicionarBloco:: Grade -> Formato -> Grade
adicionarBloco fileiras formato'
  | vazio fileiras && not (fimDeJogo fileira) = criarFormato formato' ++ drop 4  fileiras
  | otherwise = fileiras

-- faz o bloco descer a grade
descerBloco:: Grade -> Grade
descerBloco fileiras
  | crescer /= fileiras = descerBloco crescer
  | otherwise = fileiras
  where
    crescer = gravidade fileiras

-- acelera a "gravidade"
acelerar:: Grade -> Grade
acelerar = gravidade

-- move o bloco para direita
moverDireita:: Grade -> Grade
moverDireita fileiras
  | toqueDireita fileiras = fileiras
  | otherwise = transpor . gravidade . transpor $ fileiras

-- move o bloco para direita
moverEsquerda:: Grade -> Grade
moverEsquerda fileiras
  | toqueEsquerda fileiras = fileiras
  | otherwise = map reverse . transpor . gravidade . transpor . map reverse $ fileiras

-- checa se o bloco toca a parede da direita
toqueDireita:: Grade -> Bool
toqueDireita = any movimento . mapMaybe last

-- checa se o bloco toca a parede da esquerda
toqueEsquerda:: Grade -> Bool
toqueEsquerda = any movimento . mapMaybe head

-- rotaciona o bloco no sentido horário
rotacionar:: Grade -> Grade
rotacionar g = girar (limparGrade g) (rotacionarBloco g) (map (getBloco g) (coordenadas g))

-- recebe a grade, as coordenadas e o bloco, e retorna a grade atualizada
girar:: Grade -> [(Int, Int)] -> [Maybe Bloco] -> Grid
girar grade [] _ = grade
girar grade (cabeca:cauda) (cabeca_v:cauda_v) = girar (setBloco grade cabeca cabeca_v) cauda cauda_v
girar _ (_:_) [] = error "Isso não pode acontecer"

-- limpar Grade
limparGrade:: Grade -> Grade
limparGrade grade = limparGrade' grade $coordenadas grade

limparGrade':: Grade -> [(Int, Int)] -> Grade
limparGrade' = foldl (\grade cabeca -> setBloco grade cabeca Nothing)

-- retorna as coordenadas
coordenadas:: Grade -> [(Int, Int)]
coordenadas [] = []
coordenadas (cabeca: cauda) = coordenadas' cabeca (25 - length cauda) ++ coordenadas cauda

coordenadas':: Fileiras -> Int -> [(Int, Int)]
coordenadas' [] = []
coordenadas' (cabeca:cauda) y
  | movimentaBloco cabeca = (y, 9 - length cauda): coordenadas' cauda y
  | otherwise = coordenadas' cauda y

-- retorna a origem
getOrigem:: Grade -> (Int, Int)
getOrigem = head . origens 

-- retorna se é ou não origem
ehOrigem:: Grade -> (Int, Int) -> Bool
ehOrigem grade (x, y) = maybe False origem $ getBloco grade (x, y)

-- retorna as coordenadas de origem
origens:: Grade -> [(Int, Int)]
origens grade = filter (ehOrigem grade) (coordenadas grade)

rotacionarBloco:: Grade -> [(Int,Int)]
rotacionarBloco grade
  | temOrigem grade && all (estaDisponivel grade) rotacionado = rotacionado
  | otherwise = coordenadasMovimento
  where
    coordenadasMovimento = coordenadas grade
    rotacionado = map (rotacionarPonto $ getOrigem grade) coordenadasMovimento

rotacionarPonto:: (Int,Int) -> (Int,Int) -> (Int,Int)
rotacionarPonto (origemX,origemY) (destinoX,destinoY) = (origemX + origemY - destinoY, origemY - origemX + destinoX)

temOrigem:: Grade -> Bool
temOrigem = not . null . origens

estaDisponivel:: Grade -> (Int,Int) -> Bool
estaDisponivel grade (x,y) =
  and [x > 0, x < alturaGrade, y > 0, y < larguraGrade, not . blocoParado $ getBloco grade (x,y)]

getBloco:: Grade -> (Int,Int) -> Maybe Bloco
getBloco grade (x,y) = grade !! x !! y
