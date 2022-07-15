-- funções do arquivo
module Tetris(
    acelerar,
    adicionarBloco,
    atualizar,
    descerBloco,
    fimDeJogo,
    formaAleatoria,
    moverDireita,
    moverEsquerda,
    novoJogo,
    pontuacao,
    rotacionar,
    Bloco(..),
    Formato(..),
    Fileira,
    Grade,
) where

-- importando bibliotecas
-- responsável por disponibilizar funções uteis para manipulação de listas
import Data.List
-- responsável por encapsular valores opcionais e evitar casos de error caso venham a ocorrer
import Data.Maybe
-- gera números pseudo-aleatórios, para tomar decisões dentro do jogo, como qual bloco da vez
import System.Random

-- dados para gerar o bloco baseado no formato
data Bloco = Bloco { formato:: Formato, movimento:: Bool, origem::Bool}
            deriving (Eq, Show)

-- dados para formato do bloco
data Formato = J | L | I | S | Z | O | T
            deriving (Eq, Show, Enum)

-- fileira formada por blocos
type Fileira = [Maybe Bloco]

-- grade formada por fileiras
type Grade = [Fileira]

-- definindo a altura e largura da grade
alturaGrade:: Int
alturaGrade = 26

larguraGrade:: Int
larguraGrade = 10

-- criando o formato dos blocos
criarFormato:: Formato -> Grade
criarFormato forma
  | forma == I = preencher criarI
  | forma == J = preencher criarJ
  | forma == L = preencher criarL
  | forma == S = preencher criarS
  | forma == Z = preencher criarZ
  | forma == O = preencher criarO
  | forma == T = preencher criarT
  | otherwise = error "Formato não reconhecido"
  where
    bloco formato' origem' = Just (Bloco formato' True origem')
    x = Nothing
    preencher' linha = replicate 3 x ++ linha ++ replicate 4 x
    -- "f" é uma abreviação para formato, que será usado para operações nas funções
    preencher f
      | length f == 2 = fileiraVazia : map preencher' f ++ [fileiraVazia]
      | length f == 3 = fileiraVazia : map preencher' f
      | otherwise = map preencher' f

    -- "b" se refere a uma parte, e "o" se refere ao centro do bloco, que será usado para rotacionar o bloco completo
    criarI = [
      [x,b,x],
      [x,o,x],
      [x,b,x],
      [x,b,x]
      ]
      where
        b = bloco I False
        o = bloco I True

    criarJ = [
      [x,b,x],
      [x,o,x],
      [b,b,x]
      ]
      where
        b = bloco J False
        o = bloco J True

    criarL = [
      [x,b,x],
      [x,o,x],
      [x,b,b]
      ]
      where
        b = bloco L False
        o = bloco L True

    criarS = [
      [x,b,b],
      [b,o,x]
      ]
      where
        b = bloco S False
        o = bloco S True

    criarZ = [
      [b,b,x],
      [x,o,b]
      ]
      where
        b = bloco Z False
        o = bloco Z True

    criarO = [
      [x,b,b],
      [x,b,b]
      ]
      where
        b = bloco O False

    criarT = [
      [b,o,b],
      [x,b,x]
      ]
      where
        b = bloco T False
        o = bloco T True

-- acelera a "gravidade"
acelerar:: Grade -> Grade
acelerar = gravidade

-- adiciona um bloco no topo da grade
adicionarBloco:: Grade -> Formato -> Grade
adicionarBloco fileiras formato'
  | vazio fileiras && not (fimDeJogo fileiras) = criarFormato formato' ++ drop 4  fileiras
  | otherwise = fileiras

-- atualiza o estado atual da grade pela "gravidade" limpando as linhas e parando os blocos
atualizar:: Grade -> Formato -> Grade
atualizar = adicionarBloco . gravidade . limparLinhas . congelarBlocos

-- determina quando um determinado bloco está estacionado
blocoEstacionario:: Maybe Bloco -> Bool
blocoEstacionario = maybe False (not . movimento)

-- altera os blocos em movimento que pararam de se mover para estacionários
congelarBlocos:: Grade -> Grade
congelarBlocos fileiras
  | parar fileiras = map congelarBlocos' fileiras
  | otherwise = fileiras
  where
    congelarBlocos':: Fileira -> Fileira
    congelarBlocos' [] = []
    congelarBlocos' (Just (Bloco f True o):cauda) = Just (Bloco f False o): congelarBlocos' cauda
    congelarBlocos' b = head b:congelarBlocos' (tail b)

-- retorna as coordenadas
coordenadas:: Grade -> [(Int, Int)]
coordenadas [] = []
coordenadas (cabeca: cauda) = coordenadas' cabeca (25 - length cauda) ++ coordenadas cauda

coordenadas':: Fileira -> Int -> [(Int, Int)]
coordenadas' [] _ = []
coordenadas' (cabeca:cauda) y
  | movimentoBloco cabeca = (y, 9 - length cauda): coordenadas' cauda y
  | otherwise = coordenadas' cauda y

-- faz o bloco descer a grade
descerBloco:: Grade -> Grade
descerBloco fileiras
  | crescer /= fileiras = descerBloco crescer
  | otherwise = fileiras
  where
    crescer = gravidade fileiras

-- retorna se é ou não origem
ehOrigem:: Grade -> (Int, Int) -> Bool
ehOrigem grade (x, y) = maybe False origem $ getBloco grade (x, y)

-- retorna se aquelas coordenadas estão disponíveis
estaDisponivel:: Grade -> (Int,Int) -> Bool
estaDisponivel grade (x,y) =
  and [x > 0, x < alturaGrade, y > 0, y < larguraGrade, not . blocoEstacionario $ getBloco grade (x,y)]

-- retorna as fileiras ausentes da grade
fileirasAusentes:: Grade -> Int
fileirasAusentes fileiras = length fileiras - length (removerLinhas fileiras)

-- retorna uma fileira vazia
fileiraVazia:: Fileira
fileiraVazia = replicate 10 Nothing

-- indica quando o estado atual resulta em um fim de jogo (game over)
fimDeJogo:: Grade -> Bool
fimDeJogo = any (not . all movimento . catMaybes) . take 4

-- retorna uma tupla contendo um formato aleatório e um gerador
formaAleatoria:: RandomGen gerador => gerador -> (Formato, gerador)
formaAleatoria gerador = case randomR(0, length [J ..]-1) gerador of (aleatorio,gerador') -> (toEnum aleatorio, gerador')

-- retorna o bloco
getBloco:: Grade -> (Int,Int) -> Maybe Bloco
getBloco grade (x,y) = grade !! x !! y

-- retorna a origem
getOrigem:: Grade -> (Int, Int)
getOrigem = head . origens

-- recebe a grade, as coordenadas e o bloco, e retorna a grade atualizada
girar:: Grade -> [(Int, Int)] -> [Maybe Bloco] -> Grade
girar grade [] _ = grade
girar grade (cabeca:cauda) (cabeca_v:cauda_v) = girar (setBloco grade cabeca cabeca_v) cauda cauda_v
girar _ (_:_) [] = error "Isso não pode acontecer"

-- "gravidade" faz o bloco descer a grade
gravidade:: Grade -> Grade
gravidade fileiras
  | parar fileiras = fileiras
  | otherwise = transpose . gravidade_fileiras . transpose $ fileiras
  where
    gravidade_fileira:: Fileira -> Fileira
    gravidade_fileira [] = []
    gravidade_fileira fileira@(cabeca:cauda)
        | movimentoBloco cabeca = moverBlocos fileira
        | otherwise = cabeca : gravidade_fileira cauda

    gravidade_fileiras:: Grade -> Grade
    gravidade_fileiras [] = []
    gravidade_fileiras (cabeca:cauda) = gravidade_fileira cabeca : gravidade_fileiras cauda

-- limpar Grade
limparGrade:: Grade -> Grade
limparGrade grade = limparGrade' grade $coordenadas grade

-- função auxiliar para limparGrade
limparGrade':: Grade -> [(Int, Int)] -> Grade
limparGrade' = foldl (\grade cabeca -> setBloco grade cabeca Nothing)

-- limpa as linhas da grade
limparLinhas:: Grade -> Grade
limparLinhas fileiras
  | vazio fileiras = replicate (fileirasAusentes fileiras) fileiraVazia ++ removerLinhas fileiras
  | otherwise = fileiras

-- determina quando uma linha está completa
linhaCompleta:: Fileira -> Bool
linhaCompleta linha = filter (/= Nothing) linha == linha

-- move o bloco para baixo
moverBlocos:: Fileira -> Fileira
moverBlocos l
  | eh_lacuna (lacuna l) = (Nothing:movimentoBlocos l) ++ tail (lacuna l) ++ chao l
  | otherwise = error "Nunca deve acontecer?"
  where
    eh_lacuna:: Fileira -> Bool
    eh_lacuna fileira = not (null $ lacuna fileira) && isNothing (head $ lacuna fileira)

    movimentoBlocos:: Fileira -> Fileira
    movimentoBlocos (cabeca:cauda) | movimentoBloco cabeca = cabeca:movimentoBlocos cauda
    movimentoBlocos _ = []

    lacuna:: Fileira -> Fileira
    lacuna (Nothing:cauda) = Nothing: lacuna' cauda
    lacuna (cabeca:cauda) | movimentoBloco cabeca = lacuna cauda
    lacuna _ = []

    lacuna':: Fileira -> Fileira
    lacuna' (Nothing:cauda) = Nothing:lacuna' cauda
    lacuna' _ = []

    chao:: Fileira -> Fileira
    chao [] = []
    chao (cabeca:cauda)
      | blocoEstacionario cabeca = cabeca:cauda
      | otherwise = chao cauda

-- move o bloco para direita
moverDireita:: Grade -> Grade
moverDireita fileiras
  | toqueDireita fileiras = fileiras
  | otherwise = transpose . gravidade . transpose $ fileiras

-- move o bloco para direita
moverEsquerda:: Grade -> Grade
moverEsquerda fileiras
  | toqueEsquerda fileiras = fileiras
  | otherwise = map reverse . transpose . gravidade . transpose . map reverse $ fileiras

-- determina quando um determinado bloco está em movimento
movimentoBloco:: Maybe Bloco -> Bool
movimentoBloco = maybe False movimento

-- retorna uma grade vazia
novoJogo:: Grade
novoJogo = replicate alturaGrade (replicate larguraGrade Nothing)

-- retorna as coordenadas de origem
origens:: Grade -> [(Int, Int)]
origens grade = filter (ehOrigem grade) (coordenadas grade)

-- determina quando um bloco deve parar de se mover
parar:: Grade -> Bool
parar fileiras = any parar' (transpose fileiras) || vazio fileiras
  where
    parar':: Fileira -> Bool
    parar' [] = False
    parar' fileira | all movimentoBloco fileira = True
    parar' (primeiro:segundo:_) | movimentoBloco primeiro && blocoEstacionario segundo = True
    parar' (_:cauda) = parar' cauda

-- retorna a pontuação do estado atual
pontuacao:: Grade -> Int
pontuacao = product . replicate 2 . length . filter id. map linhaCompleta

-- remove uma linha de blocos
removerLinhas:: Grade -> Grade
removerLinhas = filter (not . linhaCompleta)

-- rotaciona o bloco no sentido horário
rotacionar:: Grade -> Grade
rotacionar g = girar (limparGrade g) (rotacionarBloco g) (map (getBloco g) (coordenadas g))

-- rotaciona o bloco no sentido horario
rotacionarBloco:: Grade -> [(Int,Int)]
rotacionarBloco grade
  | temOrigem grade && all (estaDisponivel grade) rotacionado = rotacionado
  | otherwise = coordenadasMovimento
  where
    coordenadasMovimento = coordenadas grade
    rotacionado = map (rotacionarPonto $ getOrigem grade) coordenadasMovimento

-- rotaciona um ponto de acordo com as coordenadas recebidas
rotacionarPonto:: (Int,Int) -> (Int,Int) -> (Int,Int)
rotacionarPonto (origemX,origemY) (destinoX,destinoY) = (origemX + origemY - destinoY, origemY - origemX + destinoX)

-- coloca um bloco nas coordenadas recebidas
setBloco:: Grade -> (Int, Int) -> Maybe Bloco -> Grade
setBloco grade (x, y) val = g1 ++ setBloco' (head g2) y val : tail g2
  where
    (g1, g2) = splitAt x grade

setBloco':: Fileira -> Int -> Maybe Bloco -> Fileira
setBloco' fileira y val = f1 ++ val : tail f2
  where
    (f1, f2) = splitAt y fileira

-- retorna se há alguma origem na grade
temOrigem:: Grade -> Bool
temOrigem = not . null . origens

-- checa se o bloco toca a parede da direita
toqueDireita:: Grade -> Bool
toqueDireita = any movimento . mapMaybe last

-- checa se o bloco toca a parede da esquerda
toqueEsquerda:: Grade -> Bool
toqueEsquerda = any movimento . mapMaybe head

-- determina quando não tem blocos em movimento
vazio:: Grade -> Bool
vazio fileiras = all vazio' (transpose fileiras)
  where
    vazio':: Fileira -> Bool
    vazio' l = not (any movimento (catMaybes l))
