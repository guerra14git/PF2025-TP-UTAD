{- 
Tarefa 1 - Ler os ficheiros csv, criar uma estrutura de dados e imprimir a informação no ecrã

https://github.com/guerra14git/PF2025-TP-UTAD.git

./data contem os ficheiros .csv

Modulo: Fileread

funcoes de read que temos de implementar
    readTorneioAVE :: String -> IO TorneioAVE
    readResultadosTorneioAVE :: String -> IO ResultadosAVE
    readTorneioElim :: String -> IO TorneioElim
    readResultadosTorneioElim :: String -> IO ResultadosElim

funcoes de print "    "    "    "   
    printTorneioAVE :: TorneioAVE -> IO ()
    printResultadosTorneioAVE :: ResultadosAVE -> IO ()
    printTorneioElim :: TorneioElim -> IO ()
    printResultadosTorneioElim :: ResultadosElim -> IO ()
-}

module Fileread
    (Jogador, TorneioAVE, ResultadoAVE, ResultadosAVE, Equipa, TorneioElim, ResultadoElim, ResultadosElim, --Tipos exportados
    readTorneioAVE, readResultadosTorneioAVE, readTorneioElim, readResultadosTorneioElim, --Funcoes exportadas
    printTorneioAVE, printResultadosTorneioAVE,
    printTorneioElim, printResultadosTorneioElim
    ) where

import System.IO
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List (intercalate)

type Jogador = (String, Int, Int, Double) --tipos de dados 
type TorneioAVE = (String, Int, [Jogador])
type ResultadoAVE = (String, String, Int, Int)
type ResultadosAVE = [ResultadoAVE]
type Equipa = String
type TorneioElim = (String, [Equipa])
type ResultadoElim = (String, String, String)
type ResultadosElim = [ResultadoElim]

--remover espaco em branco
trim :: String -> String --tipo de funcao: string input e output
trim = f . f --aplica f duas vezes:
  where f = reverse . dropWhile (`elem` " \t\r\n") -- remove espaco em branco no inicio da string, inverte a e remove o espaco do inicio(do fim) novo. 

--transforma texto em n inteiro
parseInt :: String -> Int
parseInt s = fromMaybe 0 (readMaybe (trim s) :: Maybe Int) --se a string nao for nº valido ele retorna 0

parseDouble :: String -> Double
parseDouble s = fromMaybe 0.0 (readMaybe (trim s) :: Maybe Double) -- igua a parseInt mas recebe string e devolve um decimal

--funcao para divir strings por virgula 
--nao consegui importar Data.List.Split (splitOn)
splitByComma :: String -> [String] --recebe strings e retorna numa lista []
splitByComma [] = [""] --garante retornar sempre uma lista mm vazia
splitByComma (x:xs)
  | x == ','  = "" : rest --comeca uma string na lista
  | otherwise = (x : head rest) : tail rest
  where
    rest = splitByComma xs

readTorneioAVE :: String -> IO TorneioAVE -- recebe string (nome do ficheiro csv) e devolve valor tipo TorneioAVE: (String, Int, [Jogador])
readTorneioAVE fileName = do -- do significa que vai executar varias acoes io input output
  content <- readFile fileName -- le o file e guarda na var content 
  let linhas = map trim (lines content) -- divide texto em lista de linhas e usa trim para tirar espacos
      nome = head linhas -- head devolve primeiro elemento da lista linhas
      rondas = read (linhas !! 1) :: Int -- atribui a rondas o valor inteiro da 2 linha do ficheiro
      jogadores = map parseJogador (drop 2 linhas) -- drop 2 linhas remove as 2 primeiras linhas e map aplica a funcao parseJogador a cada linha restante
  return (nome, rondas, jogadores)
  where
    parseJogador :: String -> Jogador --devolve tupla
    parseJogador linha =
      case splitByComma linha of -- usa a splitbycomma para dividir a linha
        [n, v, p, a] -> (n, read v, read p, read a) -- se houver 4 elem , atribui e devolve a tupla
        [n, v, p]    -> let vg = read v -- se houver 3 elem, atribui 0.0 a avg e etc ... 
                            vp = read p
                            ave = fromIntegral vg / fromIntegral (vg + vp)
                        in (n, vg, vp, ave)
        [n]          -> (n, 0, 0, 0.0)
        _            -> ("", 0, 0, 0.0) -- caso contrario devolve valores por defeito

readResultadosTorneioAVE :: String -> IO ResultadosAVE
readResultadosTorneioAVE fileName = do --
  content <- readFile fileName 
  let ls = filter (not . null) $ map trim $ lines content -- remove linhas vazias e espacos
  return $ map parseRes (drop 1 ls) -- ignora a primeira linha (header) e aplica parseRes a cada linha
  where
    parseRes line =
      case splitByComma line of -- usa a splitbycomma para dividir a linha
        (j1:j2:s1:s2:_) -> (j1, j2, parseInt s1, parseInt s2)
        _ -> ("", "", 0, 0)

readTorneioElim :: String -> IO TorneioElim
readTorneioElim fileName = do
  content <- readFile fileName
  let ls = filter (not . null) $ map trim $ lines content -- remove linhas vazias e espacos
  case ls of
    (nome:rest) -> return (nome, concatMap splitByComma rest) -- concatMap aplica splitByComma a cada linha
    _ -> return ("", [])

readResultadosTorneioElim :: String -> IO ResultadosElim
readResultadosTorneioElim fileName = do
  content <- readFile fileName
  let ls = filter (not . null) $ map trim $ lines content
  return $ map parseLine (drop 1 ls)
  where
    parseLine line =
      case splitByComma line of
        (a:b:v:_) -> (a, b, v) -- devolve tupla
        _ -> ("", "", "")

printTorneioAVE :: TorneioAVE -> IO () -- funcao de print do tipo IO que nao devolve nada (())
printTorneioAVE (nome, n, jogadores) = do
  putStrLn $ "Torneio: " ++ nome 
  putStrLn $ "Número de rondas: " ++ show n
  putStrLn "Jogador | Ganhos | Perdidos | AVE"
  putStrLn "---------------------------------"
  mapM_ (\(j,g,p,a) -> putStrLn $ intercalate " | " [j, show g, show p, show a]) jogadores -- mapM_ aplica a funcao a cada elem da lista jogadores

printResultadosTorneioAVE :: ResultadosAVE -> IO () 
printResultadosTorneioAVE resultados = do
  putStrLn "Resultados (AVE):"
  mapM_ (\(j1,j2,s1,s2) -> putStrLn $ j1 ++ " " ++ show s1 ++ " - " ++ show s2 ++ " " ++ j2) resultados -- imprime resultados

printTorneioElim :: TorneioElim -> IO ()
printTorneioElim (nome, equipas) = do
  putStrLn $ "Torneio: " ++ nome
  putStrLn "Equipas:"
  mapM_ (\e -> putStrLn ("- " ++ e)) equipas

printResultadosTorneioElim :: ResultadosElim -> IO ()
printResultadosTorneioElim resultados = do
  putStrLn "Resultados (Elim):"
  mapM_ (\(a,b,v) -> putStrLn $ a ++ " vs " ++ b ++ " -> " ++ v) resultados

-- prmeira tarefa completa pa entrega