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
    (Jogador, TorneioAVE, ResutadoAVE, ResultadosAVE, Equipa, TorneioElim, ResultadoElim, ResultadosElim, --Tipos exportados   
    readTorneioAVE, readResultadosTorneioAVE, --Funcoes exportadas
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