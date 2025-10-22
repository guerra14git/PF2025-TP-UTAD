module Fileread
  ( Jogador, TorneioAVE, ResultadoAVE, ResultadosAVE
  , Equipa, TorneioElim, ResultadoElim, ResultadosElim
  , readTorneioAVE, readResultadosTorneioAVE
  , readTorneioElim, readResultadosTorneioElim
  , printTorneioAVE, printResultadosTorneioAVE
  , printTorneioElim, printResultadosTorneioElim
  ) where

import System.IO
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List (intercalate)

-- ============================
-- Tipos de dados
-- ============================
type Jogador = (String, Int, Int, Double)
type TorneioAVE = (String, Int, [Jogador])
type ResultadoAVE = (String, String, Int, Int)
type ResultadosAVE = [ResultadoAVE]

type Equipa = String
type TorneioElim = (String, [Equipa])
type ResultadoElim = (String, String, String)
type ResultadosElim = [ResultadoElim]

-- ============================
-- Funções auxiliares
-- ============================
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\r\n")

parseInt :: String -> Int
parseInt s = fromMaybe 0 (readMaybe (trim s) :: Maybe Int)

parseDouble :: String -> Double
parseDouble s = fromMaybe 0.0 (readMaybe (trim s) :: Maybe Double)

-- Função caseira para dividir strings por vírgula
splitByComma :: String -> [String]
splitByComma [] = [""]
splitByComma (c:cs)
  | c == ','  = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitByComma cs

-- ============================
-- Leitura: Torneio AVE
-- ============================
readTorneioAVE :: String -> IO TorneioAVE
readTorneioAVE fileName = do
  content <- readFile fileName
  let ls = filter (not . null) $ map trim $ lines content
  case ls of
    (nome : rondasStr : rest) ->
      let nroR = parseInt rondasStr
          jogadores = map parseJogador rest
      in return (nome, nroR, jogadores)
    _ -> return ("", 0, [])
  where
    parseJogador line =
      case splitByComma line of
        (n:v:p:a:_) -> (n, parseInt v, parseInt p, parseDouble a)
        (n:v:p:_)   ->
          let vg = parseInt v
              vp = parseInt p
              ave = if (vg + vp) > 0 then fromIntegral vg / fromIntegral (vg + vp) else 0.0
          in (n, vg, vp, ave)
        (n:_)       -> (n, 0, 0, 0.0)
        _           -> ("", 0, 0, 0.0)

-- Leitura dos resultados AVE
readResultadosTorneioAVE :: String -> IO ResultadosAVE
readResultadosTorneioAVE fileName = do
  content <- readFile fileName
  let ls = filter (not . null) $ map trim $ lines content
  return $ map parseRes (drop 1 ls)
  where
    parseRes line =
      case splitByComma line of
        (j1:j2:s1:s2:_) -> (j1, j2, parseInt s1, parseInt s2)
        _ -> ("", "", 0, 0)

-- ============================
-- Leitura: Eliminatórias
-- ============================
readTorneioElim :: String -> IO TorneioElim
readTorneioElim fileName = do
  content <- readFile fileName
  let ls = filter (not . null) $ map trim $ lines content
  case ls of
    (nome:rest) -> return (nome, concatMap splitByComma rest)
    _ -> return ("", [])

readResultadosTorneioElim :: String -> IO ResultadosElim
readResultadosTorneioElim fileName = do
  content <- readFile fileName
  let ls = filter (not . null) $ map trim $ lines content
  return $ map parseLine (drop 1 ls)
  where
    parseLine line =
      case splitByComma line of
        (a:b:v:_) -> (a, b, v)
        _ -> ("", "", "")

-- ============================
-- Impressão
-- ============================
printTorneioAVE :: TorneioAVE -> IO ()
printTorneioAVE (nome, n, jogadores) = do
  putStrLn $ "Torneio: " ++ nome
  putStrLn $ "Número de rondas: " ++ show n
  putStrLn "Jogador | Ganhos | Perdidos | AVE"
  putStrLn "---------------------------------"
  mapM_ (\(j,g,p,a) -> putStrLn $ intercalate " | " [j, show g, show p, show a]) jogadores

printResultadosTorneioAVE :: ResultadosAVE -> IO ()
printResultadosTorneioAVE resultados = do
  putStrLn "Resultados (AVE):"
  mapM_ (\(j1,j2,s1,s2) -> putStrLn $ j1 ++ " " ++ show s1 ++ " - " ++ show s2 ++ " " ++ j2) resultados

printTorneioElim :: TorneioElim -> IO ()
printTorneioElim (nome, equipas) = do
  putStrLn $ "Torneio: " ++ nome
  putStrLn "Equipas:"
  mapM_ (\e -> putStrLn ("- " ++ e)) equipas

printResultadosTorneioElim :: ResultadosElim -> IO ()
printResultadosTorneioElim resultados = do
  putStrLn "Resultados (Elim):"
  mapM_ (\(a,b,v) -> putStrLn $ a ++ " vs " ++ b ++ " -> " ++ v) resultados
