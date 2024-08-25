module ClientInterface where

import System.IO (withFile, IOMode(..), hGetContents)
import Data.List (isPrefixOf, find)

-- Função para visualizar filmes
viewMovies :: IO ()
viewMovies = do
  withFile "movies.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStrLn "Lista de filmes disponíveis:"
    putStrLn contents

-- Função para visualizar itens da bomboniere
viewItems :: IO [String]
viewItems = do
  withFile "concessionStand.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    let items = lines contents
    putStrLn "Itens da bomboniere disponíveis:"
    if null items
      then putStrLn "Nenhum item disponível." >> return []
      else do
        mapM_ putStrLn items
        return items

-- Função para comprar um item da bomboniere
buyItem :: IO ()
buyItem = do
  items <- viewItems
  if null items
    then putStrLn "Voltando ao menu principal..." -- Caso não haja itens, volta ao menu principal
    else do
      putStrLn "Digite o nome do item que deseja comprar:"
      itemName <- getLine
      -- Verifica se o item existe na lista de itens disponíveis
      case find (isPrefixOf itemName) items of
        Just _  -> putStrLn ("Você comprou o item: " ++ itemName)
        Nothing -> putStrLn "Item não encontrado. Voltando ao menu principal."

-- Outras funções do cliente
buyTicket :: IO ()
buyTicket = putStrLn "Comprar um ingresso."

giveFeedback :: IO ()
giveFeedback = putStrLn "Deixar um feedback sobre o filme, sala ou comida."

-- Menu do cliente
runClientMode :: IO ()
runClientMode = do
  putStrLn "Modo Cliente:"
  putStrLn "1) Visualizar Filmes"
  putStrLn "2) Comprar Ingresso"
  putStrLn "3) Comprar Item da Bomboniere"
  putStrLn "4) Deixar Feedback"
  putStrLn "5) Voltar ao Menu Principal"
  option <- getLine
  case option of
    "1" -> viewMovies >> runClientMode
    "2" -> buyTicket >> runClientMode
    "3" -> buyItem >> runClientMode
    "4" -> giveFeedback >> runClientMode
    "5" -> return () -- Retorna ao menu principal
    _   -> putStrLn "Opção inválida. Tente novamente." >> runClientMode