module ClientInterface where

import System.IO (readFile)
import Data.List (isPrefixOf)

-- Função para visualizar filmes
viewMovies :: IO ()
viewMovies = do
  contents <- readFile "movies.txt"
  putStrLn "Lista de filmes disponíveis:"
  putStrLn contents

-- Função para visualizar itens da bomboniere
viewItems :: IO ()
viewItems = do
  contents <- readFile "concessionStand.txt"
  putStrLn "Itens da bomboniere disponíveis:"
  putStrLn contents

-- Função para comprar um item da bomboniere
buyItem :: IO ()
buyItem = do
  viewItems
  putStrLn "Digite o nome do item que deseja comprar:"
  itemName <- getLine
  -- Simulação de confirmação de compra
  putStrLn ("Você comprou o item: " ++ itemName)
  -- Aqui você pode adicionar lógica para realmente processar a compra, se necessário

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
