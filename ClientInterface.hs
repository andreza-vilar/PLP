module ClientInterface where

import System.IO (withFile, IOMode(..), hGetContents)
import Data.List (isPrefixOf, find)
import SessionManagement (Session(..)) -- Importa o tipo Session do módulo SessionManagement

-- Função para visualizar filmes (mantida)
viewMovies :: IO ()
viewMovies = do
  withFile "movies.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStrLn "Lista de filmes disponíveis:"
    putStrLn contents

-- Função para visualizar itens da bomboniere (mantida)
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

-- Função para comprar um item da bomboniere (mantida)
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

-- Função para visualizar sessões de cinema
viewSessions :: IO ()
viewSessions = do
  withFile "sessions.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    let sessions = read contents :: [Session]
    putStrLn "Sessões disponíveis:"
    if null sessions
      then putStrLn "Nenhuma sessão disponível."
      else mapM_ printSessionDetails sessions

-- Função auxiliar para imprimir os detalhes de uma sessão
printSessionDetails :: Session -> IO ()
printSessionDetails (Session title time room date) = do
  putStrLn $ "Filme: " ++ title
  putStrLn $ "Data: " ++ date
  putStrLn $ "Horário: " ++ time
  putStrLn $ "Sala: " ++ room
  putStrLn "------------------------"

-- Outras funções do cliente (mantidas)
buyTicket :: IO ()
buyTicket = putStrLn "Comprar um ingresso."

giveFeedback :: IO ()
giveFeedback = putStrLn "Deixar um feedback sobre o filme, sala ou comida."

-- Menu do cliente (atualizado)
runClientMode :: IO ()
runClientMode = do
  putStrLn "Modo Cliente:"
  putStrLn "1) Visualizar Filmes"
  putStrLn "2) Visualizar Sessões Disponíveis"  -- Adicionada nova opção para visualizar sessões
  putStrLn "3) Comprar Ingresso"
  putStrLn "4) Comprar Item da Bomboniere"
  putStrLn "5) Deixar Feedback"
  putStrLn "6) Voltar ao Menu Principal"
  option <- getLine
  case option of
    "1" -> viewMovies >> runClientMode
    "2" -> viewSessions >> runClientMode -- Chama a nova função de visualizar sessões
    "3" -> buyTicket >> runClientMode
    "4" -> buyItem >> runClientMode
    "5" -> giveFeedback >> runClientMode
    "6" -> return () -- Retorna ao menu principal
    _   -> putStrLn "Opção inválida. Tente novamente." >> runClientMode
