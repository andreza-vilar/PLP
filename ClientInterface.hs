module ClientInterface where

import System.IO (readFile)

-- Função para visualizar filmes
viewMovies :: IO ()
viewMovies = do
  contents <- readFile "movies.txt"
  putStrLn "Lista de filmes disponíveis:"
  putStrLn contents

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
  putStrLn "3) Deixar Feedback"
  putStrLn "4) Voltar ao Menu Principal"
  option <- getLine
  case option of
    "1" -> viewMovies >> runClientMode
    "2" -> buyTicket >> runClientMode
    "3" -> giveFeedback >> runClientMode
    "4" -> return () -- Retorna ao menu principal no módulo Main
    _   -> putStrLn "Opção inválida. Tente novamente." >> runClientMode
