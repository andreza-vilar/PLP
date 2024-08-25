module MovieManagement where

import System.IO (appendFile, readFile)

-- Função para adicionar um filme
addMovie :: IO ()
addMovie = do
  putStrLn "Digite o título do filme:"
  title <- getLine
  putStrLn "Digite o diretor do filme:"
  director <- getLine
  putStrLn "Digite o ano de lançamento:"
  year <- getLine
  let movieEntry = title ++ " | " ++ director ++ " | " ++ year ++ "\n"
  appendFile "movies.txt" movieEntry
  putStrLn "Filme adicionado com sucesso."
  listMovies  -- Adiciona esta linha para listar os filmes após a adição

-- Função para listar todos os filmes
listMovies :: IO ()
listMovies = do
  contents <- readFile "movies.txt"
  putStrLn "Lista de filmes:"
  putStrLn contents

-- Funções para editar e remover filme (mantidas como estão)
editMovie :: IO ()
editMovie = putStrLn "Editar informações de um filme existente."

removeMovie :: IO ()
removeMovie = putStrLn "Remover um filme do sistema."

-- Menu de gerenciamento de filmes atualizado
manageMovies :: IO ()
manageMovies = do
  putStrLn "1) Adicionar Filme"
  putStrLn "2) Editar Filme"
  putStrLn "3) Remover Filme"
  putStrLn "4) Listar Filmes"  -- Adiciona a opção para listar filmes
  option <- getLine
  case option of
    "1" -> addMovie
    "2" -> editMovie
    "3" -> removeMovie
    "4" -> listMovies
    _   -> putStrLn "Opção inválida. Tente novamente."
