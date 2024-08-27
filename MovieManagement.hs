module MovieManagement where

import System.IO (appendFile, readFile, writeFile)
import Data.List (isPrefixOf, isInfixOf, intercalate, delete)

-- Função para adicionar um filme
addMovie :: IO ()
addMovie = do
  putStrLn "Digite o título do filme:"
  title <- getLine
  putStrLn "Digite o diretor do filme:"
  director <- getLine
  putStrLn "Digite o ano de lançamento:"
  year <- getLine
  let movieEntry = "Movie: " ++ title ++ " | " ++ director ++ " | " ++ year ++ "\n"
  appendFile "movies.txt" movieEntry
  putStrLn "Filme adicionado com sucesso."

-- Função para listar todos os filmes
listMovies :: IO [String]
listMovies = do
  contents <- readFile "movies.txt"
  let movies = filter (isPrefixOf "Movie: ") (lines contents)
  putStrLn "Lista de filmes disponíveis:"
  mapM_ putStrLn movies
  return movies

-- Função para editar um filme existente
editMovie :: IO ()
editMovie = do
  movies <- listMovies
  putStrLn "Digite o título do filme a ser editado:"
  titleToEdit <- getLine
  let movieToEdit = filter (isInfixOf titleToEdit) movies
  if null movieToEdit
    then putStrLn "Filme não encontrado."
    else do
      putStrLn "Digite o novo título do filme:"
      newTitle <- getLine
      putStrLn "Digite o novo diretor do filme:"
      newDirector <- getLine
      putStrLn "Digite o novo ano de lançamento:"
      newYear <- getLine
      let newMovieEntry = "Movie: " ++ newTitle ++ " | " ++ newDirector ++ " | " ++ newYear
      let updatedMovies = map (\movie -> if isInfixOf titleToEdit movie then newMovieEntry else movie) movies
      writeFile "movies.txt" (unlines updatedMovies)
      putStrLn "Filme editado com sucesso."

-- Função para remover um filme
removeMovie :: IO ()
removeMovie = do
  movies <- listMovies
  putStrLn "Digite o título do filme a ser removido:"
  titleToRemove <- getLine
  let updatedMovies = filter (not . isInfixOf titleToRemove) movies
  writeFile "movies.txt" (unlines updatedMovies)
  putStrLn "Filme removido com sucesso."

-- Função para gerenciar filmes
manageMovies :: IO ()
manageMovies = do
  putStrLn "Gerenciamento de Filmes:"
  putStrLn "1) Adicionar Filme"
  putStrLn "2) Editar Filme"
  putStrLn "3) Listar Filmes"
  putStrLn "4) Remover Filme"
  putStrLn "5) Voltar"
  option <- getLine
  case option of
    "1" -> addMovie >> manageMovies
    "2" -> editMovie >> manageMovies
    "3" -> listMovies >> manageMovies
    "4" -> removeMovie >> manageMovies
    "5" -> putStrLn "Voltando ao menu anterior."
    _   -> putStrLn "Opção inválida. Tente novamente." >> manageMovies