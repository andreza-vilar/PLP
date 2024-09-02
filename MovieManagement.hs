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
  putStrLn "Digite a classificação indicativa:"
  rating <- getLine
  putStrLn "Digite a duração do filme (em minutos):"
  duration <- getLine
  let movieEntry = "Filme em cartaz: " ++ title ++ " | " ++ director ++ " | " ++ year ++ " | " ++ rating ++ " | " ++ duration ++ " min\n"
  appendFile "movies.txt" movieEntry
  putStrLn "Filme adicionado com sucesso."

-- Função para adicionar um filme que ainda vai entrar em cartaz
addUpcomingMovie :: IO ()
addUpcomingMovie = do
  putStrLn "Digite o título do filme (lançamento futuro):"
  title <- getLine
  putStrLn "Digite o diretor do filme:"
  director <- getLine
  putStrLn "Digite o ano previsto de lançamento:"
  releaseYear <- getLine
  putStrLn "Digite a classificação indicativa:"
  rating <- getLine
  putStrLn "Digite a duração estimada do filme (em minutos):"
  duration <- getLine
  let upcomingMovieEntry = "Chega em breve: " ++ title ++ " | " ++ director ++ " | " ++ releaseYear ++ " | " ++ rating ++ " | " ++ duration ++ " min\n"
  appendFile "upcoming_movies.txt" upcomingMovieEntry
  putStrLn "Filme futuro adicionado com sucesso."

-- Função para listar todos os filmes
listMovies :: IO [String]
listMovies = do
  contents <- readFile "movies.txt"
  let movies = filter (isPrefixOf "Filme em cartaz: ") (lines contents)
  putStrLn "Lista de filmes disponíveis:"
  mapM_ putStrLn movies
  return movies

-- Função para listar lançamentos futuros
listUpcomingMovies :: IO [String]
listUpcomingMovies = do
  contents <- readFile "upcoming_movies.txt"
  let upcomingMovies = filter (isPrefixOf "Upcoming Movie: ") (lines contents)
  putStrLn "Lista de filmes que entrarão em cartaz:"
  mapM_ putStrLn upcomingMovies
  return upcomingMovies

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
      putStrLn "Digite a nova classificação indicativa:"
      newRating <- getLine
      putStrLn "Digite a nova duração do filme (em minutos):"
      newDuration <- getLine
      let newMovieEntry = "Movie: " ++ newTitle ++ " | " ++ newDirector ++ " | " ++ newYear ++ " | " ++ newRating ++ " | " ++ newDuration ++ " min"
      let updatedMovies = map (\movie -> if isInfixOf titleToEdit movie then newMovieEntry else movie) movies
      writeFile "movies.txt" (unlines(updatedMovies))
      putStrLn "Filme editado com sucesso."

-- Função para remover um filme
removeMovie :: IO ()
removeMovie = do
  movies <- listMovies
  putStrLn "Digite o título do filme a ser removido:"
  titleToRemove <- getLine
  let updatedMovies = filter (not . isInfixOf titleToRemove) movies
  writeFile "movies.txt" (unlines(updatedMovies))
  putStrLn "Filme removido com sucesso."

-- Função para gerenciar filmes
manageMovies :: IO ()
manageMovies = do
  putStrLn "Gerenciamento de Filmes:"
  putStrLn "1) Adicionar Filme"
  putStrLn "2) Adicionar Filme EM BREVE"
  putStrLn "3) Editar Filme"
  putStrLn "4) Listar Filmes"
  putStrLn "5) Listar Lançamentos Futuros"
  putStrLn "6) Remover Filme"
  putStrLn "7) Voltar"
  option <- getLine
  case option of
    "1" -> addMovie >> manageMovies
    "2" -> addUpcomingMovie >> manageMovies
    "3" -> editMovie >> manageMovies
    "4" -> listMovies >> manageMovies
    "5" -> listUpcomingMovies >> manageMovies
    "6" -> removeMovie >> manageMovies
    "7" -> putStrLn "Voltando ao menu anterior."
    _   -> putStrLn "Opção inválida. Tente novamente." >> manageMovies
