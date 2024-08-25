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

-- Função para remover um filme
removeMovie :: IO ()
removeMovie = do
  movies <- listMovies
  putStrLn "Digite o título do filme a ser removido:"
  titleToRemove <- getLine
  let updatedMovies = filter (not . isTitle titleToRemove) movies
  writeFile "movies.txt" (unlines updatedMovies)
  putStrLn "Filme removido com sucesso."

-- Função auxiliar para verificar se a linha contém o título do filme
isTitle :: String -> String -> Bool
isTitle title line = title `isInfixOf` line && "Movie: " `isPrefixOf` line

-- Funções para editar filme (mantidas como estão)
editMovie :: IO ()
editMovie = putStrLn "Editar informações de um filme existente."

-- Menu de gerenciamento de filmes
manageMovies :: IO ()
manageMovies = do
  putStrLn "1) Adicionar Filme"
  putStrLn "2) Editar Filme"
  putStrLn "3) Remover Filme"
  putStrLn "4) Listar Filmes"
  putStrLn "5) Voltar ao Menu Principal"
  option <- getLine
  case option of
    "1" -> addMovie
    "2" -> editMovie
    "3" -> removeMovie
    "4" -> listMovies >> return ()
    "5" -> return () -- Volta ao menu principal
    _   -> putStrLn "Opção inválida. Tente novamente." >> manageMovies