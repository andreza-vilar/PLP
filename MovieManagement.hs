module MovieManagement where

import System.IO (appendFile, readFile, writeFile)
import Data.List (isInfixOf, isPrefixOf, intercalate, delete)

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
  contents <- readFile "movies.txt"
  let updatedContents = unlines (filter (\line -> not (isTitle titleToRemove line)) (lines contents))
  writeFile "movies.txt" updatedContents
  putStrLn "Filme removido com sucesso."
  removeMovieFromViews titleToRemove

-- Função para remover o filme das visualizações de usuários e funcionários
removeMovieFromViews :: String -> IO ()
removeMovieFromViews titleToRemove = do
  contents <- readFile "movies.txt"
  let updatedContents = unlines $ map (removeTitle titleToRemove) (lines contents)
  writeFile "movies.txt" updatedContents

-- Função auxiliar para remover o título da lista de visualização de um usuário ou funcionário
removeTitle :: String -> String -> String
removeTitle titleToRemove line
  | "User:" `isPrefixOf` line || "Employee:" `isPrefixOf` line =
      let (prefix, titles) = break (== ':') line
          updatedTitles = filter (/= titleToRemove) (splitTitles $ drop 1 titles)
      in prefix ++ ": " ++ intercalate ", " updatedTitles
  | otherwise = line
  where
    splitTitles = words . map (\c -> if c == ',' then ' ' else c) -- Converte a lista de títulos em palavras

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
  option <- getLine
  case option of
    "1" -> addMovie
    "2" -> editMovie
    "3" -> removeMovie
    "4" -> listMovies >> return ()
    _   -> putStrLn "Opção inválida. Tente novamente."
