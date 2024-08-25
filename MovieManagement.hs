module MovieManagement where

addMovie :: IO ()
addMovie = putStrLn "Adicionar um novo filme ao sistema."

editMovie :: IO ()
editMovie = putStrLn "Editar informações de um filme existente."

removeMovie :: IO ()
removeMovie = putStrLn "Remover um filme do sistema."

manageMovies :: IO ()
manageMovies = do
  putStrLn "1) Adicionar Filme"
  putStrLn "2) Editar Filme"
  putStrLn "3) Remover Filme"
  option <- getLine
  case option of
    "1" -> addMovie
    "2" -> editMovie
    "3" -> removeMovie
    _   -> putStrLn "Opção inválida. Tente novamente."