module UserManagement where
import System.IO

-- Adiciona um novo usuário ao arquivo "usersCinema.txt"
addUser :: IO ()
addUser = do
  putStrLn "Digite o email do novo usuário:"
  userEmail <- getLine
  putStrLn "Digite a quantidade de pontos do novo usuário:"
  userPoints <- getLine
  appendFile "usersCinema.txt" (userEmail ++ " " ++ userPoints ++ "\n")
  putStrLn "Usuário adicionado com sucesso."

-- Edita um dos usuários do arquivo "usersCinema.txt"
editUser :: IO()
editUser = do
  putStrLn "teste"

-- Remove o usuário do arquivo "usersCinema.txt"
removeUser :: IO()
removeUser = do
  putStrLn "teste"

-- Função principal de gerenciamento de usuários
manageUsers :: IO ()
manageUsers = do
  putStrLn "1) Adicionar Usuário"
  putStrLn "2) Editar Usuário"
  putStrLn "3) Remover Usuário"
  putStrLn "4) Voltar"
  option <- getLine
  case option of
    "1" -> addUser >> manageUsers
    "2" -> editUser >> manageUsers
    "3" -> removeUser >> manageUsers
    "4" -> putStrLn "Voltando ao menu anterior."
    _   -> putStrLn "Opção inválida. Tente novamente." >> manageUsers
