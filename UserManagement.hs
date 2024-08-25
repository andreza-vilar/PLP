module UserManagement where

addUser :: IO ()
addUser = putStrLn "Adicionar um novo usuário."

editUser :: IO ()
editUser = putStrLn "Editar perfil de um usuário."

removeUser :: IO ()
removeUser = putStrLn "Remover um usuário do sistema."

manageUsers :: IO ()
manageUsers = do
  putStrLn "1) Adicionar Usuário"
  putStrLn "2) Editar Usuário"
  putStrLn "3) Remover Usuário"
  option <- getLine
  case option of
    "1" -> addUser
    "2" -> editUser
    "3" -> removeUser
    _   -> putStrLn "Opção inválida. Tente novamente."