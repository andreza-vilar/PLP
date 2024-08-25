module SessionManagement where

createSession :: IO ()
createSession = putStrLn "Criar uma nova sessão de cinema."

editSession :: IO ()
editSession = putStrLn "Editar uma sessão existente."

removeSession :: IO ()
removeSession = putStrLn "Remover uma sessão."

manageSessions :: IO ()
manageSessions = do
  putStrLn "1) Criar Sessão"
  putStrLn "2) Editar Sessão"
  putStrLn "3) Remover Sessão"
  option <- getLine
  case option of
    "1" -> createSession
    "2" -> editSession
    "3" -> removeSession
    _   -> putStrLn "Opção inválida. Tente novamente."