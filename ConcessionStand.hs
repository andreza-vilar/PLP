module ConcessionStand where

addItem :: IO ()
addItem = putStrLn "Adicionar um novo item na bomboniere."

removeItem :: IO ()
removeItem = putStrLn "Remover um item da bomboniere."

manageConcessionStand :: IO ()
manageConcessionStand = do
  putStrLn "1) Adicionar Item"
  putStrLn "2) Remover Item"
  option <- getLine
  case option of
    "1" -> addItem
    "2" -> removeItem
    _   -> putStrLn "Opção inválida. Tente novamente."