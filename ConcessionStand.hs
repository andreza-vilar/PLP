module ConcessionStand where

import System.IO (withFile, IOMode(..), hGetContents, hPutStrLn, hPutStr, hClose)
import Data.List (isPrefixOf)

-- Arquivo onde os itens da bomboniere são armazenados
fileName :: FilePath
fileName = "concessionStand.txt"

-- Código de acesso para o funcionário (você pode mudar para algo mais seguro)
employeePassword :: String
employeePassword = "1234"

-- Função para adicionar um item à bomboniere
addItem :: IO ()
addItem = do
  putStrLn "Digite o nome do item:"
  itemName <- getLine
  putStrLn "Digite o preço do item (em reais):"
  price <- getLine
  let itemEntry = itemName ++ " | R$ " ++ price ++ "\n"
  withFile fileName AppendMode (\handle -> hPutStrLn handle itemEntry)
  putStrLn "Item adicionado com sucesso."

-- Função para listar todos os itens da bomboniere
listItems :: IO ()
listItems = do
  withFile fileName ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStrLn "Itens da bomboniere:"
    putStrLn contents

-- Função para editar um item na bomboniere
editItem :: IO ()
editItem = do
  listItems
  putStrLn "Digite o nome do item a ser editado:"
  oldName <- getLine
  putStrLn "Digite o novo nome do item (ou deixe em branco para manter o mesmo):"
  newName <- getLine
  putStrLn "Digite o novo preço do item (ou deixe em branco para manter o mesmo):"
  newPrice <- getLine
  contents <- readFile fileName
  let updatedItems = map (updateItem oldName newName newPrice) (lines contents)
  writeFile fileName (unlines updatedItems)
  putStrLn "Item atualizado com sucesso."

-- Função auxiliar para atualizar um item
updateItem :: String -> String -> String -> String -> String
updateItem oldName newName newPrice line
  | oldName `isPrefixOf` line =
      let (name, price) = break (== '|') line
          updatedName = if null newName then name else newName
          updatedPrice = if null newPrice then drop 1 price else newPrice
      in updatedName ++ " | R$ " ++ updatedPrice
  | otherwise = line

-- Função para remover um item da bomboniere (somente funcionário)
removeItem :: IO ()
removeItem = do
  putStrLn "Digite o código de acesso do funcionário:"
  enteredPassword <- getLine
  if enteredPassword == employeePassword
    then do
      contents <- readFile fileName
      putStrLn "Itens da bomboniere:"
      putStrLn contents
      putStrLn "Digite o nome do item a ser removido:"
      itemToRemove <- getLine
      let updatedItems = filter (not . isPrefixOf itemToRemove) (lines contents)
      -- Força a avaliação do conteúdo antes de abrir o arquivo novamente
      length updatedItems `seq` writeFile fileName (unlines updatedItems)
      putStrLn "Item removido com sucesso."
    else putStrLn "Código de acesso incorreto. Acesso negado."


-- Menu de gerenciamento da bomboniere
manageConcessionStand :: IO ()
manageConcessionStand = do
  putStrLn "1) Adicionar Item"
  putStrLn "2) Listar Itens"
  putStrLn "3) Editar Item"
  putStrLn "4) Remover Item"
  putStrLn "5) Voltar ao Menu Principal"
  option <- getLine
  case option of
    "1" -> addItem >> manageConcessionStand
    "2" -> listItems >> manageConcessionStand
    "3" -> editItem >> manageConcessionStand
    "4" -> removeItem >> manageConcessionStand
    "5" -> return () -- Volta ao menu principal
    _   -> putStrLn "Opção inválida. Tente novamente." >> manageConcessionStand
