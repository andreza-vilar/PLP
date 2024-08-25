module SessionManagement where

import System.IO (appendFile, readFile, writeFile)
import Data.List (isInfixOf)

data UserType = Employee | Customer deriving (Eq, Show)

-- Função para verificar se o usuário é funcionário
isEmployee :: UserType -> Bool
isEmployee Employee = True
isEmployee _        = False

-- Função para criar uma nova sessão de cinema
createSession :: IO ()
createSession = do
    putStrLn "Digite o nome do filme:"
    movie <- getLine
    putStrLn "Digite a data da sessão (dd/mm/yyyy):"
    date <- getLine
    putStrLn "Digite o horário da sessão (hh:mm):"
    time <- getLine
    let sessionEntry = "Session: " ++ movie ++ " | " ++ date ++ " | " ++ time ++ "\n"
    appendFile "sessions.txt" sessionEntry
    putStrLn "Sessão criada com sucesso."

-- Função para editar uma sessão de cinema
editSession :: UserType -> IO ()
editSession user = 
    if isEmployee user
    then do
        sessions <- listSessions
        putStrLn "Digite o nome do filme da sessão a ser editada:"
        movieToEdit <- getLine
        let sessionToEdit = filter (isInfixOf movieToEdit) sessions
        if null sessionToEdit
            then putStrLn "Sessão não encontrada.\n"
            else do
                putStrLn "Digite o novo nome do filme (ou deixe em branco para manter o mesmo):"
                newMovie <- getLine
                putStrLn "Digite a nova data da sessão (ou deixe em branco para manter a mesma):"
                newDate <- getLine
                putStrLn "Digite o novo horário da sessão (ou deixe em branco para manter o mesmo):"
                newTime <- getLine
                let newSessionEntry = "Session: " ++ (if null newMovie then movieToEdit else newMovie) 
                                    ++ " | " ++ (if null newDate then head (words (drop 1 (dropWhile (/= '|') (head sessionToEdit)))) else newDate)
                                    ++ " | " ++ (if null newTime then last (words (drop 1 (dropWhile (/= '|') (head sessionToEdit)))) else newTime)
                let updatedSessions = map (\session -> if isInfixOf movieToEdit session then newSessionEntry else session) sessions
                writeFile "sessions.txt" (unlines updatedSessions)
                putStrLn "Sessão editada com sucesso."
    else putStrLn "Apenas funcionários podem editar sessões."

-- Função para remover uma sessão de cinema
removeSession :: IO ()
removeSession = do
    sessions <- listSessions
    putStrLn "Digite o nome do filme da sessão a ser removida:"
    movieToRemove <- getLine
    let updatedSessions = filter (not . isInfixOf movieToRemove) sessions
    writeFile "sessions.txt" (unlines updatedSessions)
    putStrLn "Sessão removida com sucesso."

-- Função para listar todas as sessões
listSessions :: IO [String]
listSessions = do
    contents <- readFile "sessions.txt"
    let sessions = lines contents
    if null sessions
        then putStrLn "Nenhuma sessão disponível." >> return []
        else do
            putStrLn "Sessões disponíveis:\n"
            mapM_ putStrLn sessions
            putStrLn "\n"
            return sessions

-- Função principal de gerenciamento de sessões
manageSessions :: UserType -> IO ()
manageSessions user = do
    putStrLn "Gerenciamento de Sessões:"
    putStrLn "1) Criar Sessão"
    putStrLn "2) Editar Sessão"
    putStrLn "3) Remover Sessão"
    putStrLn "4) Listar Sessões"
    putStrLn "5) Voltar\n"
    option <- getLine
    case option of
        "1" -> createSession >> manageSessions user
        "2" -> editSession user >> manageSessions user
        "3" -> removeSession >> manageSessions user
        "4" -> listSessions >> manageSessions user
        "5" -> putStrLn "Voltando ao menu anterior."
        _   -> putStrLn "Opção inválida. Tente novamente.\n" >> manageSessions user
