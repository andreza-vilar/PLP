module Main where

import MovieManagement
import SessionManagement
import UserManagement
import ReportGeneration
import ConcessionStand
import ClientInterface

runEmployeeMode :: IO ()
runEmployeeMode = do
  putStrLn "Modo Funcionário:"
  putStrLn "1) Gerenciamento de Filmes"
  putStrLn "2) Gerenciamento de Sessões"
  putStrLn "3) Administração de Usuários"
  putStrLn "4) Geração de Relatórios"
  putStrLn "5) Gerenciamento da Bomboniere"
  option <- getLine
  case option of
    "1" -> manageMovies
    "2" -> manageSessions
    "3" -> manageUsers
    "4" -> generateReports
    "5" -> manageConcessionStand
    _   -> putStrLn "Opção inválida. Tente novamente."

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Cinema!"
  putStrLn "Você é: 1) Funcionário 2) Cliente"
  userType <- getLine
  case userType of
    "1" -> runEmployeeMode
    "2" -> runClientMode
    _   -> putStrLn "Opção inválida. Tente novamente."