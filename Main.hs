module Main where

import MovieManagement ( manageMovies )
import SessionManagement ( manageSessions, UserType(Employee) )
import UserManagement ( manageUsers )
import ReportGeneration ( generateReports )
import ConcessionStand ( manageConcessionStand )
import ClientInterface (runClientMode) -- Importa a função runClientMode do módulo ClientInterface

-- Função para validar a senha do funcionário
validateEmployeePassword :: IO Bool
validateEmployeePassword = do
  putStrLn "Digite a senha de acesso para o modo Funcionário:"
  password <- getLine
  putStr "\n"
  return (password == "admin")

runEmployeeMode :: IO ()
runEmployeeMode = do
  valid <- validateEmployeePassword
  if valid
    then employeeMenu
    else do
      putStrLn "Senha incorreta. Retornando ao menu principal.\n"
      main -- Retorna ao menu principal

employeeMenu :: IO ()
employeeMenu = do
  putStrLn "Modo Funcionário:"
  putStrLn "1) Gerenciamento de Filmes"
  putStrLn "2) Gerenciamento de Sessões"
  putStrLn "3) Administração de Usuários"
  putStrLn "4) Geração de Relatórios"
  putStrLn "5) Gerenciamento da Bomboniere"
  putStrLn "6) Voltar ao Menu Principal\n"
  option <- getLine
  putStr "\n"
  case option of
    "1" -> manageMovies >> employeeMenu
    "2" -> manageSessions Employee >> employeeMenu  -- Passa 'Employee' como argumento
    "3" -> manageUsers >> employeeMenu
    "4" -> generateReports >> employeeMenu
    "5" -> manageConcessionStand >> employeeMenu
    "6" -> main -- Volta ao menu principal
    _   -> putStrLn "Opção inválida. Tente novamente." >> employeeMenu


main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Cinema!"
  putStrLn "Você é: 1) Funcionário 2) Cliente 3) Sair"
  userType <- getLine 
  putStr "\n"
  case userType of
    "1" -> runEmployeeMode
    "2" -> runClientMode >> main -- Chama runClientMode e volta ao menu principal
    "3" -> putStrLn "Encerrando o sistema."
    _   -> putStrLn "Opção inválida. Tente novamente." >> main