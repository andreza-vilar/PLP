module ReportGeneration where

generateSalesReport :: IO ()
generateSalesReport = putStrLn "Gerar relatório de vendas de ingressos."

generateOccupancyReport :: IO ()
generateOccupancyReport = putStrLn "Gerar relatório de ocupação das salas."

generateReports :: IO ()
generateReports = do
  putStrLn "1) Relatório de Vendas"
  putStrLn "2) Relatório de Ocupação"
  option <- getLine
  case option of
    "1" -> generateSalesReport
    "2" -> generateOccupancyReport
    _   -> putStrLn "Opção inválida. Tente novamente."