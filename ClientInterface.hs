module ClientInterface where

viewMovies :: IO ()
viewMovies = putStrLn "Visualizar horários e sessões disponíveis."

buyTicket :: IO ()
buyTicket = putStrLn "Comprar um ingresso."

giveFeedback :: IO ()
giveFeedback = putStrLn "Deixar um feedback sobre o filme, sala ou comida."

runClientMode :: IO ()
runClientMode = do
  putStrLn "1) Visualizar Filmes"
  putStrLn "2) Comprar Ingresso"
  putStrLn "3) Deixar Feedback"
  option <- getLine
  case option of
    "1" -> viewMovies
    "2" -> buyTicket
    "3" -> giveFeedback
    _   -> putStrLn "Opção inválida. Tente novamente."