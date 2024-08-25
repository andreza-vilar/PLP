module SessionManagement where

data UserType = Employee | Customer deriving (Eq, Show)

-- Função para verificar se o usuário é funcionário
isEmployee :: UserType -> Bool
isEmployee Employee = True
isEmployee _        = False

-- Função para editar uma sessão de cinema
editSession :: UserType -> IO ()
editSession user = 
    if isEmployee user
    then do
        putStrLn "Editar sessão: Informe os novos detalhes."
        -- Aqui você poderia adicionar o código para editar a sessão,
        -- como atualizar o banco de dados ou a estrutura de dados.
        putStrLn "Sessão editada com sucesso para funcionários e clientes."
    else
        putStrLn "Apenas funcionários podem editar sessões."

-- Exemplo de uso da função principal de gerenciamento de sessões
manageSessions :: UserType -> IO ()
manageSessions user = do
    putStrLn "1) Criar Sessão"
    putStrLn "2) Editar Sessão"
    putStrLn "3) Remover Sessão"
    option <- getLine
    case option of
        "1" -> putStrLn "Criar uma nova sessão de cinema."
        "2" -> editSession user
        "3" -> putStrLn "Remover uma sessão."
        _   -> putStrLn "Opção inválida. Tente novamente."
