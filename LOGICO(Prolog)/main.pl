% Importa os modulos necessários
:- consult('filmes.pl').  % Apenas o modulo de filmes por enquanto
% Adicione os outros modulos conforme necessário

% Função principal para iniciar o sistema
iniciar :- 
    writeln('Bem-vindo ao Sistema de Gerenciamento do Cinema!'),
    writeln('Você é cliente ou funcionário?'),
    writeln('1. Funcionário'),
    writeln('2. Cliente'),
    writeln('3. Sair'),
    read(Opcao),
    processar_opcao(Opcao).

% Processar a opção inicial do usuário
processar_opcao(1) :- autenticar_funcionario.
processar_opcao(2) :- menu_cliente.
processar_opcao(3) :- 
    writeln('Saindo do sistema. Até logo!').
processar_opcao(_) :- 
    writeln('Opção inválida. Tente novamente.'), 
    iniciar.

% Autenticação do funcionário
autenticar_funcionario :-
    writeln('Por favor, insira a senha de administrador:'),
    read(Senha),
    validar_senha(Senha).

% Validação da senha do funcionário
validar_senha(Senha) :-
    Senha == 'admin',
    writeln('Autenticação bem-sucedida!'),
    menu_funcionario.

validar_senha(_) :-
    writeln('Senha incorreta. Tente novamente.'),
    iniciar.

% Menu para funcionários com acesso privilegiado
menu_funcionario :- 
    writeln('Menu do Funcionário:'),
    writeln('1. Gerenciar filmes'),
    writeln('2. Gerenciar sessões'),
    writeln('3. Gerenciar usuários'),
    writeln('4. Gerar relatórios'),
    writeln('5. Gerenciar bomboniere'),
    writeln('6. Voltar ao menu principal'),
    writeln('7. Sair'),  % Adiciona a opção de sair
    read(OpcaoFuncionario),
    processar_opcao_funcionario(OpcaoFuncionario).

% Processamento das opções do funcionário
processar_opcao_funcionario(1) :- gerenciar_filmes, menu_funcionario.
processar_opcao_funcionario(2) :- menu_sessoes, menu_funcionario.
processar_opcao_funcionario(3) :- menu_usuarios, menu_funcionario.
processar_opcao_funcionario(4) :- gerar_relatorio_avaliacoes, menu_funcionario.
processar_opcao_funcionario(5) :- menu_bomboniere, menu_funcionario.
processar_opcao_funcionario(6) :- iniciar.  % Volta ao menu principal
processar_opcao_funcionario(7) :- 
    writeln('Voltando ao menu anterior...'), 
    iniciar.  % Retorna ao menu anterior
processar_opcao_funcionario(_) :- 
    writeln('Opção inválida. Tente novamente.'),
    menu_funcionario.


% Menu para clientes (exemplo, você pode expandir)
menu_cliente :- 
    writeln('Menu do Cliente:'),
    writeln('1. Comprar ingresso'),
    writeln('2. Avaliar sala/comida'),
    writeln('3. Voltar ao menu principal'),
    writeln('4. Sair'),
    read(OpcaoCliente),
    processar_opcao_cliente(OpcaoCliente).

processar_opcao_cliente(1) :- writeln('Comprar ingresso.').
processar_opcao_cliente(2) :- writeln('Avaliar sala/comida.').
processar_opcao_cliente(3) :- iniciar.
processar_opcao_cliente(4) :- 
    writeln('Saindo do sistema. Até logo!').
processar_opcao_cliente(_) :- 
    writeln('Opção inválida. Tente novamente.'),
    menu_cliente.
