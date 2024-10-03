% Inicializa o sistema
:- dynamic(filme/5).  % Declara que filme/5 pode ser dinâmico (adicionado/removido)

iniciar :- 
    write('Bem-vindo ao Sistema de Gerenciamento de Cinema!'), nl,
    write('Escolha o modo de operação:'), nl,
    write('1. Modo Funcionário'), nl,
    write('2. Modo Cliente'), nl,
    read(Opcao),
    selecionar_modo(Opcao).

% Seleciona o modo de operação
selecionar_modo(1) :- 
    entrar_modo_funcionario.
selecionar_modo(2) :- 
    entrar_modo_cliente.
selecionar_modo(_) :- 
    write('Opção inválida, tente novamente.'), nl,
    iniciar.

% Modo funcionário com verificação de senha
entrar_modo_funcionario :- 
    write('Digite a senha de administrador: '), nl,
    read(Senha),
    (Senha = admin -> 
        write('Bem-vindo, Funcionario!'), nl, 
        menu_funcionario; 
        write('Senha incorreta! Acesso negado.'), nl, 
        iniciar).

% Modo cliente
entrar_modo_cliente :- 
    write('Bem-vindo, Cliente!'), nl, 
    menu_cliente.

% Menu de opções para o funcionário
menu_funcionario :- 
    write('Escolha uma opção:'), nl,
    write('1. Gerenciar Filmes'), nl,
    write('2. Gerenciar Sessões'), nl,
    write('3. Gerenciar Usuários'), nl,
    write('4. Gerar Relatórios'), nl,
    write('5. Sair'), nl,
    read(Opcao),
    executar_opcao_funcionario(Opcao).

% Menu de opções para o cliente
menu_cliente :- 
    write('Escolha uma opção:'), nl,
    write('1. Visualizar Filmes'), nl,
    write('2. Comprar Ingressos'), nl,
    write('3. Avaliar Sessões'), nl,
    write('4. Sair'), nl,
    read(Opcao),
    executar_opcao_cliente(Opcao).

% Executa a opção escolhida pelo funcionário
executar_opcao_funcionario(1) :- 
    gerenciar_filmes.
executar_opcao_funcionario(5) :- 
    write('Saindo...'), nl.
executar_opcao_funcionario(_) :- 
    write('Opção inválida, tente novamente.'), nl, 
    menu_funcionario.

% Gerenciar filmes
gerenciar_filmes :-
    write('Gerenciamento de Filmes.'), nl,
    write('Escolha uma opção:'), nl,
    write('1. Adicionar Filme'), nl,
    write('2. Editar Filme'), nl,
    write('3. Remover Filme'), nl,
    write('4. Listar Filmes'), nl,
    write('5. Voltar'), nl,
    read(Op), 
    executar_opcao_filmes(Op).

% Executa a opção escolhida no gerenciamento de filmes
executar_opcao_filmes(1) :- 
    adicionar_filme.
executar_opcao_filmes(2) :- 
    write('Editar Filme (implementação ainda não feita).'), nl,
    gerenciar_filmes.
executar_opcao_filmes(3) :- 
    write('Remover Filme (implementação ainda não feita).'), nl,
    gerenciar_filmes.
executar_opcao_filmes(4) :- 
    listar_filmes,
    gerenciar_filmes.
executar_opcao_filmes(5) :- 
    menu_funcionario.
executar_opcao_filmes(_) :- 
    write('Opção inválida, tente novamente.'), nl,
    gerenciar_filmes.

% Adiciona um filme à lista
adicionar_filme :-
    write('Digite o nome do filme: '), nl,
    read(Nome),
    write('Digite o diretor do filme: '), nl,
    read(Diretor),
    write('Digite o ano de lançamento: '), nl,
    read(Ano),
    write('Digite a classificação (por exemplo, PG-13): '), nl,
    read(Classificacao),
    write('Digite a duração em minutos: '), nl,
    read(Duracao),
    assertz(filme(Nome, Diretor, Ano, Classificacao, Duracao)),  % Adiciona o filme
    write('Filme adicionado com sucesso!'), nl,
    gerenciar_filmes.

% Lista todos os filmes
listar_filmes :-
    write('Filmes cadastrados:'), nl,
    findall(Nome, filme(Nome, _, _, _, _), Lista),  % Encontra todos os nomes dos filmes
    (Lista \= [] -> 
        listar_filmes_aux(Lista); 
        write('Nenhum filme cadastrado.'), nl).

% Função auxiliar para listar os filmes
listar_filmes_aux([]).
listar_filmes_aux([H|T]) :- 
    filme(H, Diretor, Ano, Classificacao, Duracao), 
    format('Nome: ~w, Diretor: ~w, Ano: ~w, Classificação: ~w, Duração: ~w minutos~n', [H, Diretor, Ano, Classificacao, Duracao]),
    listar_filmes_aux(T).

% Executa a opção escolhida pelo cliente
executar_opcao_cliente(1) :- 
    write('Visualização de Filmes.'), nl,
    menu_cliente.
executar_opcao_cliente(4) :- 
    write('Saindo...'), nl.
executar_opcao_cliente(_) :- 
    write('Opção inválida, tente novamente.'), nl,
    menu_cliente.

% Para iniciar o sistema, chame:
% ?- iniciar.
