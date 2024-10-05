% Importação dos modulos necessários
:- ensure_loaded('MovieManagement').
:- ensure_loaded('SessionManagement').
:- ensure_loaded('UserManagement').
:- ensure_loaded('ConcessionStand').
:- ensure_loaded('ReviewManagement').
:- ensure_loaded('ClientInterface').
:- ensure_loaded('FAQ').

% Exibir a mensagem de boas-vindas
show_welcome_message :-
    write('████████╗ █████╗ ██████╗ ███████╗██████╗  ██████╗  █████╗ '), nl,
    write('╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██╔══██╗██╔═══██╗██╔══██╗'), nl,
    write('   ██║   ███████║██████╔╝█████╗  ██████╔╝██║   ██║███████║'), nl,
    write('   ██║   ██╔══██║██╔═══╝ ██╔══╝  ██╔══██╗██║   ██║██╔══██║'), nl,
    write('   ██║   ██║  ██║██║     ███████╗██║  ██║╚██████╔╝██║  ██║'), nl,
    write('   ╚═╝   ╚═╝  ╚═╝╚═╝     ╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═╝'), nl,
    write('███████╗██╗██╗     ███╗   ███╗███████╗███████║    ██╗     '), nl,
    write('██╔════╝██║██║     ████╗ ████║██╔════╝██╔════╝    ██║     '), nl,
    write('█████╗  ██║██║     ██╔████╔██║█████╗  ███████╗    ██║     '), nl,
    write('██╔══╝  ██║██║     ██║╚██╔╝██║██╔══╝  ╚════██║    ╚═╝     '), nl,
    write('██║     ██║███████╗██║ ╚═╝ ██║███████╗███████║    ██╗   '), nl.

% Validação da senha do funcionário
validate_employee_password :-
    write('Digite a senha de acesso para o modo Funcionário:'), nl,
    read(Password),
    ( Password = 'admin' ->
        true
    ;   write('Senha incorreta. Retornando ao menu principal.'), nl,
        fail ).

% Geração de relatórios de avaliação
generate_reports :-
    calculate_average_rating(InfraAvg, FoodAvg),
    write('Relatório de Avaliações:'), nl,
    write('Média das avaliações de infraestrutura: '), write(InfraAvg), nl,
    write('Média das avaliações de comida: '), write(FoodAvg), nl.

% Modo funcionário
run_employee_mode :-
    ( validate_employee_password ->
        employee_menu
    ;   main_menu
    ).

% Menu do funcionário
employee_menu :-
    write('Modo Funcionário:'), nl,
    write('1) Gerenciamento de Filmes'), nl,
    write('2) Gerenciamento de Sessões'), nl,
    write('3) Administração de Usuários'), nl,
    write('4) Geração de Relatórios'), nl,
    write('5) Gerenciamento da Bomboniere'), nl,
    write('6) Voltar ao Menu Principal'), nl,
    read(Option),
    handle_employee_option(Option).

handle_employee_option(1) :-
    manage_movies,
    employee_menu.
handle_employee_option(2) :-
    manage_sessions,  % Chamada corrigida para manage_sessions/0
    employee_menu.
handle_employee_option(3) :-
    manage_users,
    employee_menu.
handle_employee_option(4) :-
    generate_reports,
    employee_menu.
handle_employee_option(5) :-
    manage_concession_stand,
    employee_menu.
handle_employee_option(6) :-
    main_menu.
handle_employee_option(_) :-
    write('Opção inválida. Tente novamente.'), nl,
    employee_menu.

% Menu principal
main_menu :-
    write('Você é: 1) Funcionário 2) Cliente 3) Sair'), nl,
    read(UserType),
    handle_main_menu_option(UserType).

handle_main_menu_option(1) :-
    run_employee_mode.
handle_main_menu_option(2) :-
    run_client_mode,
    main_menu.
handle_main_menu_option(3) :-
    write('Encerrando o sistema.'), nl.
handle_main_menu_option(_) :-
    write('Opção inválida. Tente novamente.'), nl,
    main_menu.

% Função principal (inicial)
main :-
    show_welcome_message,
    main_menu.

% Alias para a função principal
iniciar :- main.
