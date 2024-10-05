:- dynamic user/2.

% Função para adicionar um usuário
add_user :-
    write("Digite o e-mail do usuário: "), nl,
    read(Email),
    write("Digite a pontuação do usuário: "), nl,
    read(Points),
    assertz(user(Email, Points)),
    write("Usuário adicionado com sucesso."), nl,
    save_users.

% Função para listar os usuários
list_users :-
    findall((Email, Points), user(Email, Points), Users),
    write("Lista de usuários:"), nl,
    print_users(Users).

% Auxiliar para imprimir os usuários
print_users([]).
print_users([(Email, Points)|T]) :-
    format("Usuario: ~w | Pontos: ~w", [Email, Points]), nl,
    print_users(T).

% Função para editar um usuário existente
edit_user :-
    list_users,
    write("Digite o e-mail do usuário a ser editado: "), nl,
    read(EmailToEdit),
    (   retract(user(EmailToEdit, _)) ->
        write("Digite o novo e-mail do usuário: "), nl,
        read(NewEmail),
        write("Digite a nova pontuação do usuário: "), nl,
        read(NewPoints),
        assertz(user(NewEmail, NewPoints)),
        write("Usuário editado com sucesso."), nl,
        save_users
    ;   write("Usuário não encontrado."), nl
    ).

% Função para remover um usuário
remove_user :-
    list_users,
    write("Digite o e-mail do usuário a ser removido: "), nl,
    read(EmailToRemove),
    (   retract(user(EmailToRemove, _)) ->
        write("Usuário removido com sucesso."), nl,
        save_users
    ;   write("Usuário não encontrado."), nl
    ).

% Função para gerenciar usuários
manage_users :-
    write("Gerenciamento de Usuários:"), nl,
    write("1) Adicionar Usuário"), nl,
    write("2) Editar Usuário"), nl,
    write("3) Remover Usuário"), nl,
    write("4) Voltar"), nl,
    read(Option),
    manage_option(Option).

manage_option(1) :- add_user, manage_users.
manage_option(2) :- edit_user, manage_users.
manage_option(3) :- remove_user, manage_users.
manage_option(4) :- write("Voltando ao menu anterior."), nl.
manage_option(_) :-
    write("Opção inválida. Tente novamente."), nl,
    manage_users.

% Função para salvar usuários no arquivo
save_users :-
    open('usersCinema.txt', write, Stream),
    forall(user(Email, Points),
        format(Stream, "Usuario: ~w | Pontos: ~w~n", [Email, Points])
    ),
    close(Stream).

% Função para carregar usuários do arquivo
load_users :-
    open('usersCinema.txt', read, Stream),
    repeat,
    read_line_to_string(Stream, Line),
    (   Line == end_of_file ->
        close(Stream), !
    ;   split_string(Line, ":| ", "", Parts),
        nth1(2, Parts, Email),
        nth1(4, Parts, PointsString),
        number_string(Points, PointsString),
        assertz(user(Email, Points)),
        fail
    ).
