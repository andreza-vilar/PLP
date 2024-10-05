:- use_module(library(readutil)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(pio)). % Para manipulação de arquivos

% Função para adicionar um usuário
add_user :-
    write("Digite o e-mail do usuário:"), nl,
    read(Email),
    write("Digite a pontuação do usuário:"), nl,
    read(Points),
    format(string(UserEntry), "Usuario: ~w | Pontos: ~w~n", [Email, Points]),
    open('usersCinema.txt', append, Stream),
    write(Stream, UserEntry),
    close(Stream),
    write("Usuário adicionado com sucesso."), nl.

% Lista os usuários para facilitar a visualização na hora de remover
list_users :-
    read_file_to_string('usersCinema.txt', Content, []),
    split_string(Content, "\n", "", Users),
    write("Lista de usuários:"), nl,
    forall(member(User, Users), (sub_string(User, _, _, _, "Usuario: "), write(User), nl)).

% Função para editar um usuário existente
edit_user :-
    list_users,
    write("Digite o e-mail do usuário a ser editado:"), nl,
    read(EmailToEdit),
    read_file_to_string('usersCinema.txt', Content, []),
    split_string(Content, "\n", "", Users),
    findall(User, (member(User, Users), sub_string(User, _, _, _, EmailToEdit)), UserToEdit),
    ( UserToEdit = [] ->
        write("Usuário não encontrado."), nl
    ;   write("Digite o novo e-mail do usuário:"), nl,
        read(NewEmail),
        write("Digite a nova pontuação do usuário:"), nl,
        read(NewPoints),
        format(string(NewUserEntry), "Usuario: ~w | Pontos: ~w", [NewEmail, NewPoints]),
        updated_users(Users, EmailToEdit, NewUserEntry, UpdatedUsers),
        open('usersCinema.txt', write, Stream),
        forall(member(User, UpdatedUsers), write(Stream, User)),
        close(Stream),
        write("Usuário editado com sucesso."), nl
    ).

% Atualiza a lista de usuários
updated_users([], _, _, []).
updated_users([User|Rest], EmailToEdit, NewUserEntry, [NewUserEntry|Rest]) :-
    sub_string(User, _, _, _, EmailToEdit), !.
updated_users([User|Rest], EmailToEdit, NewUserEntry, [User|UpdatedRest]) :-
    updated_users(Rest, EmailToEdit, NewUserEntry, UpdatedRest).

% Função para remover um usuário
remove_user :-
    list_users,
    write("Digite o e-mail do usuário a ser removido:"), nl,
    read(EmailToRemove),
    read_file_to_string('usersCinema.txt', Content, []),
    split_string(Content, "\n", "", Users),
    findall(User, (member(User, Users), sub_string(User, _, _, _, EmailToRemove)), UserToRemove),
    ( UserToRemove = [] ->
        write("Usuário não encontrado."), nl
    ;   exclude(contains_email(EmailToRemove), Users, UpdatedUsers),
        open('usersCinema.txt', write, Stream),
        forall(member(User, UpdatedUsers), write(Stream, User)),
        close(Stream),
        write("Usuário removido com sucesso."), nl
    ).

% Função auxiliar para verificar se a string contém o e-mail
contains_email(Email, User) :-
    sub_string(User, _, _, _, Email).

% Função para gerenciar usuários
manage_users :-
    write("Gerenciamento de Usuários:"), nl,
    write("1) Adicionar Usuário"), nl,
    write("2) Editar Usuário"), nl,
    write("3) Remover Usuário"), nl,
    write("4) Voltar"), nl,
    read(Option),
    handle_option(Option).

handle_option(1) :- add_user, manage_users.
handle_option(2) :- edit_user, manage_users.
handle_option(3) :- remove_user, manage_users.
handle_option(4) :- write("Voltando ao menu anterior."), nl.
handle_option(_) :- write("Opção inválida. Tente novamente."), nl, manage_users.
