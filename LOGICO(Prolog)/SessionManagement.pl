:- dynamic session/6.

% session(Titulo, Horario, Sala, Data, Preco, TipoAudio).

% Adiciona uma nova sessão (Apenas Funcionários)
add_session(Employee) :-
    Employee = 'Employee',
    write('Informe o título do filme: '), read(Title),
    write('Informe o horário da sessão (por exemplo, 15:00): '), read(Time),
    write('Informe a sala (por exemplo, Sala 3): '), read(Room),
    write('Informe a data de exibição do filme no formato DD/MM (por exemplo, 25/08): '), read(Date),
    write('Informe o preço do ingresso (por exemplo, 20.50): '), read(Price),
    write('Informe o tipo de áudio (Legendado ou Dublado): '), read(Audio),
    assertz(session(Title, Time, Room, Date, Price, Audio)),
    save_sessions,
    write('Sessão adicionada com sucesso.'), nl.

% Edita uma sessão existente (Apenas Funcionários)
edit_session(Employee) :-
    Employee = 'Employee',
    write('Informe o título do filme da sessão que deseja editar: '), read(Title),
    session(Title, _, _, _, _, _),  % Verifica se existe a sessão
    write('Informe o novo horário da sessão (por exemplo, 17:00): '), read(NewTime),
    write('Informe a nova sala (por exemplo, Sala 4): '), read(NewRoom),
    write('Informe a nova data de exibição do filme (por exemplo, 25/08): '), read(NewDate),
    write('Informe o novo preço do ingresso (por exemplo, 25.00): '), read(NewPrice),
    write('Informe o novo tipo de áudio (Legendado ou Dublado): '), read(NewAudio),
    retract(session(Title, _, _, _, _, _)),  % Remove a sessão anterior
    assertz(session(Title, NewTime, NewRoom, NewDate, NewPrice, NewAudio)),
    save_sessions,
    write('Sessão editada com sucesso.'), nl.

% Remove uma sessão (Apenas Funcionários)
remove_session(Employee) :-
    Employee = 'Employee',
    write('Informe o título do filme da sessão que deseja remover: '), read(Title),
    retract(session(Title, _, _, _, _, _)),
    save_sessions,
    write('Sessão removida com sucesso.'), nl.

% Exibe todas as sessões (Clientes e Funcionários)
view_sessions(_) :-
    forall(session(Title, Time, Room, Date, Price, Audio),
        (   write('Filme: '), write(Title), nl,
            write('Horário: '), write(Time), nl,
            write('Sala: '), write(Room), nl,
            write('Data: '), write(Date), nl,
            write('Preço do Ingresso: R$ '), write(Price), nl,
            write('Tipo de Áudio: '), write(Audio), nl,
            write('------------------------'), nl
        )
    ).

% Função para salvar as sessões no arquivo
save_sessions :-
    open('sessions.txt', write, Stream),
    forall(session(Title, Time, Room, Date, Price, Audio),
        (   write(Stream, session(Title, Time, Room, Date, Price, Audio)), write(Stream, '.'), nl(Stream)
        )),
    close(Stream).

% Função para carregar as sessões do arquivo
load_sessions :-
    exists_file('sessions.txt'),
    open('sessions.txt', read, Stream),
    repeat,
    read(Stream, Session),
    (   Session == end_of_file -> true;
        assertz(Session), fail
    ),
    close(Stream).

% Menu de gerenciamento de sessões
manage_sessions(UserType) :-
    write('1) Criar Sessão'), nl,
    write('2) Editar Sessão'), nl,
    write('3) Remover Sessão'), nl,
    write('4) Visualizar Sessões'), nl,
    write('5) Voltar ao Menu Principal'), nl,
    read(Option),
    handle_option(UserType, Option).

% Tratamento das opções do menu
handle_option(UserType, '1') :-
    add_session(UserType), manage_sessions(UserType).
handle_option(UserType, '2') :-
    edit_session(UserType), manage_sessions(UserType).
handle_option(UserType, '3') :-
    remove_session(UserType), manage_sessions(UserType).
handle_option(UserType, '4') :-
    view_sessions(UserType), manage_sessions(UserType).
handle_option(_, '5') :-
    write('Voltando ao menu principal...'), nl.
handle_option(UserType, _) :-
    write('Opção inválida. Tente novamente.'), nl,
    manage_sessions(UserType).

% Função principal de inicialização
session_management_main :-
    load_sessions,
    write('Informe o tipo de usuário (Employee ou Customer): '), read(UserType),
    manage_sessions(UserType).
