:- dynamic session/6.  % Para permitir a adição e remoção dinâmica de sessões

% Adiciona uma sessão ao arquivo e à base de dados dinâmica
add_session(ID, Title, Time, Room, Date, Price, Audio) :-
    assertz(session(ID, Title, Time, Room, Date, Price, Audio)),  % Adiciona ao banco de dados dinâmico
    open('sessions.txt', append, Stream),
    write(Stream, ID), write(Stream, ';'),
    write(Stream, Title), write(Stream, ';'),
    write(Stream, Time), write(Stream, ';'),
    write(Stream, Room), write(Stream, ';'),
    write(Stream, Date), write(Stream, ';'),
    write(Stream, Price), write(Stream, ';'),
    write(Stream, Audio), write(Stream, '\n'),
    close(Stream),
    write('Sessão adicionada com sucesso!'), nl.

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

% Função para listar as sessões
list_sessions :-
    write('Lista de Sessões:'), nl,
    (   session(ID, MovieName, StartTime, EndTime, Room) ->
        format('ID: ~w, Filme: ~w, Início: ~w, Fim: ~w, Sala: ~w', [ID, MovieName, StartTime, EndTime, Room]), nl,
        fail  % Força a busca por mais sessões
    ;   write('Nenhuma sessão encontrada.'), nl
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
manage_sessions :- 
    write('Gerenciamento de Sessões:'), nl,
    write('1) Adicionar Sessão'), nl,
    write('2) Listar Sessões'), nl,
    write('3) Editar Sessão'), nl,
    write('4) Remover Sessão'), nl,
    write('5) Voltar ao Menu Funcionário'), nl,
    read(Option),
    handle_session_option(Option).

handle_session_option(1) :- 
    write('Digite o ID da sessão: '), read(ID),
    write('Digite o título do filme: '), read(Title),
    write('Digite o horário da sessão (por exemplo, 15:00): '), read(Time),
    write('Digite a sala (por exemplo, Sala 3): '), read(Room),
    write('Digite a data de exibição do filme no formato DD/MM (por exemplo, 25/08): '), read(Date),
    write('Digite o preço do ingresso (por exemplo, 20.50): '), read(Price),
    write('Digite o tipo de áudio (Legendado ou Dublado): '), read(Audio),
    add_session(ID, Title, Time, Room, Date, Price, Audio),
    manage_sessions.

handle_session_option(2) :- 
    view_sessions,  % Certifique-se de que view_sessions/0 está implementado corretamente
    manage_sessions.

handle_session_option(3) :- 
    write('Digite o ID da sessão a ser editada: '), read(EditID),
    write('Digite o novo título do filme: '), read(NewTitle),
    write('Digite o novo horário da sessão (por exemplo, 17:00): '), read(NewTime),
    write('Digite a nova sala (por exemplo, Sala 4): '), read(NewRoom),
    write('Digite a nova data de exibição do filme (por exemplo, 25/08): '), read(NewDate),
    write('Digite o novo preço do ingresso (por exemplo, 25.00): '), read(NewPrice),
    write('Digite o novo tipo de áudio (Legendado ou Dublado): '), read(NewAudio),
    edit_session(EditID, NewTitle, NewTime, NewRoom, NewDate, NewPrice, NewAudio),
    manage_sessions.

handle_session_option(4) :- 
    write('Digite o ID da sessão a ser removida: '), read(RemoveID),
    remove_session(RemoveID),
    manage_sessions.

handle_session_option(5) :- 
    write('Voltando ao Menu Funcionário...'), nl.
