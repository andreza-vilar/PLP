:- use_module(library(lists)).
:- use_module(library(readutil)).
:- dynamic session/6.

% Função para verificar se o usuário é funcionário
is_employee(employee).

% Adiciona uma sessão ao arquivo e à base de dados dinâmica
add_session(ID, Title, Time, Room, Date, Price, Audio) :-
    write('Adicionar nova sessão: Informe o título do filme.\n'), read(Title),
    write('Informe o horário da sessão (por exemplo, 15:00).\n'), read(Time),
    write('Informe a sala (por exemplo, Sala 3).\n'), read(Room),
    write('Informe a data de exibição do filme no formato DD/MM.\n'), read(Date),
    write('Informe o preço do ingresso (por exemplo, 20.50).\n'), read(Price),
    write('Informe o tipo de áudio (Legendado ou Dublado).\n'), read(Audio),
    assertz(session(Title, Time, Room, Date, Price, Audio)),
    save_sessions_to_file,
    write('Sessão adicionada com sucesso.\n').

% Edita uma sessão existente (Apenas Funcionários)
edit_session(employee) :-
    write('Editar sessão: Informe o título do filme da sessão que deseja editar.\n'), read(Title),
    (   session(Title, _, _, _, _, _)
    ->  write('Informe o novo horário da sessão (por exemplo, 17:00).\n'), read(NewTime),
        write('Informe a nova sala (por exemplo, Sala 4).\n'), read(NewRoom),
        write('Informe a nova data de exibição do filme (por exemplo, 2024-08-25).\n'), read(NewDate),
        write('Informe o novo preço do ingresso (por exemplo, 25.00).\n'), read(NewPrice),
        write('Informe o novo tipo de áudio (Legendado ou Dublado).\n'), read(NewAudio),
        retract(session(Title, _, _, _, _, _)),
        assertz(session(Title, NewTime, NewRoom, NewDate, NewPrice, NewAudio)),
        save_sessions_to_file,
        write('Sessão editada com sucesso.\n')
    ;   write('Sessão não encontrada.\n')).

% Remove uma sessão (Apenas Funcionários)
remove_session(employee) :-
    write('Remover sessão: Informe o título do filme da sessão que deseja remover.\n'), read(Title),
    (   session(Title, _, _, _, _, _)
    ->  retract(session(Title, _, _, _, _, _)),
        save_sessions_to_file,
        write('Sessão removida com sucesso.\n')
    ;   write('Sessão não encontrada.\n')).

% Função para listar as sessões
view_sessions :-
    (   session(_, _, _, _, _, _)
    ->  forall(session(Title, Time, Room, Date, Price, Audio),
               (   format('Filme: ~w\nHorário: ~w\nSala: ~w\nData: ~w\nPreço do Ingresso: R$ ~2f\nTipo de Áudio: ~w\n------------------------\n',
                       [Title, Time, Room, Date, Price, Audio])))
    ;   write('Nenhuma sessão disponível.\n')).

% Função para salvar as sessões no arquivo
save_sessions_to_file :-
    open('sessions.txt', write, Stream),
    forall(session(Title, Time, Room, Date, Price, Audio),
           format(Stream, 'session(~w,~w,~w,~w,~2f,~w).\n', [Title, Time, Room, Date, Price, Audio])),
    close(Stream).

% Função para carregar as sessões do arquivo
load_sessions_from_file :-
    exists_file('sessions.txt'),
    open('sessions.txt', read, Stream),
    repeat,
    read(Stream, Term),
    ( Term == end_of_file -> true;
      assertz(Term), fail
    ),
    close(Stream).

% Menu de gerenciamento de sessões
manage_sessions(employee) :-
    write('1) Criar Sessão\n'),
    write('2) Editar Sessão\n'),
    write('3) Remover Sessão\n'),
    write('4) Visualizar Sessões\n'),
    write('5) Voltar ao Menu Principal\n'),
    read(Option),
    (   Option == 1 -> add_session(employee), manage_sessions(employee);
        Option == 2 -> edit_session(employee), manage_sessions(employee);
        Option == 3 -> remove_session(employee), manage_sessions(employee);
        Option == 4 -> view_sessions, manage_sessions(employee);
        Option == 5 -> write('Voltando ao menu principal...\n');
        write('Opção inválida. Tente novamente.\n'), manage_sessions(employee)
    ).

% Carrega as sessões ao iniciar o programa
session_management_main :-
    load_sessions_from_file,
    write('Informe o tipo de usuário (employee ou customer):\n'),
    read(UserType),
    (   UserType == employee -> manage_sessions(employee);
        UserType == customer -> view_sessions;
        write('Tipo de usuário inválido.\n')
    ).