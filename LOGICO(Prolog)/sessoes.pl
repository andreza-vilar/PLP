:- dynamic sessao/4.  % Dinâmico para permitir adição e remoção

% Carregar sessões do arquivo ao iniciar o sistema
carregar_sessoes :-
    open('sessoes.txt', read, Stream),
    repeat,
    read(Stream, Sessao),
    (   Sessao == end_of_file -> ! ;
        assertz(Sessao),
        fail
    ),
    close(Stream).

% Salvar sessões no arquivo
salvar_sessoes :-
    open('sessoes.txt', write, Stream),
    forall(sessao(ID, FilmeID, Sala, Horario),
           writeln(Stream, sessao(ID, FilmeID, Sala, Horario))),
    write(Stream, end_of_file),
    close(Stream).

% Adicionar sessão
adicionar_sessao(ID, FilmeID, Sala, Horario) :-
    assertz(sessao(ID, FilmeID, Sala, Horario)),
    salvar_sessoes.

% Editar sessão
editar_sessao(ID, NovoFilmeID, NovaSala, NovoHorario) :-
    retract(sessao(ID, _, _, _)),
    assertz(sessao(ID, NovoFilmeID, NovaSala, NovoHorario)),
    salvar_sessoes.

% Remover sessão
remover_sessao(ID) :-
    retract(sessao(ID, _, _, _)),
    salvar_sessoes.

% Exibir sessões
exibir_sessoes :-
    findall(sessao(ID, FilmeID, Sala, Horario), sessao(ID, FilmeID, Sala, Horario), Sessoes),
    forall(member(Sessao, Sessoes), writeln(Sessao)).
