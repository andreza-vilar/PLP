:- use_module(library(readutil)).

% Importando os modulos
:- consult('SessionManagement').
:- consult('CinemaInfo').
:- consult('ReviewManagement').
:- consult('FAQ').

% Função para visualizar filmes
view_movies :-
    write("Lista de filmes disponiveis:"), nl,
    read_file('movies.txt', Contents),
    write(Contents), nl.

% Função para visualizar lançamentos futuros
view_upcoming_movies :-
    write("Lista de lancamentos futuros:"), nl,
    read_file('upcoming_movies.txt', Contents),
    write(Contents), nl.

% Função para visualizar itens da bomboniere
view_items(Items) :-
    read_file_lines('concessionStand.txt', Items),
    write("Itens da bomboniere disponiveis:"), nl,
    ( Items = [] ->
        write("Nenhum item disponivel."), nl
    ; maplist(write_item, Items)
    ).

write_item(Item) :-
    write(Item), nl.

% Função para comprar um item da bomboniere
buy_item :-
    view_items(Items),
    ( Items = [] ->
        write("Voltando ao menu principal..."), nl
    ; write("Digite o nome do item que deseja comprar:"), nl,
      read_line_to_string(user_input, ItemName),
      ( member(ItemName, Items) ->
          write("Voce comprou o item: "), write(ItemName), nl
      ; write("Item nao encontrado. Voltando ao menu principal."), nl
      )
    ).

% Função para visualizar sessões de cinema
view_sessions :-
    write("Sessoes disponiveis:"), nl,
    read_file('sessions.txt', Contents),
    ( Contents = [] ->
        write("Nenhuma sessao disponivel."), nl
    ; read_term_from_atom(Contents, Sessions, []),
      maplist(print_session_details, Sessions)
    ).

% Função auxiliar para imprimir os detalhes de uma sessão
print_session_details(Session) :-
    Session = session(Title, Time, Room, Date, Price, Audio),
    write("Filme: "), write(Title), nl,
    write("Data: "), write(Date), nl,
    write("Horario: "), write(Time), nl,
    write("Sala: "), write(Room), nl,
    write("Preco do Ingresso: R$ "), write(Price), nl,
    write("Tipo de audio: "), write(Audio), nl,
    write("------------------------"), nl.

% Função para comprar um ingresso
buy_ticket :-
    write("Digite o titulo do filme para o qual deseja comprar o ingresso:"), nl,
    read_line_to_string(user_input, MovieTitle),
    write("Digite a data da sessao (DD/MM):"), nl,
    read_line_to_string(user_input, SessionDate),
    write("Digite o horario da sessao (HH:MM):"), nl,
    read_line_to_string(user_input, SessionTime),
    
    read_file('sessions.txt', Contents),
    read_term_from_atom(Contents, Sessions, []),
    ( member(session(MovieTitle, SessionTime, _, SessionDate, Price, Audio), Sessions) ->
        write("Sessao encontrada!"), nl,
        write("Filme: "), write(MovieTitle), nl,
        write("Data: "), write(SessionDate), nl,
        write("Horario: "), write(SessionTime), nl,
        write("Preco do Ingresso: R$ "), write(Price), nl,
        write("Tipo de audio: "), write(Audio), nl,
        write("Voce eh estudante e tem carteirinha? (s/n):"), nl,
        read_line_to_string(user_input, IsStudent),
        ( IsStudent = "s" ->
            FinalPrice is Price * 0.5
        ; FinalPrice is Price
        ),
        write("Preco final do Ingresso: R$ "), write(FinalPrice), nl,
        write("Digite 's' para confirmar a compra ou qualquer outra tecla para cancelar:"), nl,
        read_line_to_string(user_input, Confirmation),
        ( Confirmation = "s" ->
            write("Ingresso comprado com sucesso!"), nl
        ; write("Compra cancelada."), nl
        )
    ; write("Sessao nao encontrada."), nl,
      write("1) Tentar novamente"), nl,
      write("2) Voltar ao menu anterior"), nl,
      read_line_to_string(user_input, Option),
      ( Option = "1" -> buy_ticket
      ; true
      )
    ).

% Função para deixar feedback
give_feedback :-
    leave_review.

% Função auxiliar para ler o conteúdo de um arquivo
read_file(File, Contents) :-
    open(File, read, Stream),
    read_string(Stream, _, Contents),
    close(Stream).

% Função auxiliar para ler linhas de um arquivo
read_file_lines(File, Lines) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

% Menu do cliente
run_client_mode :-
    write("Modo Cliente:"), nl,
    write("1) Visualizar Filmes"), nl,
    write("2) Visualizar Sessoes Disponiveis"), nl,
    write("3) Visualizar Lancamentos Futuros"), nl,
    write("4) Comprar Ingresso"), nl,
    write("5) Comprar Item da Bomboniere"), nl,
    write("6) Visualizar Informacoes do Cinema"), nl,
    write("7) Deixar Feedback"), nl,
    write("8) Acessar FAQ"), nl,
    write("9) Voltar ao Menu Principal"), nl,

    read_line_to_string(user_input, Option),
    ( Option = "1" -> view_movies, run_client_mode
    ; Option = "2" -> view_sessions, run_client_mode
    ; Option = "3" -> view_upcoming_movies, run_client_mode
    ; Option = "4" -> buy_ticket, run_client_mode
    ; Option = "5" -> buy_item, run_client_mode
    ; Option = "6" -> view_cinema_info, run_client_mode
    ; Option = "7" -> give_feedback, run_client_mode
    ; Option = "8" -> view_faq, run_client_mode
    ; Option = "9" -> true
    ; write("Opcao invalida. Tente novamente."), nl, run_client_mode
    ).
