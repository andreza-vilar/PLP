:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(pure_input)).
:- dynamic movie/5. % Fatos dinâmicos de filmes

% Predicado para adicionar um filme
add_movie :-
    write('Digite o título do filme:'), nl,
    read_line_to_string(user_input, Title),
    write('Digite o diretor do filme:'), nl,
    read_line_to_string(user_input, Director),
    write('Digite o ano de lançamento:'), nl,
    read_line_to_string(user_input, Year),
    write('Digite a classificação indicativa:'), nl,
    read_line_to_string(user_input, Rating),
    write('Digite a duração do filme (em minutos):'), nl,
    read_line_to_string(user_input, Duration),
    assertz(movie(Title, Director, Year, Rating, Duration)),
    write('Filme adicionado com sucesso.'), nl.

% Predicado para adicionar um filme que ainda vai entrar em cartaz
add_upcoming_movie :-
    write('Digite o título do filme (lançamento futuro):'), nl,
    read_line_to_string(user_input, Title),
    write('Digite o diretor do filme:'), nl,
    read_line_to_string(user_input, Director),
    write('Digite o ano previsto de lançamento:'), nl,
    read_line_to_string(user_input, ReleaseYear),
    write('Digite a classificação indicativa:'), nl,
    read_line_to_string(user_input, Rating),
    write('Digite a duração estimada do filme (em minutos):'), nl,
    read_line_to_string(user_input, Duration),
    write('Digite a data prevista de estreia (formato DD/MM):'), nl,
    read_line_to_string(user_input, ReleaseDate),
    assertz(upcoming_movie(Title, Director, ReleaseDate, ReleaseYear, Rating, Duration)),
    write('Filme futuro adicionado com sucesso.'), nl.

% Predicado para listar todos os filmes em cartaz
list_movies :-
    write('Lista de filmes disponíveis:'), nl,
    forall(movie(Title, Director, Year, Rating, Duration),
           format('Filme em cartaz: ~w | ~w | ~w | ~w | ~w min~n', [Title, Director, Year, Rating, Duration])).

% Predicado para listar lançamentos futuros
list_upcoming_movies :-
    write('Lista de filmes que entrarão em cartaz:'), nl,
    forall(upcoming_movie(Title, Director, ReleaseDate, ReleaseYear, Rating, Duration),
           format('Chega em breve: ~w | ~w | ~w | ~w | ~w min~n', [Title, Director, ReleaseDate, ReleaseYear, Duration])).

% Predicado para editar um filme em cartaz
edit_movie :-
    write('Digite o título do filme a ser editado:'), nl,
    read_line_to_string(user_input, TitleToEdit),
    retract(movie(TitleToEdit, Director, Year, Rating, Duration)),
    write('Digite o novo título do filme:'), nl,
    read_line_to_string(user_input, NewTitle),
    write('Digite o novo diretor do filme:'), nl,
    read_line_to_string(user_input, NewDirector),
    write('Digite o novo ano de lançamento:'), nl,
    read_line_to_string(user_input, NewYear),
    write('Digite a nova classificação indicativa:'), nl,
    read_line_to_string(user_input, NewRating),
    write('Digite a nova duração do filme (em minutos):'), nl,
    read_line_to_string(user_input, NewDuration),
    assertz(movie(NewTitle, NewDirector, NewYear, NewRating, NewDuration)),
    write('Filme editado com sucesso.'), nl.

% Predicado para remover um filme
remove_movie :-
    write('Digite o título do filme a ser removido:'), nl,
    read_line_to_string(user_input, TitleToRemove),
    retract(movie(TitleToRemove, Director, Year, Rating, Duration)),
    write('Filme removido com sucesso.'), nl.

% Predicado para remover filmes que AINDA entrarão em cartaz
remove_upcoming_movie :-
    write('Digite o título do lançamento futuro a ser removido:'), nl,
    read_line_to_string(user_input, TitleToRemove),
    retract(upcoming_movie(TitleToRemove, Director, ReleaseDate, ReleaseYear, Rating, Duration)),
    write('Lançamento futuro removido com sucesso.'), nl.

% Predicado para editar filmes em pré-lançamento
edit_upcoming_movie :-
    write('Digite o título do lançamento futuro a ser editado:'), nl,
    read_line_to_string(user_input, TitleToEdit),
    retract(upcoming_movie(TitleToEdit, Director, ReleaseDate, ReleaseYear, Rating, Duration)),
    write('Digite o novo título do filme (lançamento futuro):'), nl,
    read_line_to_string(user_input, NewTitle),
    write('Digite o novo diretor do filme:'), nl,
    read_line_to_string(user_input, NewDirector),
    write('Digite o novo ano previsto de lançamento:'), nl,
    read_line_to_string(user_input, NewReleaseYear),
    write('Digite a nova classificação indicativa:'), nl,
    read_line_to_string(user_input, NewRating),
    write('Digite a nova duração estimada do filme (em minutos):'), nl,
    read_line_to_string(user_input, NewDuration),
    write('Digite a nova data prevista de estreia (formato DD/MM):'), nl,
    read_line_to_string(user_input, NewReleaseDate),
    assertz(upcoming_movie(NewTitle, NewDirector, NewReleaseDate, NewReleaseYear, NewRating, NewDuration)),
    write('Lançamento futuro editado com sucesso.'), nl.

% Predicado para gerenciar filmes
manage_movies :-
    write('Gerenciamento de Filmes:'), nl,
    write('1) Adicionar Filme (em cartaz)'), nl,
    write('2) Adicionar lançamento futuro'), nl,
    write('3) Editar Filme em cartaz'), nl,
    write('4) Editar Lançamento Futuro'), nl,
    write('5) Listar Filmes em cartaz'), nl,
    write('6) Listar Lançamentos Futuros'), nl,
    write('7) Remover Filme em cartaz'), nl,
    write('8) Remover Lançamento Futuro'), nl,
    write('9) Voltar'), nl,
    read_line_to_string(user_input, Option),
    (Option = "1" -> add_movie, manage_movies;
     Option = "2" -> add_upcoming_movie, manage_movies;
     Option = "3" -> edit_movie, manage_movies;
     Option = "4" -> edit_upcoming_movie, manage_movies;
     Option = "5" -> list_movies, manage_movies;
     Option = "6" -> list_upcoming_movies, manage_movies;
     Option = "7" -> remove_movie, manage_movies;
     Option = "8" -> remove_upcoming_movie, manage_movies;
     Option = "9" -> write('Voltando ao menu anterior.'), nl;
     write('Opção inválida. Tente novamente.'), nl, manage_movies).

% Função principal
main :-
    write('Bem-vindo, Funcionário!'), nl,
    repeat,
    write('Escolha uma opção:'), nl,
    write('1. Gerenciar Filmes'), nl,
    write('2. Gerenciar Sessões'), nl,
    write('3. Gerenciar Usuários'), nl,
    write('4. Gerar Relatórios'), nl,
    write('5. Sair'), nl,
    read_line_to_string(user_input, Option),
    (Option = "1" -> manage_movies;
     Option = "2" -> write('Gerenciar Sessões ainda não implementado.'), nl, fail;
     Option = "3" -> write('Gerenciar Usuários ainda não implementado.'), nl, fail;
     Option = "4" -> write('Gerar Relatórios ainda não implementado.'), nl, fail;
     Option = "5" -> write('Saindo...'), nl, !;
     write('Opção inválida. Tente novamente.'), nl, fail).
