:- use_module(library(readutil)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- dynamic review/2.

% Função para deixar uma avaliação
leave_review :-
    write('Avalie a infraestrutura da sala (nota de 0 a 10):\n'),
    read(InfraRating),
    write('Avalie a comida (nota de 0 a 10):\n'),
    read(FoodRating),
    assertz(review(InfraRating, FoodRating)),
    save_reviews_to_file,
    write('Avaliação registrada com sucesso.\n').

save_reviews_to_file :-
    open('reviews.txt', append, Stream),
    forall(review(Infra, Food),
           format(Stream, 'Infraestrutura: ~w | Comida: ~w\n', [Infra, Food])),
    close(Stream).

% Função para calcular a media das avaliações
calculate_average_rating :-
    findall(Infra, review(Infra, _), InfraRatings),
    findall(Food, review(_, Food), FoodRatings),
    average(InfraRatings, InfraAvg),
    average(FoodRatings, FoodAvg),
    format('Média de Infraestrutura: ~2f\n', [InfraAvg]),
    format('Média de Comida: ~2f\n', [FoodAvg]).

% Função auxiliar para calcular a media de uma lista de números
average(List, Avg) :-
    sum_list(List, Sum),
    length(List, Len),
    ( Len > 0 -> Avg is Sum / Len ; Avg is 0 ).

% Função auxiliar para parsear a avaliação
parse_review_line(Line, Infra, Food) :-
    split_string(Line, "|", "", [InfraStr, FoodStr]),
    split_string(InfraStr, ":", "", [_, InfraValue]),
    split_string(FoodStr, ":", "", [_, FoodValue]),
    number_string(Infra, InfraValue),
    number_string(Food, FoodValue).