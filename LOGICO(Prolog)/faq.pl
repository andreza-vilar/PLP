:- use_module(library(readutil)).

% Função para exibir o FAQ completo
view_faq :-
    write("FAQ - Perguntas Frequentes"), nl,
    write("==========================="), nl,
    write("\nEscolha uma seção para mais informações:"), nl,
    write("1) Programação"), nl,
    write("2) Ingressos"), nl,
    write("3) Voltar ao menu principal"), nl,
    read_line_to_string(user_input, Section),
    ( Section = "1" -> programming_faq
    ; Section = "2" -> tickets_faq
    ; Section = "3" -> true
    ; write("Opção inválida. Tente novamente."), nl, view_faq
    ).

% Seção: Programação
programming_faq :-
    write("\nEscolha uma pergunta sobre Programação:"), nl,
    write("1) Como faço para consultar a programação?"), nl,
    write("2) Quando o filme possui classificação indicativa, como proceder?"), nl,
    write("3) Por que são exibidas mais sessões dubladas?"), nl,
    write("4) Por quanto tempo um filme permanece em exibição?"), nl,
    write("5) Voltar"), nl,
    write("Caso sua dúvida não tenha sido esclarecida, envie mensagem para o número 4002-8922"), nl,
    read_line_to_string(user_input, Option),
    ( Option = "1" -> answer_programming_faq(1), programming_faq
    ; Option = "2" -> answer_programming_faq(2), programming_faq
    ; Option = "3" -> answer_programming_faq(3), programming_faq
    ; Option = "4" -> answer_programming_faq(4), programming_faq
    ; Option = "5" -> view_faq
    ; write("Opção inválida. Tente novamente."), nl, programming_faq
    ).

% Seção: Ingressos
tickets_faq :-
    write("\nEscolha uma pergunta sobre Ingressos:"), nl,
    write("1) Como consulto o valor do ingresso?"), nl,
    write("2) Quais são as formas de pagamento?"), nl,
    write("3) Aniversariante tem entrada gratuita?"), nl,
    write("4) Quero fechar uma sala para meus convidados. Como faço?"), nl,
    write("5) Voltar"), nl,
    write("Caso sua dúvida não tenha sido esclarecida, envie mensagem para o número 4002-8922"), nl,
    read_line_to_string(user_input, Option),
    ( Option = "1" -> answer_tickets_faq(1), tickets_faq
    ; Option = "2" -> answer_tickets_faq(2), tickets_faq
    ; Option = "3" -> answer_tickets_faq(3), tickets_faq
    ; Option = "4" -> answer_tickets_faq(4), tickets_faq
    ; Option = "5" -> view_faq
    ; write("Opção inválida. Tente novamente."), nl, tickets_faq
    ).

% Respostas para as perguntas sobre Programação
answer_programming_faq(1) :-
    write("Para consultar a programação, acesse nosso site ou visite a bilheteria do cinema."), nl.
answer_programming_faq(2) :-
    write("Verifique a classificação indicativa antes de comprar o ingresso. Respeite as restrições de idade."), nl.
answer_programming_faq(3) :-
    write("O motivo de exibirmos em maior demanda filmes dublados é a grande procura e preferência da maioria do público por este tipo de versão frente à versão legendada."), nl.
answer_programming_faq(4) :-
    write("O que irá definir que um filme permaneça em exibição será a procura pelo mesmo e os acordos firmados com a Distribuidora do filme. Sendo assim, às vezes é necessário que um filme saia de exibição para a entrada de outro lançamento."), nl.
answer_programming_faq(_) :-
    write("Opção inválida."), nl.

% Respostas para as perguntas sobre Ingressos
answer_tickets_faq(1) :-
    write("O valor do ingresso pode ser consultado na bilheteria ou em nosso site."), nl.
answer_tickets_faq(2) :-
    write("Aceitamos cartões de crédito, débito e dinheiro."), nl.
answer_tickets_faq(3) :-
    write("Infelizmente não temos essa política."), nl.
answer_tickets_faq(4) :-
    write("Para fechar uma sala, entre em contato com nossa equipe pelo número 4002-8922."), nl.
answer_tickets_faq(_) :-
    write("Opção inválida."), nl.
