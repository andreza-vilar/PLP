:- dynamic filme/5.  % Dinâmico para permitir adição e remoção

% Carregar filmes do arquivo ao iniciar o sistema
carregar_filmes :- 
    (   exists_file('filmes.txt') ->  % Verifica se o arquivo existe
        open('filmes.txt', read, Stream),
        repeat,
        read(Stream, Filme),
        (   Filme == end_of_file -> ! ;
            assertz(Filme),
            fail
        ),
        close(Stream)
    ;   true  % Se o arquivo não existe, não faz nada
    ).

% Salvar filmes no arquivo
salvar_filmes :- 
    open('filmes.txt', write, Stream),
    forall(filme(ID, Titulo, Diretor, Ano, Classificacao, Duracao), 
           writeln(Stream, filme(ID, Titulo, Diretor, Ano, Classificacao, Duracao))),
    write(Stream, end_of_file),
    close(Stream).

% Adicionar filme
adicionar_filme(ID, Titulo, Diretor, Ano, Classificacao, Duracao) :- 
    assertz(filme(ID, Titulo, Diretor, Ano, Classificacao, Duracao)),
    salvar_filmes,
    writeln('Filme adicionado com sucesso.').

% Editar filme
editar_filme(ID, NovoTitulo, NovoDiretor, NovoAno, NovaClassificacao, NovaDuracao) :- 
    retract(filme(ID, _, _, _, _, _)),
    assertz(filme(ID, NovoTitulo, NovoDiretor, NovoAno, NovaClassificacao, NovaDuracao)),
    salvar_filmes,
    writeln('Filme editado com sucesso.').

% Remover filme
remover_filme(ID) :- 
    retract(filme(ID, _, _, _, _, _)),
    salvar_filmes,
    writeln('Filme removido com sucesso.').

% Exibir filmes
exibir_filmes :- 
    findall(filme(ID, Titulo, Diretor, Ano, Classificacao, Duracao), filme(ID, Titulo, Diretor, Ano, Classificacao, Duracao), Filmes),
    (   Filmes \= [] -> 
        forall(member(Filme, Filmes), 
            (   Filme = filme(ID, Titulo, Diretor, Ano, Classificacao, Duracao),
                format('Filme em cartaz: ~w | ~w | ~w | ~w | ~w min~n', [Titulo, Diretor, Ano, Classificacao, Duracao])
            ))
    ;   writeln('Nenhum filme cadastrado.')
    ).

% Função para gerenciar filmes
gerenciar_filmes :-
    writeln('Gerenciamento de Filmes:'),
    writeln('1) Adicionar Filme'),
    writeln('2) Editar Filme'),
    writeln('3) Remover Filme'),
    writeln('4) Listar Filmes'),
    writeln('5) Voltar'),
    read(Op),
    (   Op == 1 -> 
        writeln('Digite o ID do filme:'),
        read(ID),
        writeln('Digite o título do filme:'),
        read(Titulo),
        writeln('Digite o diretor do filme:'),
        read(Diretor),
        writeln('Digite o ano de lançamento:'),
        read(Ano),
        writeln('Digite a classificação indicativa:'),
        read(Classificacao),
        writeln('Digite a duração do filme (em minutos):'),
        read(Duracao),
        adicionar_filme(ID, Titulo, Diretor, Ano, Classificacao, Duracao),
        gerenciar_filmes
    ;   Op == 2 -> 
        writeln('Digite o ID do filme a ser editado:'),
        read(ID),
        writeln('Digite o novo título do filme:'),
        read(NovoTitulo),
        writeln('Digite o novo diretor do filme:'),
        read(NovoDiretor),
        writeln('Digite o novo ano de lançamento:'),
        read(NovoAno),
        writeln('Digite a nova classificação indicativa:'),
        read(NovaClassificacao),
        writeln('Digite a nova duração do filme (em minutos):'),
        read(NovaDuracao),
        editar_filme(ID, NovoTitulo, NovoDiretor, NovoAno, NovaClassificacao, NovaDuracao),
        gerenciar_filmes
    ;   Op == 3 -> 
        writeln('Digite o ID do filme a ser removido:'),
        read(ID),
        remover_filme(ID),
        gerenciar_filmes
    ;   Op == 4 -> 
        exibir_filmes,
        gerenciar_filmes
    ;   Op == 5 -> 
        writeln('Voltando ao menu anterior.')
    ;   writeln('Opção inválida. Tente novamente.'),
        gerenciar_filmes
    ).

% Carregar filmes ao iniciar o sistema
:- carregar_filmes.
