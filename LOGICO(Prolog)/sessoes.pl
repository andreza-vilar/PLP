:- dynamic filme/6.  % Dinâmico para permitir adição e remoção de filmes
:- dynamic sessao/6. % Dinâmico para permitir adição e remoção de sessões

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

% Adicionar nova sessão
adicionar_sessao(Titulo, Horario, Sala, Data, Preco, Audio) :- 
    assertz(sessao(Titulo, Horario, Sala, Data, Preco, Audio)),
    writeln('Sessão adicionada com sucesso.').

% Editar sessão
editar_sessao(Titulo, NovoHorario, NovaSala, NovaData, NovoPreco, NovoAudio) :- 
    retract(sessao(Titulo, _, _, _, _, _)),
    assertz(sessao(Titulo, NovoHorario, NovaSala, NovaData, NovoPreco, NovoAudio)),
    writeln('Sessão editada com sucesso.').

% Remover sessão
remover_sessao(Titulo) :- 
    retract(sessao(Titulo, _, _, _, _, _)),
    writeln('Sessão removida com sucesso.').

% Exibir sessões
exibir_sessoes :- 
    findall(sessao(Titulo, Horario, Sala, Data, Preco, Audio), sessao(Titulo, Horario, Sala, Data, Preco, Audio), Sessoes),
    (   Sessoes \= [] -> 
        forall(member(Sessao, Sessoes), 
            (   Sessao = sessao(Titulo, Horario, Sala, Data, Preco, Audio),
                format('Sessão: ~w | Horário: ~w | Sala: ~w | Data: ~w | Preço: R$~2f | Áudio: ~w~n', [Titulo, Horario, Sala, Data, Preco, Audio])
            ))
    ;   writeln('Nenhuma sessão cadastrada.')
    ).

% Função para gerenciar sessões
gerenciar_sessoes :- 
    writeln('Gerenciamento de Sessões:'),
    writeln('1) Adicionar Sessão'),
    writeln('2) Editar Sessão'),
    writeln('3) Remover Sessão'),
    writeln('4) Listar Sessões'),
    writeln('5) Voltar'),
    read(Op),
    (   Op == 1 -> 
        writeln('Adicionar nova sessão: Informe o título do filme.'),
        read(Titulo),
        writeln('Informe o horário da sessão (por exemplo, 15:00).'),
        read(Horario),
        writeln('Informe a sala (por exemplo, Sala 3).'),
        read(Sala),
        writeln('Informe a data de exibição do filme no formato DD/MM (por exemplo, 25/08).'),
        read(Data),
        writeln('Informe o preço do ingresso (por exemplo, 20.50).'),
        read(Preco),
        writeln('Informe o tipo de áudio (Legendado ou Dublado).'),
        read(Audio),
        adicionar_sessao(Titulo, Horario, Sala, Data, Preco, Audio),
        gerenciar_sessoes
    ;   Op == 2 -> 
        writeln('Digite o título da sessão a ser editada:'),
        read(Titulo),
        writeln('Digite o novo horário da sessão:'),
        read(NovoHorario),
        writeln('Digite a nova sala:'),
        read(NovaSala),
        writeln('Digite a nova data:'),
        read(NovaData),
        writeln('Digite o novo preço:'),
        read(NovoPreco),
        writeln('Digite o novo tipo de áudio:'),
        read(NovoAudio),
        editar_sessao(Titulo, NovoHorario, NovaSala, NovaData, NovoPreco, NovoAudio),
        gerenciar_sessoes
    ;   Op == 3 -> 
        writeln('Digite o título da sessão a ser removida:'),
        read(Titulo),
        remover_sessao(Titulo),
        gerenciar_sessoes
    ;   Op == 4 -> 
        exibir_sessoes,
        gerenciar_sessoes
    ;   Op == 5 -> 
        writeln('Voltando ao menu anterior.')
    ;   writeln('Opção inválida. Tente novamente.'),
        gerenciar_sessoes
    ).

% Carregar filmes ao iniciar o sistema
:- carregar_filmes.
