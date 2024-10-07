:- use_module(library(readutil)).
:- use_module(library(lists)).
:- dynamic user/2.

% Arquivo onde os itens da bomboniere são armazenados
file_name('concessionStand.txt').

% Código de acesso para o funcionário
employee_password('admin').

% Função para adicionar um item à bomboniere
add_item :-
    write("Digite o nome do item:"), nl,
    read_line_to_string(user_input, ItemName),
    write("Digite o preco do item (em reais):"), nl,
    read_line_to_string(user_input, Price),
    atom_concat(ItemName, ' | R$ ', Temp),
    atom_concat(Temp, Price, ItemEntry),
    file_name(FileName),
    open(FileName, append, Stream),
    write(Stream, ItemEntry), nl(Stream),
    close(Stream),
    write("Item adicionado com sucesso."), nl.

% Função para listar todos os itens da bomboniere
list_items :-
    file_name(FileName),
    open(FileName, read, Stream),
    read_lines(Stream, Items),
    close(Stream),
    ( Items = [] ->
        write("Nenhum item disponivel na bomboniere."), nl
    ; write("Itens da bomboniere:"), nl,
      maplist(display_item, Items)
    ).

% Evita a redefinição do write_item/1 em outros arquivos
display_item(Item) :-
    write(Item), nl.

% Função para editar um item na bomboniere
edit_item :-
    list_items,
    file_name(FileName),
    open(FileName, read, Stream),
    read_lines(Stream, Items),
    close(Stream),
    ( Items = [] ->
        true
    ; write("Digite o nome do item a ser editado:"), nl,
      read_line_to_string(user_input, OldName),
      write("Digite o novo nome do item (ou deixe em branco para manter o mesmo):"), nl,
      read_line_to_string(user_input, NewName),
      write("Digite o novo preço do item (ou deixe em branco para manter o mesmo):"), nl,
      read_line_to_string(user_input, NewPrice),
      maplist(update_item(OldName, NewName, NewPrice), Items, UpdatedItems),
      open(FileName, write, OutStream),
      maplist(write_item(OutStream), UpdatedItems),
      close(OutStream),
      write("Item atualizado com sucesso."), nl
    ).

% Função auxiliar para atualizar um item
update_item(OldName, NewName, NewPrice, Line, UpdatedLine) :-
    sub_string(Line, 0, _, _, OldName), !,
    ( NewName = "" -> sub_string(Line, 0, _, _, OldName), NewName = OldName ; true ),
    ( NewPrice = "" -> sub_string(Line, _, _, 0, OldPrice), UpdatedLine = Line ; atom_concat('R$ ', NewPrice, OldPrice) ),
    atom_concat(NewName, ' | ', Temp1),
    atom_concat(Temp1, OldPrice, UpdatedLine).
update_item(_, _, _, Line, Line).

% Função para remover um item da bomboniere (somente funcionário)
remove_item :-
    write("Digite o codigo de acesso do funcionario:"), nl,
    read_line_to_string(user_input, EnteredPassword),
    employee_password(Password),
    ( EnteredPassword = Password ->
        list_items,
        write("Digite o nome do item a ser removido:"), nl,
        read_line_to_string(user_input, ItemToRemove),
        file_name(FileName),
        open(FileName, read, InStream),
        read_lines(InStream, Items),
        close(InStream),
        exclude(has_substring(ItemToRemove), Items, UpdatedItems),
        open(FileName, write, OutStream),
        maplist(write_item(OutStream), UpdatedItems),
        close(OutStream),
        write("Item removido com sucesso."), nl
    ; write("Codigo de acesso incorreto. Acesso negado."), nl
    ).

% Função para comprar itens da bomboniere
buy_items :-
    list_items,
    write("Digite o nome do item que deseja comprar (ou 'voltar' para retornar ao menu principal):"), nl,
    read_line_to_string(user_input, ItemToBuy),
    ( ItemToBuy = "voltar" ->
        true
    ; write("Voce comprou: "), write(ItemToBuy), nl
    ).

% Função auxiliar para verificar substrings
has_substring(Sub, String) :- sub_string(String, _, _, _, Sub).

% Função auxiliar para ler o conteúdo do arquivo linha a linha
read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    ( Line = end_of_file ->
        Lines = []
    ; read_lines(Stream, RestLines),
      Lines = [Line|RestLines]
    ).

% Evita a redefinição do procedimento read_lines/2
write_item(Stream, Item) :-
    write(Stream, Item), nl(Stream).

% Menu de gerenciamento da bomboniere
manage_concession_stand :-
    write("1) Adicionar Item"), nl,
    write("2) Listar Itens"), nl,
    write("3) Editar Item"), nl,
    write("4) Remover Item"), nl,
    write("5) Voltar ao Menu Principal"), nl,
    read_line_to_string(user_input, Option),
    ( Option = "1" -> add_item, manage_concession_stand
    ; Option = "2" -> list_items, manage_concession_stand
    ; Option = "3" -> edit_item, manage_concession_stand
    ; Option = "4" -> remove_item, manage_concession_stand
    ; Option = "5" -> true
    ; write("Opção invalida. Tente novamente."), nl, manage_concession_stand
    ).
