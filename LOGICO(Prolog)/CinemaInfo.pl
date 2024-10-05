% Informações do cinema

% Função para obter o endereço do cinema
get_address("R. Irineu Joffily, 23-65 - Centro, Campina Grande. Cine Capitolio").

% Função para obter o contato do cinema
get_contact("4002-8922").

% Função para obter o horário de funcionamento
get_operating_hours("10h00 - 00h00").

% Função para exibir todas as informações do cinema
view_cinema_info :-
    write("Informações do Cinema:"), nl,
    get_address(Address),
    write("Endereço: "), write(Address), nl,
    get_contact(Contact),
    write("Contato: "), write(Contact), nl,
    get_operating_hours(Hours),
    write("Horário de Funcionamento: "), write(Hours), nl.
