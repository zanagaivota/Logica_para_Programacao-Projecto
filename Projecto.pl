% -------------------------------------------------------------
% Projecto Termometros
% -------------------------------------------------------------

% encontra o termometro que contem uma determinada posicao
encontra_termometro([[], _, _], _, []) :- !.
encontra_termometro([LT, _, _], Pos, Termometro) :-
    member(Termometro, LT),
    member(Pos, Termometro), !.
encontra_termometro([LT, _, _], Pos, []) :- 
    member(Termometro, LT),
    \+ member(Pos, Termometro).


% junta duas listas
junta_listas([], L, L).
junta_listas([C | L1], L2, [C | L]) :- junta_listas(L1, L2, L).


% cria uma nova lista com os elementos da lista dada ate ao elemento dado inclusive 
parte_ate_elemento(Elemento, Lista, Nova_lista) :- parte_ate_elemento(Elemento, Lista, Nova_lista, []).
parte_ate_elemento(Elemento, Lista, Nova_lista, Nova_lista) :- \+ member(Elemento, Lista), !.  
parte_ate_elemento(Elemento, [C|L], Nova_lista, Acc) :- 
    junta_listas([C], Acc, Acc1),
    sort(Acc1, OrdAcc1),
    parte_ate_elemento(Elemento, L, Nova_lista, OrdAcc1).


% -------------------------------------------------------------
% Predicado propaga
% -------------------------------------------------------------

propaga([LT, _, _], Pos, Posicoes) :-
    encontra_termometro([LT, _, _], Pos, Termometro),
    parte_ate_elemento(Pos, Termometro, Posicoes), !.


% -------------------------------------------------------------
% Predicado nao_altera_linhas_anteriores
% -------------------------------------------------------------

nao_altera_linhas_anteriores([], _, _) :- !.
nao_altera_linhas_anteriores([(PosL, _)|L], Linha, Ja_Preenchidas) :-
    PosL >= Linha, !,
    nao_altera_linhas_anteriores(L, Linha, Ja_Preenchidas).
nao_altera_linhas_anteriores([(PosL, PosC)|L], Linha, Ja_Preenchidas) :-
    PosL < Linha, !,
    Pos = (PosL,PosC),
    member(Pos,Ja_Preenchidas),
    nao_altera_linhas_anteriores(L, Linha, Ja_Preenchidas).


% verifica se o numero de elementos relativos a uma determinada coluna, numa lista, nao excede o total de elementos definidos para essa coluna 
soma_coluna_ok(Lista, Coluna, Coluna_Total) :- 
    soma_coluna_ok(Lista, Coluna, Coluna_Total, 0).
soma_coluna_ok([], _, Coluna_Total, Acc):- !,
    Acc =< Coluna_Total.
soma_coluna_ok([(_,PosC)|L], Coluna, Coluna_Total, Acc):-
    PosC =:= Coluna, !,
    Acc1 is Acc + 1,
    soma_coluna_ok(L, Coluna, Coluna_Total, Acc1).
soma_coluna_ok([(_,PosC)|L], Coluna, Coluna_Total, Acc):-
    PosC =\= Coluna, !,
    soma_coluna_ok(L, Coluna, Coluna_Total, Acc).


% -------------------------------------------------------------
% Predicado nao_altera_linhas_anteriores
% -------------------------------------------------------------

verifica_parcial([_, _, SC], Ja_Preenchidas, Dim, Poss) :-
    junta_listas(Ja_Preenchidas, Poss, Temp_Preenchidas),
    sort(Temp_Preenchidas, OrdTemp_Preenchidas),
    verifica_soma_colunas(SC, Dim, OrdTemp_Preenchidas).
verifica_soma_colunas(_,0,_) :- !.
verifica_soma_colunas(SC, Dim, OrdTemp_Preenchidas) :-
    Dim >= 1,
    Index is Dim - 1,
    nth0(Index, SC, Coluna_Total),
    soma_coluna_ok(OrdTemp_Preenchidas, Dim, Coluna_Total),
    Coluna is Dim - 1,
    verifica_soma_colunas(SC, Coluna, OrdTemp_Preenchidas).


% determina a linha de uma posicao
linha([(Linha,_)|_], Linha).


% gera uma combinacao de N elementos de uma lista
is_combinacao(N, Lista, Combinacao) :- is_combinacao(N, Lista, Combinacao, []).
is_combinacao(0, _, Combinacao, Combinacao) :- !.
is_combinacao(N, Lista, Combinacao, Acc) :-
    N > 0,
    member(Pos, Lista),
    junta_listas([Pos], Acc, Acc1),
    sort(Acc1, OrdAcc1),
    delete(Lista, Pos, Resto_Lista),
    N1 is N - 1,
    is_combinacao(N1, Resto_Lista, Combinacao, OrdAcc1).


% verifica se o numero de elementos relativos a uma determinada linha, numa lista, iguala o total definido para essa linha 
verifica_total_linha(Poss, Linha, Linha_Total):- verifica_total_linha(Poss, Linha, Linha_Total, 0).
verifica_total_linha([], _, Linha_Total, Linha_Total) :- !.
verifica_total_linha([(PosL,_)|L], Linha, Linha_Total, Acc):-
    PosL =:= Linha, !,
    Acc1 is Acc + 1,
    verifica_total_linha(L, Linha, Linha_Total, Acc1).
verifica_total_linha([(PosL,_)|L], Linha, Linha_Total, Acc):-
    PosL =\= Linha, !,
    verifica_total_linha(L, Linha, Linha_Total, Acc).


% propaga todas as combinacoes
gera_combinacoes_propagadas([LT, SL, SC], Combinacoes_linha, Combinacoes_linha_propagadas) :-
        gera_combinacoes_propagadas([LT, SL, SC], Combinacoes_linha, Combinacoes_linha_propagadas, []).
gera_combinacoes_propagadas(_, [], Combinacoes_linha_propagadas, Combinacoes_linha_propagadas) :- !.
gera_combinacoes_propagadas([LT, SL, SC], [C|L], Combinacoes_linha_propagadas, Acc) :-
    gera_combinacao_propagada([LT, SL, SC], C, Combinacao_propagada), !,
    junta_listas([Combinacao_propagada], Acc, Acc1),
    sort(Acc1, OrdAcc1),
    gera_combinacoes_propagadas([LT, SL, SC], L, Combinacoes_linha_propagadas, OrdAcc1).


% propaga uma combinacao
gera_combinacao_propagada([LT, SL, SC], Combinacao, Combinacao_propagada) :-
    gera_combinacao_propagada([LT, SL, SC], Combinacao, Combinacao_propagada, []).
gera_combinacao_propagada(_, [], Combinacao_propagada, Combinacao_propagada) :- !.
gera_combinacao_propagada([LT, SL, SC], [C|L], Combinacao_propagada, Acc) :-
    propaga([LT, SL, SC], C, Posicoes), !,
    junta_listas(Posicoes, Acc, Acc1),
    sort(Acc1, OrdAcc1),
    gera_combinacao_propagada([LT, SL, SC], L, Combinacao_propagada, OrdAcc1).


% valida uma combinacao propagada
combinacao_propagada_valida([LT, SL, SC], Combinacoes_linha_propagadas, Total, Dim, Linha, Ja_Preenchidas, Poss) :- 
    member(Poss, Combinacoes_linha_propagadas),
    verifica_total_linha(Poss, Linha, Total),
    nao_altera_linhas_anteriores(Poss, Linha, Ja_Preenchidas),
    verifica_parcial([LT, SL, SC], Ja_Preenchidas, Dim, Poss).


% -------------------------------------------------------------
% Predicado possibilidades_linha
% -------------------------------------------------------------

possibilidades_linha([LT, SL, SC], Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L):-
    linha(Posicoes_linha, Linha),
    length(SL, Dim),
    findall(Combinacao, is_combinacao(Total, Posicoes_linha, Combinacao), Combinacoes_linha),
    gera_combinacoes_propagadas([LT, SL, SC], Combinacoes_linha, Combinacoes_linha_propagadas),
    findall(Poss, combinacao_propagada_valida([LT, SL, SC], Combinacoes_linha_propagadas, Total, Dim, Linha, Ja_Preenchidas, Poss), Possibilidades_L).


% verifica se o numero de elementos relativos a uma determinada coluna, numa lista, iguala o total definido para essa coluna 
total_coluna_ok(Solucao, Coluna, Coluna_Total) :- 
    total_coluna_ok(Solucao, Coluna, Coluna_Total, 0).
total_coluna_ok([], _, Coluna_Total, Coluna_Total):- !.
total_coluna_ok([(_,PosC)|L], Coluna, Coluna_Total, Acc):-
    PosC =:= Coluna, !,
    Acc1 is Acc + 1,
    total_coluna_ok(L, Coluna, Coluna_Total, Acc1).
total_coluna_ok([(_,PosC)|L], Coluna, Coluna_Total, Acc):-
    PosC =\= Coluna, !,
    total_coluna_ok(L, Coluna, Coluna_Total, Acc).


% verifica se o total de elementos de cada coluna, numa lista, iguala o respectivo total definido
verifica_total_colunas(_,0,_) :- !.
verifica_total_colunas(SC, Dim, Solucao) :-
    Dim >= 1,
    Index is Dim - 1,
    nth0(Index, SC, Coluna_Total),
    total_coluna_ok(Solucao, Dim, Coluna_Total),
    Coluna is Dim - 1,
    verifica_total_colunas(SC, Coluna, Solucao).


% gera as posicoes possiveis para uma linha
gera_posicoes_linha(Linha, Dim, Posicoes_linha) :- gera_posicoes_linha(Linha, Dim, Posicoes_linha, []).
gera_posicoes_linha(_, 0, Posicoes_linha, Posicoes_linha) :- !.
gera_posicoes_linha(Linha, Dim, Posicoes_linha, Acc) :- 
    Pos = (Linha,Dim),
    Dim1 is Dim - 1,
    junta_listas([Pos], Acc, Acc1),
    gera_posicoes_linha(Linha, Dim1, Posicoes_linha, Acc1).


% -------------------------------------------------------------
% Predicado resolve
% -------------------------------------------------------------

resolve([LT, SL, SC], Solucao) :-
    length(SL, Dim),
    resolve([LT, SL, SC], Solucao, Dim, 1, []).
resolve([_, _, SC], Solucao, Dim, Linha, Solucao) :-
    Linha > Dim, !,
    verifica_total_colunas(SC, Dim, Solucao).
resolve([LT, SL, SC], Solucao, Dim, Linha, Ja_Preenchidas) :- 
    Linha =< Dim, !,
    Index is Linha - 1,
    nth0(Index, SL, Linha_Total),
    gera_posicoes_linha(Linha, Dim, Posicoes_linha),
    possibilidades_linha([LT, SL, SC], Posicoes_linha, Linha_Total, Ja_Preenchidas, Possibilidades_L),
    member(Solucao_propagada, Possibilidades_L),
    junta_listas(Solucao_propagada, Ja_Preenchidas, Ja_Preenchidas1),
    sort(Ja_Preenchidas1, OrdJa_Preenchidas1),
    Linha_seguinte is Linha + 1,
    resolve([LT, SL, SC], Solucao, Dim, Linha_seguinte, OrdJa_Preenchidas1).

