-module(proyecto).
-export([poblacion/1, mezclar/1, poblacion_aux/1, f1/3, suma_colisiones/2, aptitud/3]).

%% N = Reinas
%% Dominio: Un número natural
%% Codominio: Una lista de individuos con N genes cada uno
poblacion(N) when is_integer(N) -> poblacion(N, N * 4, []).
poblacion(_, 0, R) -> R;
poblacion(N, C, R) -> poblacion(N, C-1, [poblacion_aux(N)|R]).

%% Dominio: Un número natural
%% Codominio: Una lista ordenada de manera aleatoria, con valores entre 1,N
poblacion_aux(N) -> mezclar(lists:seq(1, N)).

%% Dominio: Una lista
%% Codominio: La lista ordenada de manera aleatoria
mezclar(L) when is_list(L) -> [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- L])].

%% cruces(X,N) -> lists:flatten(lists:duplicate(N,X)).


%% obtener un elemento en una posición lists:nth(Pos, Lista)

%% Las siguientes 3 funciones son encargadas de realizar la función fitness sobre la población 

%% Dominio: Una lista L con N genes, y un I que indica el gen que se evalua
%% Codominio: Una número entero con las colisiones del I con los demás genes de L
f1(L, N, I) -> f1(L, N, I, 1, 0).
f1(_, 0, _, _, R) -> R;
f1(L, N, I, J, R) ->  case (I /= J) and (abs((I+1)-(J+1)) == abs(lists:nth(I, L) - lists:nth(J, L)))
                           of true ->  f1(L, N-1, I, J+1, R+1);
                           false -> f1(L, N-1, I, J+1, R) end.

%% L = Lista con Población, N = Reinas 
%% Dominio: Una lista con la Población del algoritmo, y un número natural con el número de reinas
%% Codominio: Suma del total de colisiones de los N genes del individuo
suma_colisiones(L, N) -> suma_colisiones(L, 0, N, 0).
suma_colisiones(_, N, N, R) -> R;
suma_colisiones(L, I, N, R) -> suma_colisiones(L, I+1, N, f1(L, N, I+1) + R).

%% L = Lista de individuos, N = reinas, C = Cantidad de población, R = lista con aptitudes de los individous
%% Dominio: Una lista L con la población, un número entero con el número de reinas, y un C = cantidad de población
%% Codominio: Una Lista con las colisiones de cada individuo
aptitud(L, N, C) when is_integer(N), N > 3 -> lists:reverse(aptitud(L, N, C, [])).
aptitud([], _, 0, R) -> R;
aptitud([H|T], N, C, R) -> aptitud(T, N, C-1, [suma_colisiones(H,N)|R] ).

%% Como testear aptitud 
%% proyecto:aptitud([[3, 4, 1, 2], [3, 2, 4, 1], [4, 3, 2, 1], [4, 1, 2, 3], 
%%                   [1, 3, 4, 2], [1, 4, 3, 2], [2, 3, 1, 4], [3, 2, 1, 4],
%%                   [2, 4, 1, 3], [4, 3, 1, 2], [1, 2, 4, 3], [1, 3, 4, 2], 
%%                   [2, 3, 1, 4], [4, 1, 3, 2], [2, 1, 3, 4], [4, 2, 3, 1]],
%%                   4,16). 
%% Parámetros:
%% P1: Lista de población, donde cada sublista son individuos de dicha población y los valores sus genes.
%% P2: Reinas
%% P3: Cantidad de población total
%% Resultado esperado:
%% Una Lista(L) de tamaño = Población total, donde cada elemento de L indica que tan apto es un individuo, 
%% entre más pequeño el elemento, mejor es el individuo(sublista) de la población(P1)
%% Los valores de L son calculados en función de un target
%% El target para este ejercicio es que las reinas no se ataquen, por lo cada individuo calcula cuantas colisiones 
%% provocan sus genes(elementos de la lista)