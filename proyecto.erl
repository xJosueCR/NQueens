-module(proyecto).
-define(MUTATION, 0.05).
-define(MAX_CRUCES, 1000).
-export([poblacion/1, aptitud/3, cruces/4, geneticosNReinas/1, min_pos/3, pos_elemento/2, ejecutar/1]).

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
%% El target para este ejercicio es que las reinas no se ataquen, por lo que cada individuo calcula cuantas colisiones 
%% provocan sus genes(elementos de la lista)

%% Dominio: Lista de población, Individuo elite, N reinas, Cantidad de población
%% Codominio: una lista con la nueva generación de individuos
cruces(L, E, N, C) -> cruce(L, N, C-1, [E]).
cruce(_, _, 0, R) -> lists:reverse(R);
cruce(L, N, C, R) ->  cruce(L, N, C-1, [cruce_aux(L, N, N*4)|R]).

%% Dominio: Una lista de población, Reinas y cantidad de población
%% Codominio: Una lista con genes de un nuevo individuo que forma parte de la nueva población
cruce_aux(L, N, C) -> X1 = rand:uniform(C),
                      X2 = rand:uniform(C), 
                      padres(L, N, X1, X2).

%% Dominio: Lista de población, Número de reinas, y dos padres(individuos) aleatorios
%% Codominio: Una lista con genes de un nuevo individuo que forma parte de la nueva población
padres(L, N, X1, X2) -> P1 = lists:nth(X1, L), 
                        P2 = lists:nth(X2, L),
                        reproduccion(P1, P2, N).
%% Dominio: Dos padres aleatorio y número de reinas
%% Codominio: Una lista con genes de un nuevo individuo que forma parte de la nueva población evaluando la posiblidad de mutación
reproduccion(P1, P2, N) -> L = split(P1, trunc(N/2)), 
                           Hijo = reproducirse(P2, lists:reverse(L)),
                           Rand = rand:uniform(),
                           case Rand =< ?MUTATION of
                           true  -> mutacion(Hijo, N);
                           false ->  Hijo end.

%% Dominio: Una lista de genes de un padre y otra lista con genes del segundo padre
%% Codominio: Un nuevo individuo con los genes de ambos padres
reproducirse([], R) -> lists:reverse(R);
reproducirse([H|T], R) -> case lists:member(H, R) of 
                          true -> reproducirse(T, R); 
                          false -> reproducirse(T, [H|R]) end.

%% Dominio: Una lista L  y un número N que indica posición a split
%% Codominio: Una sublista desde  hasta N
split(L, N) -> split(L, N, 0, [], []).
split([], _, _, L1, _)-> lists:reverse(L1);
split([H|T], N, C, L1, []) when N > C -> split(T, N, C+1, [H|L1], []);
split([H|T], N, C, L1, L2)-> split(T,N, C+1, L1, [H|L2]).


%% To test Crossover
%% Parámetros: Población, Elemento Alfa, Reinas, Cantidad de población
%% Esperado: Una lista de tamaño igual a la cantidad de población con el cruce entre individuos
%% proyecto:cruces([[2, 1, 3, 4], [4, 3, 2, 1], [3, 1, 2, 4], [1, 2, 4, 3], [1, 2, 4, 3], [4, 2, 3, 1], [2, 3, 1, 4], [3, 4, 1, 2], [1, 4, 3, 2], [3, 4, 2, 1], [3, 2, 4, 1], [4, 2, 1, 3], [2, 1, 3, 4], [3, 2, 4, 1], [2, 1, 4, 3], [4, 2, 1, 3]], [4,2,3,1], 4, 16).
%% Mutaciones

%% L = Población, N = Reinas, M = Mutaciones, C = Cantidad de población
%% Dominio: un individuo seleccionado, número de reinas
%% Codominio: Un inviduo que presenta mutación en sus genes
mutacion(Picked, N) ->  Gen1 = rand:uniform(N),
                        Gen2 = rand:uniform(N),
                        mutar(lists:nth(Gen1, Picked), lists:nth(Gen2, Picked), Picked).
 
%% Dominio: Genes seleccionados a mutar y lista actual de genes
%% Codominio: Lista con los genes mutados
mutar(Gen1, Gen1, List) -> List;
mutar(Gen1, Gen2, List) -> {P1, P2} = {min(Gen1,Gen2), max(Gen1,Gen2)},
                           {L1, [Elem1 | T1]} = lists:split(P1-1, List),
                           {L2, [Elem2 | L3]} = lists:split(P2-P1-1, T1), 
                           lists:append([L1, [Elem2], L2, [Elem1], L3]).


%% mostrar solución 
%% Dominio: Una lista de genes, número de reinas, cantidad de población 
%% Codominio: Impresión en pantalla del individuo que brinda solución
mostrarSolucion(_, _, 0, _, R) -> io:fwrite("Solución:~n", []), lists:reverse(R);
mostrarSolucion(L, N, S, C, R) -> Pos = pos_elemento(S, L),
                                  QL = generate(Pos, N),
                                  mostrarSolucion(L, N, S-1, C, [QL|R]).

%% Dominio: Una posición de la lista de genes, Número de reinas
%% Codominio: Una lista que representa una fila del tablero de ajedrez
generate(Pos, N) -> generate(Pos, N, []).
generate(_, 0, R)-> R;
generate(Pos, N, R) -> case Pos == N of
                       true -> generate(Pos, N-1, ['♕'|R]);
                       false -> generate(Pos, N-1, ['■'|R]) end.
%% función principal
%% Dominio: Un n natural mayor que 3
%% Codominio: Una solución al problema de NReinas
geneticosNReinas(N) when N > 3 -> 
                    Poblacion = poblacion(N),
                    calcular(?MAX_CRUCES, Poblacion, N, N*4).
%%Dominio: Elemento con mejor aptitud, Cantidad de cruces efectuados
%% Codominio: Impresión por pantalla de elite actual junto con el número de cruce
eliteActual(Elite, Cruces) ->   io:fwrite("Cruce: ~p~n", [?MAX_CRUCES -Cruces]),
                                io:fwrite("Élite actual: ", []),
                                erlang:display(Elite),
                                io:fwrite("~n", []).
%% Dominio: cantidad de cruces, poblacióm, Reinas y cantidad de población
%% Codominio: cantidad de cruces necesarios para otorgar una respuesta en dado de que se encuentre
calcular(0, _, _, _) -> [];
calcular(Cruces, Poblacion, N, C) -> Aptitudes = aptitud(Poblacion, N, C),
                                     Min = trunc(lists:min(Aptitudes)),      
                                     Pos = min_pos(Aptitudes, Min, 1),
                                     Elite = lists:nth(Pos, Poblacion),
                                     case Min == 0 of
                                     true -> mostrarSolucion(Elite, N, N, Cruces, []);
                                     false -> eliteActual(Elite, Cruces),
                                              N_Poblacion = cruces(Poblacion, Elite, N, C),
                                              calcular(Cruces-1, N_Poblacion, N, C) end.
%% Dominio: Lista, Cabeza, Posición
%% Codominio: Posición actual de un elemento
min_pos([H|_], H, R)-> R;
min_pos([_|T], N, R)->min_pos(T, N, R+1).

%% Dominio: Elemento, Lista
%% Codominio: posición de un elemento
pos_elemento(N, L) -> pos_elemento(N, 1, L).
pos_elemento(H, N, [H|_]) -> N;
pos_elemento(N, P, [_|R]) -> pos_elemento(N, P+1, R).
 
%% PRUEBAS PARA RESULTADOS
ejecutar(N) -> ejecutar(N, 1).
ejecutar(_, 0) -> 0; 
ejecutar(N, C) -> RP = geneticosNReinas(N), case RP == [] of true-> false; false-> ejecutar(N, C-1) end. 