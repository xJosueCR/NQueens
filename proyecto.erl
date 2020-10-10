-module(proyecto).
-export([poblacion/1, mezclar/1, poblacion_aux/1, f1/3, sum/2, aptitud/3]).

poblacion(N) when is_integer(N) -> poblacion(N, N * 4, []).
poblacion(_, 0, R) -> R;
poblacion(N, C, R) -> poblacion(N, C-1, [poblacion_aux(N)|R]).

poblacion_aux(N) -> mezclar(poblacion_aux(N, [])).
poblacion_aux(0, R) -> R;
poblacion_aux(N, R) -> poblacion_aux(N-1, [N|R]).


mezclar(L) when is_list(L) -> [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- L])].

%% cruces(X,N) -> lists:flatten(lists:duplicate(N,X)).


%% obtener un elemento en una posición lists:nth(Pos, Lista)
%% Las siguientes 3 funciones son encargadas de realizar la
f1(L, N, I) -> f1(L, N, I, 1, 0).
f1(_, 0, _, _, R) -> R;
f1(L, N, I, J, R) ->  case (I /= J) and (abs((I+1)-(J+1)) == abs(lists:nth(I, L) - lists:nth(J, L)))
                           of true ->  f1(L, N-1, I, J+1, R+1);
                           false -> f1(L, N-1, I, J+1, R) end.

sum(L, N) -> sum(L, 0, N, 0).
sum(_, N, N, R) -> R;
sum(L, I, N, R) -> sum(L, I+1, N, f1(L, N, I+1) + R).

%% L = Lista de individuos, N = reinas, C = Cantidad de población, R = lista con aptitudes de los individous
aptitud(L, N, C) when is_integer(N), N > 3 -> lists:reverse(aptitud(L, N, C, [])).
aptitud([], _, 0, R) -> R;
aptitud([H|T], N, C, R) -> aptitud(T, N, C-1, [sum(H,N)|R] ).