use_module(library(csv)).

%% ejemplo de consulta: 
%% leer_partidos("./resultados.txt").
leer_partidos(Archivo) :-
    csv_read_file(Archivo, Partidos, [functor(partido), arity(4)]),
    maplist(assert, Partidos).

%% luego de eso se puede consultar en la consola:
%% partido(A,B,C,D).
%% para obtener los partidos.
