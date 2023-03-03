use_module(library(csv)).

leer_partidos(Archivo) :-
    csv_read_file(Archivo, Partidos, [functor(partido), arity(4)]),
    maplist(assert, Partidos).


