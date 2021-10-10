:- assert(dbg_mode(1)).

set_dbg(X):- retractall(dbg_mode(_)), assert(dbg_mode(X)).

% forming db ======================

% Note: class-2 cities can only visit class-1 cities; class-1 cities can visit all cities.

clear:-
    tty_clear,
    retractall(places_to_1(_)),
    retractall(places_from_1(_, _)),
    retractall(places_to_2(_)),
    retractall(places_from_2(_, _)),
    retractall(queue(_)).

init:-
    assert(queue([])).

index:-
    row(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48),
    data_count(N), retract(data_count(N)),
    (
        N =:= 0 -> assert(places_to_1([A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48])) ;
        assert(places_from_1(A1, [A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48]))
    ),
    N_ is N + 1, 
    assert(data_count(N_)), !.
index:- 
    row(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), 
    data_count(N), retract(data_count(N)), 
    (
        N =:= 0 -> assert(places_to_2([A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21])) ;
        assert(places_from_2(A1, [A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]))
    ),
    N_ is N + 1,
    assert(data_count(N_)), !.

form:- clear, init, read_1, read_2.
read_1:- csv_read_file('class_1_cities.csv', DATA_1), assert(data_count(0)), parse(DATA_1).
read_2:- csv_read_file('class_2_cities.csv', DATA_2), assert(data_count(0)), parse(DATA_2).
parse([H|T]):- assert(H), index, retract(H), parse(T).
parse([]):- retractall(data_count(_)).

% utilities =======================

% maintained for each iteration of search
clear_queue:- retractall(queue(_)), assert(queue([])), !.
clear_visited:- retractall(visted(_)), assert(visted([])), !.
clear_p_info:- retractall(p_info(_, _, _)), !.

dist(X, Y, D):- places_from_1(X, PF_D), places_to_1(PT), nth1(I, PT, Y), nth1(I, PF_D, D_), ( D_ = '-' -> D = 0 ; D = D_ ), !.
dist(X, Y, D):- places_from_2(X, PF_D), places_to_2(PT), nth1(I, PT, Y), nth1(I, PF_D, D_), ( D_ = '-' -> D = 0 ; D = D_ ), !.

% binds LC: "List of Cities" reachable from X
cities_to(X, LC):- places_from_1(X, _), places_to_1(LC), !.
cities_to(X, LC):- places_from_2(X, _), places_to_2(LC), !.

% p_info denotes Path Information of the form p_info(City, Path From Start, Dist From Start).
add_p_infos(_, [], _, _).
add_p_infos(U, [H | T], PathYet, DistYet):- 
    push_back(PathYet, H, NewPath), 
    dist(U, H, D), NewDist is DistYet + D, 
    assert(p_info(H, NewPath, NewDist)), 
    add_p_infos(U, T, PathYet, DistYet).

% adds a list of cities to visited
add_visited([]).
add_visited([H | T]):- visted(V), push_back(V, H, NV), retractall(visted(_)), assert(visted(NV)), add_visited(T).
is_visited(X):- visted(V), member(X, V).

add_q([]).
add_q([H | T]):- q_push_back(H), add_q(T).
q_push_back(X):- queue(Q), retractall(queue(_)), push_back(Q, X, NQ), assert(queue(NQ)).
q_pop_front:- queue(Q), retractall(queue(_)), pop_front(Q, NQ, _), assert(queue(NQ)).
q_front(X):- queue([X | _]).
q_empty:- queue(Q), empty(Q).

empty([]).

push_back([], X, [X]).
push_back([H | T], X, [H | L]):- push_back(T, X, L), !.

pop_front([H | T], T, H).

% algo =========================

show_bfs(Start, End, Path, Dist):- 
    clear_queue, clear_visited, clear_p_info, 
    assert(p_info(Start, [Start], 0)), q_push_back(Start),
    bfs(End), p_info(End, Path, Dist).

bfs(_):- q_empty, !.
bfs(End):- not(q_empty), q_front(U), U = End, !.
bfs(End):- 
    not(q_empty), q_front(U), not(U = End), q_pop_front,
    dbg(['Q head: ', U]), dbg_q,
    p_info(U, PathYet, DistYet), 
    dbg([PathYet, DistYet]),
    cities_to(U, LV), 
    dbg(['Cities to: ', LV]),
    exclude(is_visited, LV, LV_),
    add_visited(LV_),
    dbg_vis, dbg(['Cities filtered: ', LV_]),
    add_q(LV_),
    add_p_infos(U, LV_, PathYet, DistYet), dbgnl, !,
    bfs(End).

% debug ==========================

dbg_q:- dbg_mode(M), M = 1, queue(Q), write('>>>> Queue: '), writeln(Q).
dbg_q:- dbg_mode(M), M = 0.

dbg_vis:- dbg_mode(M), M = 1, visted(V), write('>>>> Visited: '), writeln(V).
dbg_vis:- dbg_mode(M), M = 0.

dbg(X):- dbg_mode(M), M = 1, write('>>>> '), writeln(X).
dbg(_):- dbg_mode(M), M = 0.

dbgnl:- dbg_mode(M), M = 1, nl, writeln('****'), nl.
dbgnl:- dbg_mode(M), M = 0.
