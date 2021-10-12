:- assert(dbg_mode(1)).

set_dbg(X):- retractall(dbg_mode(_)), assert(dbg_mode(X)).

% forming db ======================

% Note: class-2 cities can only visit class-1 cities; class-1 cities can visit all cities.

clear:-
    tty_clear,
    retractall(places_to_1(_)),
    retractall(places_from_1(_, _)),
    retractall(heuristic_from_1(_, _)),
    retractall(places_to_2(_)),
    retractall(places_from_2(_, _)),
    retractall(heuristic_from_2(_, _)),
    retractall(queue(_)),
    retractall(pqueue(_)).

init:-
    assert(queue([])),
    assert(pqueue([])),
    assert(stack([])).

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

index_h:-
    row(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48),
    assert(heuristic_from_1(A1, [A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48])), !.
index_h:- 
    row(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), 
    assert(heuristic_from_2(A1, [A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21])), !.

% driver function for formation of db
form:- 
    clear, init, 
    read_1, read_2, 
    read_heuristic_1, read_heuristic_2.

read_1:- csv_read_file('class_1_cities.csv', DATA_1), assert(data_count(0)), parse(DATA_1).
read_2:- csv_read_file('class_2_cities.csv', DATA_2), assert(data_count(0)), parse(DATA_2).

read_heuristic_1:- csv_read_file('heuristic_1_cities.csv', [_ | DATA_1]), parse_h(DATA_1).
read_heuristic_2:- csv_read_file('heuristic_2_cities.csv', [_ | DATA_2]), parse_h(DATA_2).

parse([]):- retractall(data_count(_)).
parse([H | T]):- assert(H), index, retractall(H), parse(T).

parse_h([]).
parse_h([H | T]):- assert(H), index_h, retractall(H), parse_h(T).

% utilities =======================

% maintained for each iteration of search
clear_queue:- retractall(queue(_)), assert(queue([])).
clear_pqueue:- retractall(pqueue(_)), assert(pqueue([])).
clear_stack:- retractall(stack(_)), assert(stack([])).
clear_visited:- retractall(visted(_)), assert(visted([])).
clear_p_info:- retractall(p_info(_, _, _)).

dist(X, Y, D):- places_from_1(X, PF_D), places_to_1(PT), nth1(I, PT, Y), nth1(I, PF_D, D_), ( D_ = '-' -> D = 0 ; D = D_ ), !.
dist(X, Y, D):- places_from_2(X, PF_D), places_to_2(PT), nth1(I, PT, Y), nth1(I, PF_D, D_), ( D_ = '-' -> D = 0 ; D = D_ ), !.

heuristic(X, Y, H):- heuristic_from_1(X, PF_H), places_to_1(PT), nth1(I, PT, Y), nth1(I, PF_H, H), !.
heuristic(X, Y, H):- heuristic_from_2(X, PF_H), places_to_2(PT), nth1(I, PT, Y), nth1(I, PF_H, H), !.
heuristic(_, _, 999999999). % for cities that are not reachable, assign infinite heuristic

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

add_pq([]).
add_pq([H | T]):- pq_push_back(H), add_pq(T).
pq_push_back(X):- pqueue(Q), retractall(pqueue(_)), push_back(Q, X, NQ), assert(pqueue(NQ)).
pq_pop_front(G):- pqueue(Q), retractall(pqueue(_)), find_min(Q, M, G), remove_first(Q, M, NQ), assert(pqueue(NQ)).
pq_front(X, G):- pqueue(Q), find_min(Q, X, G).
pq_empty:- pqueue(Q), empty(Q).

add_s([]).
add_s([H | T]):- s_push_back(H), add_s(T).
s_push_back(X):- stack(S), retractall(stack(_)), push_back(S, X, NS), assert(stack(NS)).
s_pop_back:- stack(S), retractall(stack(_)), pop_back(S, NS), assert(stack(NS)).
s_back(X):- stack(S), last(S, X).
s_empty:- stack(S), empty(S).

empty([]).

push_back([], X, [X]).
push_back([H | T], X, [H | L]):- push_back(T, X, L).

pop_back([], []).
pop_back([_], []).
pop_back([H | T], [H | L]):- pop_back(T, L).

pop_front([H | T], T, H).

% finds last element of list
last([X], X).
last([_ | T], X):- last(T, X).

% removes the first appearance of X in the List
remove_first([], _, []).
remove_first([H | T], X, List):- H = X, List = T, !.
remove_first([H | T], X, [H | List]):- remove_first(T, X, List), !.

% finds min of list
find_min([X], X, _).
find_min([H | T], M, G):- find_min(T, M_, G), heuristic(H, G, Heu), heuristic(M_, G, Heu_), ( Heu < Heu_ -> M = H ; M = M_ ).

% algo =========================

% Depth First Search
show_dfs(Start, End):- 
    clear_visited, dfs(Start, [Start], 0, End), !.

dfs(U, PathU, DistU, End):- 
    U = End, 
    write('Path: '), writeln(PathU),
    write('Dist: '), writeln(DistU), !, fail.
dfs(U, _, _, End):- 
    not(U = End), is_visited(U), !.
dfs(U, PathU, DistU, End):-
    not(U = End),
    not(is_visited(U)),
    dbg(['Cur Node: ', U]),
    dbg(['Cur Path: ', PathU]),
    dbg(['Cur Dist: ', DistU]),
    add_visited([U]),
    dbg_vis,
    cities_to(U, LV),
    dbg(['Cities to: ', LV]),
    exclude(is_visited, LV, LV_),
    dbg(['Cities filtered: ', LV_]),
    dbgnl, !,
    perform_dfs(U, PathU, DistU, LV_, End).

perform_dfs(_, _, _, [], _). 
perform_dfs(U, CurPath, CurDist, [H | T], End):- 
    push_back(CurPath, H, NewPath),
    dist(U, H, D), NewDist is CurDist + D, 
    dfs(H, NewPath, NewDist, End),
    perform_dfs(U, CurPath, CurDist, T, End).

% Greedy Best First Search
show_gbs(Start, End, Path, Dist):- 
    clear_pqueue, clear_visited, clear_p_info,
    assert(p_info(Start, [Start], 0)), 
    pq_push_back(Start), add_visited([Start]),
    gbs(End), p_info(End, Path, Dist).

gbs(_):- pq_empty, !.
gbs(End):- not(pq_empty), pq_front(U, End), U = End, !.
gbs(End):- 
    not(pq_empty), pq_front(U, End), not(U = End), pq_pop_front(End),
    dbg(['PQ head: ', U]), dbg_pq,
    p_info(U, PathYet, DistYet),
    cities_to(U, LV),
    dbg(['Cities to: ', LV]),
    exclude(is_visited, LV, LV_),
    add_visited(LV_),
    dbg_vis, dbg(['Cities filtered: ', LV_]),
    add_pq(LV_),
    add_p_infos(U, LV_, PathYet, DistYet), dbgnl, !,
    gbs(End).

% Breadth First Search
show_bfs(Start, End, Path, Dist):- 
    clear_queue, clear_visited, clear_p_info, 
    assert(p_info(Start, [Start], 0)), 
    q_push_back(Start), add_visited([Start]),
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

dbg_s:- dbg_mode(M), M = 1, stack(S), write('>>>> Stack: '), writeln(S), !.
dbg_s:- dbg_mode(M), M = 0.

dbg_pq:- dbg_mode(M), M = 1, pqueue(Q), write('>>>> PQueue: '), writeln(Q), !.
dbg_pq:- dbg_mode(M), M = 0.

dbg_q:- dbg_mode(M), M = 1, queue(Q), write('>>>> Queue: '), writeln(Q), !.
dbg_q:- dbg_mode(M), M = 0.

dbg_vis:- dbg_mode(M), M = 1, visted(V), write('>>>> Visited: '), writeln(V), !.
dbg_vis:- dbg_mode(M), M = 0.

dbg(X):- dbg_mode(M), M = 1, write('>>>> '), writeln(X), !.
dbg(_):- dbg_mode(M), M = 0.

dbgnl:- dbg_mode(M), M = 1, nl, writeln('****'), nl, !.
dbgnl:- dbg_mode(M), M = 0.
