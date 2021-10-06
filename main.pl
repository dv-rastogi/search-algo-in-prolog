% forming db
clear:-
    tty_clear,
    retractall(places_to_1(_)),
    retractall(places_from_1(_, _)),
    retractall(places_to_2(_)),
    retractall(places_from_2(_, _)),
    retractall(queue(_)),
    retractall(stack(_)).

init:-
    assert(queue([])),
    assert(stack([])).

index:- 
    row(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), 
    data_count(N), retract(data_count(N)), 
    (
        N =:= 0 -> assert(places_to_2([A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21])) ;
        assert(places_from_2(A1, [A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]))
    ),
    N_ is N + 1,
    assert(data_count(N_)), !.
index:-
    row(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48),
    data_count(N), retract(data_count(N)),
    (
        N =:= 0 -> assert(places_to_1([A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48])) ;
        assert(places_from_1(A1, [A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48]))
    ),
    N_ is N + 1, 
    assert(data_count(N_)), !.

form:- clear, init, read_1, read_2.
read_1:- csv_read_file('class_1_cities.csv', DATA_1), assert(data_count(0)), parse(DATA_1).
read_2:- csv_read_file('class_2_cities.csv', DATA_2), assert(data_count(0)), parse(DATA_2).
parse([H|T]):- assert(H), index, retract(H), parse(T).
parse([]):- retractall(data_count(_)).

% utilities
dist(X, Y, D):- places_to_1(PT), nth1(I, PT, Y), !, places_from_1(X, PF_D), nth1(I, PF_D, D_), ( D_ = '-' -> D = 0 ; D = D_ ), !.
dist(X, Y, D):- places_to_2(PT), nth1(I, PT, Y), !, places_from_2(X, PF_D), nth1(I, PF_D, D_), ( D_ = '-' -> D = 0 ; D = D_ ), !.

% list utilities
clear_stack:- retractall(stack(_)), assert(stack([])), !.

clear_queue:- retractall(queue(_)), assert(queue([])), !.

empty([]).

push_front(X, L, [X | L]).

push_back(X, [], [X]).
push_back(X, [H | T], [H | L]):- push_back(X, T, L), !.

pop_front([H | T], T, H).

pop_back([X], [], X).
pop_back([H | T], [H | L], X):- pop_back(T, L, X), !.