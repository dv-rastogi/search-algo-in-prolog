% forming db
clear:-
    tty_clear,
    retractall(places_to(_)),
    retractall(places_from(_, _)),
    retractall(queue(_)),
    retractall(stack(_)).

init:-
    assert(queue([])),
    assert(stack([])).

index:- 
    row(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), 
    data_count(N), retract(data_count(N)), 
    (
        N =:= 0 -> assert(places_to([A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21])) ;
        assert(places_from(A1, [A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]))
    ),
    N_ is N + 1,
    assert(data_count(N_)).

form:- clear, init, csv_read_file('data.csv', DATA), assert(data_count(0)), form(DATA).
form([H|T]):- assert(H), index, retract(H), form(T).
form([]):- retractall(data_count(_)).

% utilities
dist(X, Y, D):- places_to(PT), nth1(I, PT, Y), !, places_from(X, PF_D), nth1(I, PF_D, D_), ( D_ = '-' -> D = 0 ; D = D_ ).

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