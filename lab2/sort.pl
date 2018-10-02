/*
Checks whether a list is in ascending order
*/
ascending([]).
ascending([_]).
ascending([X,Y|T]) :-
  (Y >= X),
  ascending([Y|T]).

/*
Returns the smallest element of a list
*/
smallest([X], X).
smallest([X,Y|T], S) :-
  (
    (Y >= X), smallest([X|T], S);
    (Y < X), smallest([Y|T], S)
  ).

/*
Deletes an element from a list
*/
del(X, [X|T], T).
del(X, [Y|T], [Y|R]) :-
    del(X, T, R).

/*
Appends an element to the end of a list
*/
app([], Ys, Ys).
app([X|Xs], Ys, [X|Zs]) :-
    app(Xs, Ys, Zs).

/*
Selection sort using the predicates smallest and del
*/
ssort([], []).
ssort(L, [S|LS]) :-
  smallest(L, S),
  del(S, L, L1),
  ssort(L1, LS).

/*
Partitions a list into into one list
containing all the elements bigger than the
pivot element and one list with all the smaller elements.
*/
partition(_, [], [], []).
partition(X, [H|T], S, [H|G]) :-
  (H >= X), partition(X, T, S, G).
partition(X, [H|T], [H|S], G) :-
  (H <  X), partition(X, T, S, G).

/*
Quicksort using the partition predicate
*/
qsort([], []).
qsort([X|T], L1) :-
  partition(X, T, S, G),
  qsort(S, S1),
  qsort(G, G1),
  append(S1, [X|G1], L1).
