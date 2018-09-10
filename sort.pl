
ascending([]).
ascending([_]).
ascending([X,Y|T]) :-
  (Y >= X),
  ascending([Y|T]).

smallest([X], S) :- S = X.
smallest([X,Y|T], S) :-
  (
    (Y >= X), smallest([X|T], S);
    (Y < X), smallest([Y|T], S)
  ).

del(X, [X|T], T).
del(X, [Y|T], [Y|R]) :-
    del(X, T, R).cd

app([], Ys, Ys).
app([X|Xs], Ys, [X|Zs]) :-
    app(Xs, Ys, Zs).

ssort([], []).
ssort(L, [S|LS]) :-
  smallest(L, S),
  del(S, L, L1),
  ssort(L1, LS).

partition(_, [], [], []).
partition(X, [H|T], S, [H|G]) :-
  (H >= X), partition(X, T, S, G).
partition(X, [H|T], [H|S], G) :-
  (H <  X), partition(X, T, S, G).

qsort([], []).
qsort([X|T], L1) :-
  partition(X, T, S, G),
  qsort(S, S1),
  qsort(G, G1),
  append(S1, [X|G1], L1).
