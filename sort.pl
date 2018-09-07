
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

del(X, [X|Tail], Tail).
del(X, [Y|Tail], [Y|Tail1]) :-
    del(X, Tail, Tail1).

ssort([], []).
ssort(L, [S|LS]) :-
  smallest(L, S),
  del(S, L, L1),
  ssort(L1, LS).
