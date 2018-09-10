% middle(X,Xs)
% X is the middle element in the list Xs

middle(X, [X]).
middle(X, [First|Xs]) :-
  write(Xs),nl,
  middle(X, Middle),
  app(Middle, [Last], Xs).

/*
Case 1():

Case 2():

Case 3():

Case 4():

*/


app([], Ys, Ys).
app([X|Xs], Ys, [X|Zs]) :-
    app(Xs, Ys, Zs).
