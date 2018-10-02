% middle(X,Xs)
% X is the middle element in the list Xs


middle(X, [First|Xs]) :-
  write('recursive'), nl,
  append(Middle, [Last], Xs),
  middle(X, Middle).
middle(X, [X]) :- write('Very nice halting condition'), nl.

/*
Case 1

middle(X, [X]).
middle(X, [First|Xs]) :-
  append(Middle, [Last], Xs),
  middle(X, Middle).

1.a)

| ?- middle(X, [a,b,c]).
X = b ? ;
no

1.b)

| ?- middle(a, L).
L = [a] ? ;
L = [_A,a,_B] ? ;
L = [_A,_B,a,_C,_D] ? ;
L = [_A,_B,_C,a,_D,_E,_F] ? ;
L = [_A,_B,_C,_D,a,_E,_F,_G,_H] ?

Case 2

middle(X, [First|Xs]) :-
  append(Middle, [Last], Xs),
  middle(X, Middle).
middle(X, [X]).

2.a)

| ?- middle(X, [a,b,c]).
X = b ? ;
no

2.b)



Case 3():

Case 4():

*/
