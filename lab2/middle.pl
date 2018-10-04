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

| ?- middle(a, L).
L = [_A,a,_B] ? ;
L = [_A,_B,a,_C,_D] ? ;
L = [_A,_B,_C,a,_D,_E,_F] ? ;
L = [_A,_B,_C,_D,a,_E,_F,_G,_H] ? ;
L = [_A,_B,_C,_D,_E,a,_F,_G,_H,_I|...] ?

Case 3:

middle(X, [X]).
middle(X, [First|Xs]) :-
  middle(X, Middle),
  append(Middle, [Last], Xs).

3.a)

| ?- middle(X, [a,b,c]).
X = b ? ;
; infinite

3.b)

| ?- middle(a, L).
L = [a] ? ;
L = [_A,a,_B] ? ;
L = [_A,_B,a,_C,_D] ? ;
L = [_A,_B,_C,a,_D,_E,_F] ?

Case 4:

middle(X, [First|Xs]) :-
  middle(X, Middle),
  append(Middle, [Last], Xs).
middle(X, [X]).

4.a)

| ?- middle(X, [a,b,c]).
! Resource error: insufficient memory

4.b)

| ?- middle(a, L).
! Resource error: insufficient memory

*/
