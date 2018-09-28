/* Facts */
path(a, b).
path(a, c).
path(b, c).
path(c, d).
path(c, e).
path(d, h).
path(d, f).
path(e, f).
path(e, g).
path(f, g).

/*
Base case
There's a path between X and Y
*/
path(X, Y, [X, Y]) :-
  path(X, Y).

/*
There is no path between X and Y.
Check if there is a path
between X's neighbours and Y.
*/
path(X, Y, [X|T]) :-
    path(X, N),
    path(N, Y, T).

/*
Call path/3 and print the results
and returns the length.
*/
npath(X, Y, L) :-
    path(X, Y, Z),
    length(Z, L),
    write('Path: '),
    write(Z),
    nl.

/*
TEST CASES
Using the graph provided in the labpm.pdf

| ?- path(b, g, L).
L = [b,c,d,f,g] ? ;
L = [b,c,e,g] ? ;
L = [b,c,e,f,g] ? ;
no

| ?- path(g, b, L).
no

| ?- path(a, b, L).
L = [a,b] ? ;
no

| ?- path(a, X, [a,c,e]).
X = e ? ;
no

| ?- npath(b, g, L).
Path: [b,c,d,f,g]
L = 5 ? ;
Path: [b,c,e,g]
L = 4 ? ;
Path: [b,c,e,f,g]
L = 5 ? ;
no

| ?- npath(b, X, 5).
Path: [b,c,d,f,g]
X = g ? ;
Path: [b,c,e,f,g]
X = g ? ;
no

| ?- path(X, Y, L), write(L),fail.
[a,b][a,c][b,c][c,d][c,e][d,h][d,f][e,f][e,g][f,g][a,b,c][a,b,c,d][a,b,c,e][a,b,c,d,h][a,b,c,d,f][a,b,c,d,f,g][a,b,c,e,f][a,b,c,e,g][a,b,c,e,f,g][a,c,d][a,c,e][a,c,d,h][a,c,d,f][a,c,d,f,g][a,c,e,f][a,c,e,g][a,c,e,f,g][b,c,d][b,c,e][b,c,d,h][b,c,d,f][b,c,d,f,g][b,c,e,f][b,c,e,g][b,c,e,f,g][c,d,h][c,d,f][c,d,f,g][c,e,f][c,e,g][c,e,f,g][d,f,g][e,f,g]
no



*/
