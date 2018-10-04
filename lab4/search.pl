

/*
search(
  [[m, m, m, c, c, c], [], []]
  [[], [], [m, m, m, c, c, c]],
  P).

*/

[0, [m, m, m, c, c, c], [], []] -->
  [0, [m, m, m, c, c], [c], []]
  [0, [m, m, m, c], [c, c], []]
  [0, [m, m, c, c], [m, c], []]



/*
  [1, [m, m, c, c], [m, c], []]
  [1, [m, m, m, c], [c, c], []]
  [1, [m, m, m, c, c], [c], []]

[1, [m, m, c, c], [m, c], []] -->
  [1, [m, m, c, c], [m], [c]]
  [1, [m, m, c, c], [], [m, c]]
  [1, [m, m, c, c], [], [m, c]]
*/

search(X, Y, P) :-
  path(X, Y, [], P).

path(X, Y, V, P) :-

children([B1, F, B2], C) :-
  findall().

floating(F) :-
  length(F) =< 2.

ferryable(F) :-
  length(F) > 0,
  floating(F).

survivable([]).
survivable(B) :-
  count(m, B, Nm),
  count(c, B, Nc),
  Nm >= Nc.

count(X, [], 0).
count(X, [Y|T], N) :-
  count(X, T, N).
count(X, [X|T], N) :-
  count(X, T, N0),
  N is N0 + 1.

child([0, [X|B1], F, B2], [0, B1, [X|F], B2]) :-
  survivable(B1),
  floating([X|F]).
child([0, [X|B1], F, B2], [0, B1, [X|F], B2]) :-
  survivable(B1),
  floating([X|F]).
