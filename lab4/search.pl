

/*

search(
  [0, [m, m, m, c, c, c], [], []]
  [0, [], [], [m, m, m, c, c, c]],
  P).


search(
  [0, [3, 3], [], []]
  [0, [], [], [3, 3]],
  P).


*/

search(X, Y, P) :-
  path(X, Y, [], P).

path(X, Y, V, P).

/* Get all child states of a given state */
children(S, CC) :-
  findall(C, child(S, C), CC).

/* Is the ferry floating? */
floating(F) :-
  length(F, N),
  N =< 2.

/* Is the ferry movable? */
ferryable(F) :-
  F \= [],
  floating(F).

/* Is a bank survivable? */
survivable([]).
survivable(B) :-
  count(m, B, Nm),
  count(c, B, Nc),
  Nm >= Nc.
survivable(B) :-
  count(m, B, 0).

/* Count occurences of X in list */
count(X, [], 0).
count(X, [X|T], N) :-
  count(X, T, N0),
  N is N0 + 1.
count(X, [Y|T], N) :-
  X \= Y,
  count(X, T, N).

/* Remove element X from L, if possible */
remove(X, [Y|T], [Y|Tn]) :-
  remove(X, T, Tn).
remove(X, [X|T], T).

/* Add element X to list L */
add(X, L, [X|L]).

/* Move element X from list A to B, if possible */
move(X, A, B, An, Bn) :-
  remove(X, A, An),
  add(X, B, Bn).

/* Load passanger from bank to ferry, if possible */
load(X, B, F, Bn, Fn) :-
  move(X, B, F, Bn, Fn),
  survivable(Bn),
  floating(Fn).

/* Unload passanger from ferry to bank, if possible */
unload(X, B, F, Bn, Fn) :-
  move(X, F, B, Bn, Fn),
  survivable(Bn),
  floating(Fn).

/* Load or unload passangers at bank 1 */
child([0, B1, F, B2], [0, B1n, Fn, B2]) :-
  load(m, B1, F, B1n, Fn).
child([0, B1, F, B2], [0, B1n, Fn, B2]) :-
  load(c, B1, F, B1n, Fn).
child([0, B1, F, B2], [0, B1n, Fn, B2]) :-
  unload(m, B1, F, B1n, Fn).
child([0, B1, F, B2], [0, B1n, Fn, B2]) :-
  unload(c, B1, F, B1n, Fn).

/* Move passangers between banks */
child([0, B1, F, B2], [1, B1, F, B2]) :-
  ferryable(F).
child([1, B1, F, B2], [0, B1, F, B2]) :-
  ferryable(F).

/* Load or unload passangers at bank 2 */
child([1, B1, F, B2], [1, B1, Fn, B2n]) :-
  load(m, B2, F, B2n, Fn).
child([1, B1, F, B2], [1, B1, Fn, B2n]) :-
  load(c, B2, F, B2n, Fn).
child([1, B1, F, B2], [1, B1, Fn, B2n]) :-
  unload(m, B2, F, B2n, Fn).
child([1, B1, F, B2], [1, B1, Fn, B2n]) :-
  unload(c, B2, F, B2n, Fn).
