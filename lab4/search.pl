
/*

search(
  [0, [3,3], [0,0], [0,0]]
  [1, [0,0], [0,0], [3, 3]],
  P).

*/

search(X, Y, P) :-
  path(X, Y, [], P).

path(X, Y, V, P).

/* Get all child states of a given state */
children(S, CC) :-
  findall(C, child(S, C), CC).

/* Count number of people on boat or bank */
people([M,C], N) :-
  N is M + C.

/* Is the ferry floating? */
floating(F) :-
  people(F, N),
  N =< 2.

/* Does the ferry have a captain? */
ferryable(F) :-
  floating(F),
  people(F, N),
  N > 0.

/* Is a bank survivable? */
survivable([0,_]).
survivable([M,C]) :-
  M >= C.

/* Remove missionary, if possible */
remove_m([M,C], [Mn,C]) :-
  M >= 1,
  Mn is M - 1.

/* Remove cannibal, if possible */
remove_c([M,C], [M,Cn]) :-
  C >= 1,
  Cn is C - 1.

/* Add missionary */
add_m([M,C], [Mn,C]) :-
  Mn is M + 1.

/* Add cannibal */
add_c([M,C], [M,Cn]) :-
  Cn is C + 1.

/* Move missionary, if possible */
move_m(A, B, An, Bn) :-
  remove_m(A, An),
  add_m(B, Bn).

/* Move cannibal, if possible */
move_c(A, B, An, Bn) :-
  remove_c(A, An),
  add_c(B, Bn).

/* Load missionary from bank to ferry, if possible */
load_m(B, F, Bn, Fn) :-
  move_m(B, F, Bn, Fn),
  survivable(Bn),
  floating(Fn).

/* Load cannibal from bank to ferry, if possible */
load_c(B, F, Bn, Fn) :-
  move_c(B, F, Bn, Fn),
  survivable(Bn),
  floating(Fn).

/* Unload missionary from ferry to bank, if possible */
unload_m(B, F, Bn, Fn) :-
  move_m(F, B, Fn, Bn),
  survivable(Bn),
  floating(Fn).

/* Unload cannibal from ferry to bank, if possible */
unload_c(B, F, Bn, Fn) :-
  move_c(F, B, Fn, Bn),
  survivable(Bn),
  floating(Fn).

/* Load or unload passangers at bank 1 */
child([0, B1, F, B2], [0, B1n, Fn, B2]) :-
  load_m(B1, F, B1n, Fn).
child([0, B1, F, B2], [0, B1n, Fn, B2]) :-
  load_c(B1, F, B1n, Fn).
child([0, B1, F, B2], [0, B1n, Fn, B2]) :-
  unload_m(B1, F, B1n, Fn).
child([0, B1, F, B2], [0, B1n, Fn, B2]) :-
  unload_c(B1, F, B1n, Fn).

/* Move passangers between banks */
child([0, B1, F, B2], [1, B1, F, B2]) :-
  ferryable(F).
child([1, B1, F, B2], [0, B1, F, B2]) :-
  ferryable(F).

/* Load or unload passangers at bank 2 */
child([1, B1, F, B2], [1, B1, Fn, B2n]) :-
  load_m(B2, F, B2n, Fn).
child([1, B1, F, B2], [1, B1, Fn, B2n]) :-
  load_c(B2, F, B2n, Fn).
child([1, B1, F, B2], [1, B1, Fn, B2n]) :-
  unload_m(B2, F, B2n, Fn).
child([1, B1, F, B2], [1, B1, Fn, B2n]) :-
  unload_c(B2, F, B2n, Fn).



/*

children([0,[3,3],[0,0],[0,0]], C).
[
  [0,[3,2],[0,1],[0,0]]
]

children([0,[3,2],[0,1],[0,0]], C).
[
  [0,[2,2],[1,1],[0,0]],
  [0,[3,2],[0,2],[0,0]],
  [0,[3,3],[0,-1],[0,0]],
  [1,[3,2],[0,1],[0,0]]
]

children([1,[3,2],[0,1],[0,0]], C).
[
  [0,[3,2],[0,1],[0,0]],
  [1,[3,2],[0,0],[0,1]]
]

*/
