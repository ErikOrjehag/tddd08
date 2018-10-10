
/*

search(
  [0, [1,1], [0,0], [0,0]],
  [0, [0,0], [1,1], [0,0]]
).

remove_explored_2(
  [ [0, [1,1], [0,0], [0,0]], [0, [0,1], [0,0], [0,0]] ],
  [ [0, [0,0], [1,1], [0,0]], [0, [1,1], [0,0], [0,0]] ],
  T
).

*/

bf_path([[Leaf|Branch]|Branches], Leaf, [Leaf|Branch]) :-
  nl, write('1'), nl.
bf_path([[Leaf|Branch]|Branches], Goal, Path) :-
  nl, write('2'), nl,
  children(Leaf, Adjacent),
  %remove_explored(Adjacent, [[Leaf|Branch]|Branches], Trimmed),
  remove_explored_2(Adjacent, Branch, Trimmed),
  dif(Trimmed, []),
  expand([Leaf|Branch], Trimmed, Expanded),
  nl, write(Expanded), nl,
  append(Branches, Expanded, NewBranches),
  bf_path(NewBranches, Goal, Path).
bf_path([[Leaf|Branch]|Branches], Goal, Path) :-
  nl, write('3'), nl,
  children(Leaf, Leaves),
  %remove_explored(Leaves, [[Leaf|Branch]|Branches], []),
  %remove_explored_2(Leaves, Branch, []),
  bf_path(Branches, Goal, Path).

remove_explored_2([], _, []).
remove_explored_2([H1|T1], Branch, [H1|Trimmed]) :-
  \+exists(H1, Branch),
  remove_explored_2(T1, Branch, Trimmed).
remove_explored_2([H1|T1], Branch, Trimmed) :-
  exists(H1, Branch),
  remove_explored_2(T1, Branch, Trimmed).


remove_explored([], _, []).
remove_explored([H1|T1], Branches, [H1|Trimmed]) :-
  \+exists_multi(H1, Branches),
  remove_explored(T1, Branches, Trimmed).
remove_explored([H1|T1], Branches, Trimmed) :-
  exists_multi(H1, Branches),
  remove_explored(T1, Branches, Trimmed).

exists_multi(E, [BH|BT]) :-
  exists(E, BH);
  exists_multi(E, BT).

exists(H, [H|T]).
exists(E, [H|T]) :-
  exists(E, T).

search(X, Y) :-
  bf_path([[X]], Y, P),
  reverse(P, PP),
  write_path(PP).

reverse([], []).
reverse([H|L1], Ln) :-
  reverse(L1, L2),
  append(L2, [H], Ln).


write_path([]).
write_path([H|T]) :-
  write(H), nl,
  write_path(T).

/* Expand from the course book p.186 */
expand(X, [], []).
expand(X, [Y|Z], [[Y|X]|W]) :-
  expand(X, Z, W).

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
survivable([0,C]) :-
  dif(C, 0).
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
[0,[3,1],[0,2],[0,0]],
[0,[3,3],[0,0],[0,0]],
[1,[3,2],[0,1],[0,0]]
]

children([1,[3,2],[0,1],[0,0]], C).
[
  [0,[3,2],[0,1],[0,0]],
  [1,[3,2],[0,0],[0,1]]
]

*/
