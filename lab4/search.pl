
/*

The state is represented in the following way.

[Side, Bank1, Ferry, Bank2]

Side: 0 if ferry is at Bank1 or 1 if the ferry is at Bank2.
Bank1, Ferry, Bank2: List of length two, the first element is
number of missionaries, second element is number of cannibals.

So for example [1, [2, 0], [0, 0], [0, 1]] means that the ferry is
located at Bank2, and that there are 2 missionaries at Bank1, no people
on the ferry, and 1 cannibal at Bank2.

(Test cases at the end of file)

*/

/*  Top helper function to do a breadth first search
    X: Current state
    Y: Target state
    Will print all possible loop-free solutions
*/
search_bf(X, Y) :-
  findall(P, bf_path_rev(X, Y, P), PP),
  write_solution(PP).

/* Top helper function to do a depth first search */
search_df(X, Y) :-
  findall(P, df_path_rev(X, Y, P), PP),
  write_solution(PP).

/* Get BF path in natural order */
bf_path_rev(X, Y, Path) :-
  bf_path([[X]], Y, P),
  reverse(P, Path).

/* Get DF path in natural order */
df_path_rev(X, Y, Path) :-
  df_path([[X]], Y, P),
  reverse(P, Path).

/* Calculate a breadth first path */
bf_path([[Leaf|Branch]|Branches], Leaf, [Leaf|Branch]).
bf_path([[Leaf|Branch]|Branches], Goal, Path) :-
  children(Leaf, Adjacent),
  prevent_loops(Adjacent, Branch, Trimmed),
  dif(Trimmed, []),
  expand([Leaf|Branch], Trimmed, Expanded),
  append(Branches, Expanded, NewBranches),
  bf_path(NewBranches, Goal, Path).
bf_path([[Leaf|Branch]|Branches], Goal, Path) :-
  children(Leaf, Leaves),
  prevent_loops(Leaves, Branch, []),
  bf_path(Branches, Goal, Path).

  /* Calculate a depth first path */
  df_path([[Leaf|Branch]|Branches], Leaf, [Leaf|Branch]).
  df_path([[Leaf|Branch]|Branches], Goal, Path) :-
    children(Leaf, Adjacent),
    prevent_loops(Adjacent, Branch, Trimmed),
    dif(Trimmed, []),
    expand([Leaf|Branch], Trimmed, Expanded),
    append(Expanded, Branches, NewBranches), % The append is reversed -> DFS
    df_path(NewBranches, Goal, Path).
  df_path([[Leaf|Branch]|Branches], Goal, Path) :-
    children(Leaf, Leaves),
    prevent_loops(Leaves, Branch, []),
    df_path(Branches, Goal, Path).

/* Remove new nodes that exists in the branch because they will cause loops */
prevent_loops([], _, []).
prevent_loops([H1|T1], Branch, [H1|Trimmed]) :-
  \+exists(H1, Branch),
  prevent_loops(T1, Branch, Trimmed).
prevent_loops([H1|T1], Branch, Trimmed) :-
  exists(H1, Branch),
  prevent_loops(T1, Branch, Trimmed).

/* Run exists condition for every element in the branch (second argument) */
exists_multi(E, [BH|BT]) :-
  exists(E, BH);
  exists_multi(E, BT).

/* Check if element i exists in the given list */
exists(H, [H|T]).
exists(E, [H|T]) :-
  exists(E, T).

/* Reverse the order of a list */
reverse([], []).
reverse([H|L1], Ln) :-
  reverse(L1, L2),
  append(L2, [H], Ln).

/* Pretty printing of solution */
write_solution(S) :-
  length(S, N),
  dif(N, 0),
  write('----------------------'), nl,
  write_paths(S, 1),
  write('----------------------'), nl,
  write('Number of solutions: '),
  write(N), nl.
write_solution(S) :-
  length(S, N),
  \+dif(N, 0),
  write('No solution found!'), nl.

write_paths([], _).
write_paths([H|T], I) :-
  write('Solution '), write(I), write(':'), nl,
  length(H, L),
  write('L='), write(L), nl,
  write_path(H, 1),
  II is I + 1,
  write_paths(T, II).

write_path([], _).
write_path([H|T], I) :-
  write(I), write('. '),
  write(H), nl,
  II is I + 1,
  write_path(T, II).

/* Expand from the course book p.186 */
expand(X, [], []).
expand(X, [Y|Z], [[Y|X]|W]) :-
  expand(X, Z, W).

/* Get all child states of a given state */
children(S, CC) :-
  findall(C, child(S, C), CC).

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



/*

EXAMPLES:


BREATH FIRST
==============================================================

| ?- search_bf(
  [0, [1,1], [0,0], [0,0]],
  [0, [0,0], [1,1], [0,0]]
).
----------------------
Solution 1:
L=3
1. [0,[1,1],[0,0],[0,0]]
2. [0,[0,1],[1,0],[0,0]]
3. [0,[0,0],[1,1],[0,0]]
Solution 2:
L=3
1. [0,[1,1],[0,0],[0,0]]
2. [0,[1,0],[0,1],[0,0]]
3. [0,[0,0],[1,1],[0,0]]
----------------------
Number of solutions: 2
yes





| ?- search_bf(
  [0, [2,1], [0,0], [0,0]],
  [1, [0,0], [0,0], [2,1]]
).
----------------------
Solution 1:
L=10
1. [0,[2,1],[0,0],[0,0]]
2. [0,[1,1],[1,0],[0,0]]
3. [0,[0,1],[2,0],[0,0]]
4. [1,[0,1],[2,0],[0,0]]
5. [1,[0,1],[1,0],[1,0]]
6. [0,[0,1],[1,0],[1,0]]
7. [0,[0,0],[1,1],[1,0]]
8. [1,[0,0],[1,1],[1,0]]
9. [1,[0,0],[0,1],[2,0]]
10. [1,[0,0],[0,0],[2,1]]
Solution 2:
L=10
1. [0,[2,1],[0,0],[0,0]]
2. [0,[1,1],[1,0],[0,0]]
3. [0,[0,1],[2,0],[0,0]]
4. [1,[0,1],[2,0],[0,0]]
5. [1,[0,1],[1,0],[1,0]]
6. [0,[0,1],[1,0],[1,0]]
7. [0,[0,0],[1,1],[1,0]]
8. [1,[0,0],[1,1],[1,0]]
9. [1,[0,0],[1,0],[1,1]]
10. [1,[0,0],[0,0],[2,1]]
(... solutions removed to save paper ...)
Solution 48:
L=20
1. [0,[2,1],[0,0],[0,0]]
2. [0,[2,0],[0,1],[0,0]]
3. [0,[1,0],[1,1],[0,0]]
4. [0,[1,1],[1,0],[0,0]]
5. [0,[0,1],[2,0],[0,0]]
6. [1,[0,1],[2,0],[0,0]]
7. [1,[0,1],[1,0],[1,0]]
8. [0,[0,1],[1,0],[1,0]]
9. [0,[1,1],[0,0],[1,0]]
10. [0,[1,0],[0,1],[1,0]]
11. [1,[1,0],[0,1],[1,0]]
12. [1,[1,0],[0,0],[1,1]]
13. [1,[1,0],[1,0],[0,1]]
14. [0,[1,0],[1,0],[0,1]]
15. [0,[0,0],[2,0],[0,1]]
16. [1,[0,0],[2,0],[0,1]]
17. [1,[0,0],[1,0],[1,1]]
18. [1,[0,0],[1,1],[1,0]]
19. [1,[0,0],[0,1],[2,0]]
20. [1,[0,0],[0,0],[2,1]]
----------------------
Number of solutions: 48
yes





| ?- search_bf(
  [0, [1,0], [0,0], [0,0]],
  [0, [0,0], [0,0], [1,0]]
).
No solution found!
yes



DEPTH FIRST
==============================================================


| ?- search_df(
  [0, [2,1], [0,0], [0,0]],
  [1, [0,0], [0,0], [2,1]]
).
----------------------
Solution 1:
L=18
1. [0,[2,1],[0,0],[0,0]]
2. [0,[1,1],[1,0],[0,0]]
3. [0,[0,1],[2,0],[0,0]]
4. [1,[0,1],[2,0],[0,0]]
5. [1,[0,1],[1,0],[1,0]]
6. [0,[0,1],[1,0],[1,0]]
7. [0,[0,0],[1,1],[1,0]]
8. [0,[1,0],[0,1],[1,0]]
9. [1,[1,0],[0,1],[1,0]]
10. [1,[1,0],[1,1],[0,0]]
11. [1,[1,0],[1,0],[0,1]]
12. [0,[1,0],[1,0],[0,1]]
13. [0,[0,0],[2,0],[0,1]]
14. [1,[0,0],[2,0],[0,1]]
15. [1,[0,0],[1,0],[1,1]]
16. [1,[0,0],[1,1],[1,0]]
17. [1,[0,0],[0,1],[2,0]]
18. [1,[0,0],[0,0],[2,1]]
(... solutions removed to save paper ...)
Solution 44:
L=10
1. [0,[2,1],[0,0],[0,0]]
2. [0,[2,0],[0,1],[0,0]]
3. [0,[1,0],[1,1],[0,0]]
4. [1,[1,0],[1,1],[0,0]]
5. [1,[1,0],[1,0],[0,1]]
6. [0,[1,0],[1,0],[0,1]]
7. [0,[0,0],[2,0],[0,1]]
8. [1,[0,0],[2,0],[0,1]]
9. [1,[0,0],[1,0],[1,1]]
10. [1,[0,0],[0,0],[2,1]]
(... solutions removed to save paper ...)
Solution 48:
L=14
1. [0,[2,1],[0,0],[0,0]]
2. [0,[2,0],[0,1],[0,0]]
3. [0,[1,0],[1,1],[0,0]]
4. [1,[1,0],[1,1],[0,0]]
5. [1,[1,0],[1,0],[0,1]]
6. [1,[1,0],[0,0],[1,1]]
7. [1,[1,0],[0,1],[1,0]]
8. [0,[1,0],[0,1],[1,0]]
9. [0,[1,1],[0,0],[1,0]]
10. [0,[0,1],[1,0],[1,0]]
11. [0,[0,0],[1,1],[1,0]]
12. [1,[0,0],[1,1],[1,0]]
13. [1,[0,0],[1,0],[1,1]]
14. [1,[0,0],[0,0],[2,1]]
----------------------
Number of solutions: 48
yes




| ?- search_df(
  [0, [1,0], [0,0], [0,0]],
  [0, [0,0], [0,0], [1,0]]
).
No solution found!
yes


*/
