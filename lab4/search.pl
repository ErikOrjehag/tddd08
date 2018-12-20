
/*

The state is represented in the following way.

[Bank1, Ferry, Bank2]

Bank1, Bank2: List of length two, the first element is
number of missionaries, second element is number of cannibals.

So for example [[2, 0], [0, 1]] means that there are two missionaries at
Bank1 and one cannibal at Bank2.

Ferry is 1 if the ferry is at Bank1, and 2 if at Bank2

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
  \+memberchk(H1, Branch),
  prevent_loops(T1, Branch, Trimmed).
prevent_loops([H1|T1], Branch, Trimmed) :-
  memberchk(H1, Branch),
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

/*Move one missionary*/
child([[M1, C1], 1, [M2, C2]], [[M1n, C1], 2, [M2n, C2]]) :-
  M1 > 0,
  M1n is M1 - 1,
  M2n is M2 + 1,
  survivable([[M1n, C1], [M2n, C2]]).
child([[M1, C1], 2, [M2, C2]], [[M1n, C1], 1, [M2n, C2]]) :-
  M2 > 0,
  M2n is M2 - 1,
  M1n is M1 + 1,
  survivable([[M1n, C1], [M2n, C2]]).
/*Move two missionaries*/
child([[M1, C1], 1, [M2, C2]], [[M1n, C1], 2, [M2n, C2]]) :-
  M1 > 1,
  M1n is M1 - 2,
  M2n is M2 + 2,
  survivable([[M1n, C1], [M2n, C2]]).
child([[M1, C1], 2, [M2, C2]], [[M1n, C1], 1, [M2n, C2]]) :-
  M2 > 1,
  M2n is M2 - 2,
  M1n is M1 + 2,
  survivable([[M1n, C1], [M2n, C2]]).
/*Move one cannibal*/
child([[M1, C1], 1, [M2, C2]], [[M1, C1n], 2, [M2, C2n]]) :-
  C1 > 0,
  C1n is C1 - 1,
  C2n is C2 + 1,
  survivable([[M1, C1n], [M2, C2n]]).
child([[M1, C1], 2, [M2, C2]], [[M1, C1n], 1, [M2, C2n]]) :-
  C2 > 0,
  C2n is C2 - 1,
  C1n is C1 + 1,
  survivable([[M1, C1n], [M2, C2n]]).
/*Move two cannibals*/
child([[M1, C1], 1, [M2, C2]], [[M1, C1n], 2, [M2, C2n]]) :-
  C1 > 1,
  C1n is C1 - 2,
  C2n is C2 + 2,
  survivable([[M1, C1n], [M2, C2n]]).
child([[M1, C1], 2, [M2, C2]], [[M1, C1n], 1, [M2, C2n]]) :-
  C2 > 1,
  C2n is C2 - 2,
  C1n is C1 + 2,
  survivable([[M1, C1n], [M2, C2n]]).
/*Move one cannibal and one missionary*/
child([[M1, C1], 1, [M2, C2]], [[M1n, C1n], 2, [M2n, C2n]]) :-
  M1 > 0,
  C1 > 0,
  M1n is M1 - 1,
  M2n is M2 + 1,
  C1n is C1 - 1,
  C2n is C2 + 1,
  survivable([[M1n, C1n], [M2n, C2n]]).
child([[M1, C1], 2, [M2, C2]], [[M1n, C1n], 1, [M2n, C2n]]) :-
  M2 > 0,
  C2 > 0,
  M2n is M2 - 1,
  M1n is M1 + 1,
  C2n is C2 - 1,
  C1n is C1 + 1,
  survivable([[M1n, C1n], [M2n, C2n]]).

survivable([[0,_],[0,_]]).
survivable([[M1, C1], [M2, C2]]):-
  dif(M1, 0),
  dif(M2, 0),
  M1 >= C1,
  M2 >= C2.
survivable([[0, _], [M2, C2]]):-
  M2 >= C2.
survivable([[M1, C1], [0, _]]):-
  M1 >= C1.

/*

EXAMPLES:


BREADTH FIRST
==============================================================

| ?- search_bf(
  [[3,3], 1, [0,0]],
  [[0,0], 2, [3,3]]
).
----------------------
Solution 1:
L=12
1. [[3,3],1,[0,0]]
2. [[3,1],2,[0,2]]
3. [[3,2],1,[0,1]]
4. [[3,0],2,[0,3]]
5. [[3,1],1,[0,2]]
6. [[1,1],2,[2,2]]
7. [[2,2],1,[1,1]]
8. [[0,2],2,[3,1]]
9. [[0,3],1,[3,0]]
10. [[0,1],2,[3,2]]
11. [[1,1],1,[2,2]]
12. [[0,0],2,[3,3]]
Solution 2:
L=12
1. [[3,3],1,[0,0]]
2. [[3,1],2,[0,2]]
3. [[3,2],1,[0,1]]
4. [[3,0],2,[0,3]]
5. [[3,1],1,[0,2]]
6. [[1,1],2,[2,2]]
7. [[2,2],1,[1,1]]
8. [[0,2],2,[3,1]]
9. [[0,3],1,[3,0]]
10. [[0,1],2,[3,2]]
11. [[0,2],1,[3,1]]
12. [[0,0],2,[3,3]]
Solution 3:
L=12
1. [[3,3],1,[0,0]]
2. [[2,2],2,[1,1]]
3. [[3,2],1,[0,1]]
4. [[3,0],2,[0,3]]
5. [[3,1],1,[0,2]]
6. [[1,1],2,[2,2]]
7. [[2,2],1,[1,1]]
8. [[0,2],2,[3,1]]
9. [[0,3],1,[3,0]]
10. [[0,1],2,[3,2]]
11. [[1,1],1,[2,2]]
12. [[0,0],2,[3,3]]
Solution 4:
L=12
1. [[3,3],1,[0,0]]
2. [[2,2],2,[1,1]]
3. [[3,2],1,[0,1]]
4. [[3,0],2,[0,3]]
5. [[3,1],1,[0,2]]
6. [[1,1],2,[2,2]]
7. [[2,2],1,[1,1]]
8. [[0,2],2,[3,1]]
9. [[0,3],1,[3,0]]
10. [[0,1],2,[3,2]]
11. [[0,2],1,[3,1]]
12. [[0,0],2,[3,3]]
----------------------
Number of solutions: 4
yes

# Other way around!
| ?- search_bf(
  [[0,0], 2, [3,3]],
  [[3,3], 1, [0,0]]
).
----------------------
Solution 1:
L=12
1. [[0,0],2,[3,3]]
2. [[0,2],1,[3,1]]
3. [[0,1],2,[3,2]]
4. [[0,3],1,[3,0]]
5. [[0,2],2,[3,1]]
6. [[2,2],1,[1,1]]
7. [[1,1],2,[2,2]]
8. [[3,1],1,[0,2]]
9. [[3,0],2,[0,3]]
10. [[3,2],1,[0,1]]
11. [[2,2],2,[1,1]]
12. [[3,3],1,[0,0]]
Solution 2:
L=12
1. [[0,0],2,[3,3]]
2. [[0,2],1,[3,1]]
3. [[0,1],2,[3,2]]
4. [[0,3],1,[3,0]]
5. [[0,2],2,[3,1]]
6. [[2,2],1,[1,1]]
7. [[1,1],2,[2,2]]
8. [[3,1],1,[0,2]]
9. [[3,0],2,[0,3]]
10. [[3,2],1,[0,1]]
11. [[3,1],2,[0,2]]
12. [[3,3],1,[0,0]]
Solution 3:
L=12
1. [[0,0],2,[3,3]]
2. [[1,1],1,[2,2]]
3. [[0,1],2,[3,2]]
4. [[0,3],1,[3,0]]
5. [[0,2],2,[3,1]]
6. [[2,2],1,[1,1]]
7. [[1,1],2,[2,2]]
8. [[3,1],1,[0,2]]
9. [[3,0],2,[0,3]]
10. [[3,2],1,[0,1]]
11. [[2,2],2,[1,1]]
12. [[3,3],1,[0,0]]
Solution 4:
L=12
1. [[0,0],2,[3,3]]
2. [[1,1],1,[2,2]]
3. [[0,1],2,[3,2]]
4. [[0,3],1,[3,0]]
5. [[0,2],2,[3,1]]
6. [[2,2],1,[1,1]]
7. [[1,1],2,[2,2]]
8. [[3,1],1,[0,2]]
9. [[3,0],2,[0,3]]
10. [[3,2],1,[0,1]]
11. [[3,1],2,[0,2]]
12. [[3,3],1,[0,0]]
----------------------
Number of solutions: 4
yes

| ?- search_bf(
  [[1,0], 1, [0,0]],
  [[0,0], 2, [0,0]]
).
No solution found!
yes

DEPTH FIRST
==============================================================

| ?- search_df(
  [[3,3], 1, [0,0]],
  [[0,0], 2, [3,3]]
).
----------------------
Solution 1:
L=12
1. [[3,3],1,[0,0]]
2. [[3,1],2,[0,2]]
3. [[3,2],1,[0,1]]
4. [[3,0],2,[0,3]]
5. [[3,1],1,[0,2]]
6. [[1,1],2,[2,2]]
7. [[2,2],1,[1,1]]
8. [[0,2],2,[3,1]]
9. [[0,3],1,[3,0]]
10. [[0,1],2,[3,2]]
11. [[1,1],1,[2,2]]
12. [[0,0],2,[3,3]]
Solution 2:
L=12
1. [[3,3],1,[0,0]]
2. [[3,1],2,[0,2]]
3. [[3,2],1,[0,1]]
4. [[3,0],2,[0,3]]
5. [[3,1],1,[0,2]]
6. [[1,1],2,[2,2]]
7. [[2,2],1,[1,1]]
8. [[0,2],2,[3,1]]
9. [[0,3],1,[3,0]]
10. [[0,1],2,[3,2]]
11. [[0,2],1,[3,1]]
12. [[0,0],2,[3,3]]
Solution 3:
L=12
1. [[3,3],1,[0,0]]
2. [[2,2],2,[1,1]]
3. [[3,2],1,[0,1]]
4. [[3,0],2,[0,3]]
5. [[3,1],1,[0,2]]
6. [[1,1],2,[2,2]]
7. [[2,2],1,[1,1]]
8. [[0,2],2,[3,1]]
9. [[0,3],1,[3,0]]
10. [[0,1],2,[3,2]]
11. [[1,1],1,[2,2]]
12. [[0,0],2,[3,3]]
Solution 4:
L=12
1. [[3,3],1,[0,0]]
2. [[2,2],2,[1,1]]
3. [[3,2],1,[0,1]]
4. [[3,0],2,[0,3]]
5. [[3,1],1,[0,2]]
6. [[1,1],2,[2,2]]
7. [[2,2],1,[1,1]]
8. [[0,2],2,[3,1]]
9. [[0,3],1,[3,0]]
10. [[0,1],2,[3,2]]
11. [[0,2],1,[3,1]]
12. [[0,0],2,[3,3]]
----------------------
Number of solutions: 4
yes

| ?- search_df(
  [[1,0], 1, [0,0]],
  [[0,0], 2, [0,0]]
).
No solution found!
yes

*/
