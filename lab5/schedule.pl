:- use_module(library(clpfd)).

answer :-
  start(Tasks, Cost, Manpower, Duration),
  write('Manpower: '), write(Manpower), nl,
  write('Duration: '), write(Duration), nl,
  write('Cost: '), write(Cost), nl,
  qsort(Tasks, T),
  write('Schedule as follows:'), nl,
  format("|~`-t~31||~n", []),
  format("|~t~a~3||~t~a~6+|~t~a~9+|~t~a~4+|~t~a~9+|~n", ['ID','Start','Duration','End','Manpower']),
  write_schedule(T),
  format("|~`-t~31||~n", []),
  retractall(id(_,_)).

write_schedule([]).
write_schedule([task(S,D,E,R,INT_ID)|Tasks]) :-
  id(ID, INT_ID),
  format("|~t~a~3||~t~d~6+|~t~d~9+|~t~d~4+|~t~d~9+|~n", [ID,S,D,E,R]),
  write_schedule(Tasks).

start(Tasks, Cost, L, End) :-
  findall(container(ID, R, D), container(ID, R, D), Containers),
  create_tasks(0, Containers, Tasks),
  set_constraints(Tasks),
  maplist(task_end, Tasks, Ends),
  maplist(task_start, Tasks, Starts),
  domain(Ends, 0, 99999),
  domain(Starts, 0, 99999),
  maximum(End, Ends),
  L in 0..99999,
  cumulative(Tasks, [limit(L)]),
  Cost #= L * End,
  append(Starts, Ends, V0),
  append(V0, [Cost], Vars),
  labeling([minimize(Cost)], Vars).

task_end(task(_,_,End,_,_), End).

task_start(task(Start,_,_,_,_), Start).

maplist(_C_2, [], []).
maplist( C_2, [X|Xs], [Y|Ys]) :-
   call(C_2, X, Y),
   maplist( C_2, Xs, Ys).

create_tasks(_, [], []).
create_tasks(INT, [container(ID, R, D)|T], [task(_,D,_,R,INT)|Tasks]) :-
  assert(id(ID, INT)),
  I is INT + 1,
  create_tasks(I, T, Tasks).

/*

*/
set_constraints([]).
set_constraints([task(S,D,E,R,ID)|Tasks]) :-
  set_constraints_help(ID, S, E, Tasks),
  set_constraints(Tasks).

set_constraints_help(_, _, _, []).
set_constraints_help(INT_ID, S, E, [task(Si,_,Ei,_,INT_IDi)|Tasks]) :-
  id(ID, INT_ID),
  id(IDi, INT_IDi),
  on(ID, IDi),
  Si #>= E,
  set_constraints_help(INT_ID, S, E, Tasks).
set_constraints_help(INT_ID, S, E, [task(Si,_,Ei,_,INT_IDi)|Tasks]) :-
  id(ID, INT_ID),
  id(IDi, INT_IDi),
  on(IDi, ID),
  S #>= Ei,
  set_constraints_help(INT_ID, S, E, Tasks).
set_constraints_help(INT_ID, S, E, [task(Si,_,Ei,_,INT_IDi)|Tasks]) :-
  id(ID, INT_ID),
  id(IDi, INT_IDi),
  \+on(ID, IDi),
  \+on(IDi, ID),
  set_constraints_help(INT_ID, S, E, Tasks).

/*
Partitions a list of tasks into one list
containing all the elements bigger than the
pivot element and one list with all the smaller elements.
*/
partition(_, [], [], []).
partition(Pivot, [Elem|T], S, [Elem|G]) :-
  Pivot = task(X,_,_,_,_),
  Elem = task(H,_,_,_,_),
  (H >= X),
  partition(Pivot, T, S, G).
partition(Pivot, [Elem|T], [Elem|S], G) :-
  Pivot = task(X,_,_,_,_),
  Elem = task(H,_,_,_,_),
  (H <  X),
  partition(Pivot, T, S, G).

/*
Quicksort of tasks using the partition predicate
*/
qsort([], []).
qsort([X|T], L1) :-
  partition(X, T, S, G),
  qsort(S, S1),
  qsort(G, G1),
  append(S1, [X|G1], L1).

/*
EXAMPLE 1:

container(a, 2, 2).
container(b, 4, 1).
container(c, 2, 2).
container(d, 1, 1).
on(a, d).
on(b, c).
on(c, d).

| ?- answer.
Manpower: 4
Duration: 4
Cost: 16
Schedule as follows:
|------------------------------|
|ID|Start|Duration|End|Manpower|
| b|    0|       1|  1|       4|
| a|    1|       2|  3|       2|
| c|    1|       2|  3|       2|
| d|    3|       1|  4|       1|
|------------------------------|
yes


EXAMPLE 2:
The purpose of this example is to check that the minimum cost
is selected and not the minimum number of people hired.

container(a, 1, 10).
container(b, 2, 10).
container(c, 1, 10).
container(d, 1, 10).
on(e,f).

Manpower * Duration = Cost:
5 * 10 = 50 cost    <- The best one even though 5 > 3
4 * 20 = 80 cost
3 * 20 = 60 cost

| ?- answer.
Manpower: 5
Duration: 10
Cost: 50
Schedule as follows:
|------------------------------|
|ID|Start|Duration|End|Manpower|
| a|    0|      10| 10|       1|
| b|    0|      10| 10|       2|
| c|    0|      10| 10|       1|
| d|    0|      10| 10|       1|
|------------------------------|
yes

*/
