:- use_module(library(clpfd)).

container(a, 2, 2).
container(b, 4, 1).
container(c, 2, 2).
container(d, 1, 1).
on(a, d).
on(b, c).
on(c, d).

/*
container(0, 1, 10).
container(1, 2, 10).
container(2, 1, 10).
container(3, 1, 10).
on(4,5).

5*10 = 50 cost <- better
4* 20 = 80 cost    we want L=5 before L=4
3* 20 = 60 cost

*/

answer :-
  start(Tasks, Cost, Manpower, Duration),
  write('Manpower: '), write(Manpower), nl,
  write('Duration: '), write(Duration), nl,
  write('Cost: '), write(Cost), nl,
  qsort(Tasks, T),
  write_schedule(T).

write_schedule([]).
write_schedule([task(S,D,E,R,ID)|Tasks]) :-
  write('ID: '), write(ID), write(', '),
  write('Start: '), write(S), write(', '),
  write('Duration: '), write(D), write(', '),
  write('End: '), write(E), write(', '),
  write('Manpower: '), write(R), write('.'), nl,
  write_schedule(Tasks).



start(Tasks, Cost, L, End) :-
  findall(container(ID, R, D), container(ID, R, D), Containers),
  create_tasks(Containers, Tasks),
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

create_tasks([], []).
create_tasks([container(ID, R, D)|T], [task(_,D,_,R,ID)|Tasks]) :-
  create_tasks(T, Tasks).

set_constraints([]).
set_constraints([task(S,D,E,R,ID)|Tasks]) :-
  set_constraints_help(ID, S, E, Tasks),
  set_constraints(Tasks).

set_constraints_help(_, _, _, []).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  on(ID, IDi),
  Si #>= E,
  set_constraints_help(ID, S, E, Tasks).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  on(IDi, ID),
  S #>= Ei,
  set_constraints_help(ID, S, E, Tasks).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  \+on(ID, IDi),
  \+on(IDi, ID),
  set_constraints_help(ID, S, E, Tasks).




  /*
  Partitions a list into into one list
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
  Quicksort using the partition predicate
  */
  qsort([], []).
  qsort([X|T], L1) :-
    partition(X, T, S, G),
    qsort(S, S1),
    qsort(G, G1),
    append(S1, [X|G1], L1).
