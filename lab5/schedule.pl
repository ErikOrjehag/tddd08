:- use_module(library(clpfd)).

/*container(0, 2, 2).
container(1, 4, 1).
container(2, 2, 2).
container(3, 1, 1).
*/

on(4,5).

container(0, 1, 10).
container(1, 2, 10).
container(2, 1, 10).
container(3, 1, 10).
/*
5*10 = 50 cost <- better
4* 20 = 80 cost    we want L=5 before L=4
3* 20 = 60 cost
[
start, dur, end, cost, id

T = [
task(0,2,2,2,0),
task(0,1,1,4,1),
task(1,2,3,2,2),
task(3,1,4,1,3)
] ?

Kan man räkna ut cost för alla L
upp till summan av alla resources,
det högsta antalet som
går att använda parallellt,
och sen ta den bästa? !Fullösning!

Det är möjligt att man kan definera resources
som en funktion av två variabler (Duration and Manpower).

*/

start(Tasks, Cost, L) :-
  findall(ID, container(ID, _, _), Containers),
  create_tasks(Containers, Tasks),
  set_constraints(Tasks),
  maplist(task_end, Tasks, Ends),
  maplist(task_start, Tasks, Starts),
  domain(Ends, 0, 1000),
  domain(Starts, 0, 1000),
  maximum(End, Ends),
  L in 0..100,
  indomain(L),
  cumulative(Tasks, [limit(L)]),
  Cost #= L * End,
  append(Starts, Ends, V0),
  append(V0, [Cost, L, End], Vars),
  labeling([minimize(Cost)], Vars).


task_end(task(_,_,End,_,_), End).

task_start(task(Start,_,_,_,_), Start).

maplist(_C_2, [], []).
maplist( C_2, [X|Xs], [Y|Ys]) :-
   call(C_2, X, Y),
   maplist( C_2, Xs, Ys).

create_tasks([], []).
create_tasks([ID|T], [task(_,D,_,R,ID)|Tasks]) :-
  container(ID, R, D),
  create_tasks(T, Tasks).

set_constraints([]).
set_constraints([task(S,D,E,R,ID)|Tasks]) :-
  set_constraints_help(ID, S, E, Tasks),
  set_constraints(Tasks).

set_constraints_help(_, _, _, []).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  on(ID, IDi),
  Si #>= E,
  write(ID), write('>='), write(IDi), nl,
  set_constraints_help(ID, S, E, Tasks).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  on(IDi, ID),
  S #>= Ei,
  write(IDi), write('>='), write(ID), nl,
  set_constraints_help(ID, S, E, Tasks).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  \+on(ID, IDi),
  \+on(IDi, ID),
  write(ID), write(' - '), write(IDi), nl,
  set_constraints_help(ID, S, E, Tasks).
