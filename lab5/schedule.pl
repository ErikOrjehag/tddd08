:- use_module(library(clpfd)).

container(a, 2, 2).
container(b, 4, 1).
container(c, 2, 2).
container(d, 1, 1).

on(a,d).
on(b,c).
on(c,d).

start(Tasks) :-
  findall(ID, container(ID, _, _), Containers),
  create_tasks(Containers, Tasks),
  set_constraints(Tasks).

create_tasks([], []).
create_tasks([ID|T], [task(_,D,_,R,ID)|Tasks]) :-
  container(ID, R, D),
  create_tasks(T, Tasks).

set_constraints([]).
set_constraints([task(S,D,E,R,ID)|Tasks]) :-
  write(ID), nl,
  set_constraints_help(ID, S, E, Tasks),
  set_constraints(Tasks).

set_constraints_help(_, _, _, []).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  on(ID, IDi),
  S #>= Ei,
  set_constraints_help(ID, S, E, Tasks).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  on(IDi, ID),
  Si #>= E,
  set_constraints_help(ID, S, E, Tasks).
set_constraints_help(ID, S, E, [task(Si,_,Ei,_,IDi)|Tasks]) :-
  \+on(ID, IDi),
  \+on(IDi, ID),
  set_constraints_help(ID, S, E, Tasks).
