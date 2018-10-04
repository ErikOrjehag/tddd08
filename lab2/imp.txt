% Helpers
/*
Checks if a binding environment
contains an element
*/
exists([], _).
exists(X, [X=_|Xs]) :-
  exists([], Xs).
exists(X, [Y=_|Ys]) :-
  dif(X, Y),
  exists(X, Ys).

/*
Deletes a variable
from a binding environment
*/
del(X, [X=_|T], T).
del(X, [Y|T], [Y|R]) :-
    del(X, T, R).

/*
Appends an element to a list
*/
app(Ys, [], Ys).
app(Ys, [X|Xs], [X|Zs]) :-
    app(Ys, Xs, Zs).

/*
Replaces the value of a variable in
a binding environment if it exists,
otherwise adds it
*/
replace(X, N, S0, Sn) :-
  del(X, S0, S1),
  app([X=N], S1, Sn).
replace(X, N, S0, Sn) :-
  \+ exists(X, S0),
  app([X=N], S0, Sn).

% Commands
execute(S0, skip, S0).
execute(S, set(X, E), Sn) :-
  evaluate(S, E, N),
  replace(X, N, S, Sn).
execute(S0, if(B, C1, _), Sn) :-
  evaluate(S0, B, tt),
  execute(S0, C1, Sn).
execute(S0, if(B, _, C2), Sn) :-
  evaluate(S0, B, ff),
  execute(S0, C2, Sn).
execute(S0, while(B, C), S2) :-
  evaluate(S0, B, tt),
  execute(S0, C, S1),
  execute(S1, while(B, C), S2).
execute(S, while(B, _), S) :-
  evaluate(S, B, ff).
execute(S0, seq(C1, C2), S2) :-
  execute(S0, C1, S1),
  execute(S1, C2, S2).

% Expressions
/*
Id-expressions
*/
evaluate(_, [], _).
evaluate([I=N], id(I), N).
evaluate([I=N|Is], id(I), N) :-
  dif(Is,[]),
  evaluate(Is, [], N).
evaluate([X=_|Is], id(I), N) :-
  dif(X, I),
  evaluate(Is, id(I), N).
/*
Arithmetic and boolean expressions
*/
evaluate(_, num(N), N).
evaluate(S, E1 + E2, N3) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N3 is N1 + N2.
evaluate(S, E1 - E2, N3) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N3 is N1 - N2.
evaluate(S, E1 * E2, N3) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N3 is N1 * N2.
evaluate(S, E1 / E2, N3) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N3 is N1 / N2.
evaluate(S, E1 > E2, tt) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N1 > N2.
evaluate(S, E1 > E2, ff) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N1 =< N2.
evaluate(S, E1 < E2, tt) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N1 < N2.
evaluate(S, E1 < E2, ff) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N1 >= N2.

/*
Atoms
*/
id(I) :- [I], { atom(I) }.
num(N) :- [N], { number(N) }.

/*

| ?- execute([a=1, x=3, b=2], set(x, num(5)), Sn).
Sn = [a=1,b=2,x=5] ? ;
no

| ?- execute([x=3], seq(set(x, id(x) + num(1)), set(x, id(x) + num(1))), Sn).
Sn = [x=5] ? ;
no

| ?- execute([x=4], if(id(x) > num(3), set(x, num(0)), set(x, num(1))), Sn).
Sn = [x=0] ? ;
no

| ?- execute([x=3],
  seq(
    if(
      id(x) < num(5),
      set(
        y,
        num(1)
      ),
      skip
    ),
    set(
      x,
      id(x) - num(1)
    )
  )
, Sn).
Sn = [y=1,x=2] ? ;
no

| ?- execute([x=10],
  while(
    id(x) > num(1),
    seq(
      if(
        id(x) < num(5),
        set(
          y,
          num(1)
        ),
        skip
      ),
      set(
        x,
        id(x) - num(1)
      )
    )
  ), Sn
).
Sn = [y=1,x=1] ? ;
no

| ?- execute([x=3],
  seq(set(y,num(1)),
    while(id(x) > num(1),
      seq(set(y, id(y) * id(x)),
        set(x, id(x) - num(1)))))
  ,Sn).
Sn = [y=6,x=1] ? ;
no
*/
