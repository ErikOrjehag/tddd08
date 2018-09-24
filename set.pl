union([], [], []).
union([H1|T1], [], [H1|T]) :-
  union(T1, [], T).
union([], [H2|T2], [H2|T]) :-
  union([], T2, T).
union([H1|T1], [H2|T2], [H1|T]) :-
  H1 @< H2,
  union(T1, [H2|T2], T).
union([H1|T1], [H2|T2], [H2|T]) :-
  H1 @>= H2,
  dif(H1, H2),
  union([H1|T1], T2, T).
union([H1|T1], [H2|T2], [H2|T]) :-
  \+ dif(H1, H2),
  union(T1, T2, T).

intersect([], [], []).
intersect([_], [], []).
intersect([], [_], []).
intersect([H1|T1], [H2|T2], [H1|T]) :-
  \+ dif(H1, H2),
  intersect(T1, T2, T).
intersect([H1|T1], [H2|T2], T) :-
  H1 @< H2,
  intersect(T1, [H2|T2], T).
intersect([H1|T1], [H2|T2], T) :-
  H1 @>= H2,
  dif(H1, H2),
  intersect([H1|T1], T2, T).

set([]).
set([_]).
set([H1,H2|T]) :-
  H1 @< H2,
  set([H2|T]).

/*
findall(L, powerset([a, b, c], L), P).

findall(X, del(X, [a, b, c], Y), P).

findall(X, del(_, [a, b, c], X), P).


[
  [a,b,c],
  [
    [b,c],
    [
      [c],
      [
        []
      ]
    ],
    [
      [b],
      [
        []
      ]
    ]
  ],
  [
    [a,c],
    [
      [c],
      [
        []
      ]
    ],
    [
      [a],
      [
        []
      ]
    ]
  ],[[a,b],[[b],[[]]],[[a],[[]]]]]

  findall(X, del(_,[a], X), P).
  findall(a, del(_, [a, b, c], X), P).
*/

pow([], []).
pow([H|T], P) :-
  nl, write('1: '), write(H), write(' '), write(T), write(' '), write(P), nl,
  pow(T,P).
pow([H|T], [H|P]) :-
  nl, write('2: '), write(H), write(' '), write(T), write(' '), write(P), nl,
  pow(T,P).
powerset(L, P) :-
  findall(L, pow([a, b, c], L), P).

/*
powerset([a,b,c], L).

set([]).
set([a]).
set([a, b, c]).
set([a, b, c, e, d, k]).

union([], [], S).
union([], [a], S).
union([a,b,c],[b,c,d], S).
union([b,c,d], [a,b,c], S).
union([a, b], [s, t], S).
union([s, t], [a, b], S).
union([], [a,b,c,d], S).
union([a,c,e], [b,d,f,g,h,i], S).

intersect([], [], S).
intersect([], [a], S).
intersect([a,b,c],[b,c,d], S).
intersect([b,c,d], [a,b,c], S).
intersect([a, b], [s, t], S).
intersect([s, t], [a, b], S).
intersect([], [a,b,c,d], S).
intersect([a,c,e], [b,d,f,g,h,i], S).
intersect([a,c,e,g], [c,g,p], S).


*/
