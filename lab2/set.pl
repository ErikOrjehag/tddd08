/*
Union of two lists
*/
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

/*
Intersection of two lists
*/
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

/*
Powerset
*/
pow([], []).
pow([_|T], P) :-
  pow(T, P).
pow([H|T], [H|P]) :-
  pow(T, P).
powerset(IN, P) :-
  findall(L, pow(IN, L), P).

/*
powerset([a,b,c], L).

| ?- union([], [], S).
S = [] ? ;
no

| ?- union([], [a], S).
S = [a] ? ;
no

| ?- union([a,b,c],[b,c,d], S).
S = [a,b,c,d] ? ;
no

| ?- union([b,c,d], [a,b,c], S).
S = [a,b,c,d] ? ;
no

| ?- union([a,c,e], [b,d,f,g,h,i], S).
S = [a,b,c,d,e,f,g,h,i] ? ;
no


| ?- intersect([], [], S).
S = [] ? ;
no

| ?- intersect([], [a], S).
S = [] ? ;
no

| ?- intersect([a,b,c],[b,c,d], S).
S = [b,c] ? ;
no

| ?- intersect([b,c,d], [a,b,c], S).
S = [b,c] ? ;
no

| ?- intersect([a,c,e,g], [c,g,p], S).
S = [c,g] ? ;
no

| ?- powerset([a,b,c], L).
L = [[],[c],[b],[b,c],[a],[a,c],[a,b],[a,b,c]] ? ;
no

| ?- powerset([a], L).
L = [[],[a]] ? ;
no

| ?- powerset([],L).
L = [[]] ? ;
no

| ?- powerset([a,b], L).
L = [[],[b],[a],[a,b]] ? ;
no

*/
