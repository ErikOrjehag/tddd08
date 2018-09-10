



execute(S0, IMP, Sn) :-
  C(IMP).


C(set(I, E)) :-



/* execute([], set(x, num(5)), Sn). */

id(I) :- [I], { atom(I) }.
num(N) :- [N], { number(N) }.
