



% Commands
execute(_, skip, _).
execute([X=N0|S], set(X, E), [X=N1|S]) :-
  evaluate([X=N0|S], E, N1).

execute(S0, if(B, C1, C2), Sn) :-
  evaluate(S0, B, BB),
  ( BB, execute(S0, C1, Sn) ) ; execute(S0, C2, Sn).
execute(S0, while(B, C), S2) :-
  evaluate(S0, B, tt),
  execute(S0, C, S1),
  execute(S1, while(B, C), S2).
execute(S0, seq(C1, C2), S2) :-
  execute(S0, C1, S1),
  execute(S1, C2, S2).

% Expressions
evaluate(_, num(N), N).
evaluate([I=N], id(I), N).
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
evaluate(S, E1 > E2, B) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N1 > N2, B=tt ; B=ff.
evaluate(S, E1 < E2, B) :-
  evaluate(S, E1, N1),
  evaluate(S, E2, N2),
  N1 < N2, B=tt ; B=ff.

id(I) :- [I], { atom(I) }.
num(N) :- [N], { number(N) }.

/* execute([a=3], set(x, id(a)), Sn). */
/* execute([], set(x, num(5)), Sn). */
/* evaluate([x=3], id(x), N). */
/* evaluate([x=3], id(x) + num(5), N). */
/* evaluate([x=3], id(x) / num(5), N). */
/* evaluate([x=3], id(x) > num(5), B). */
/* execute([x=3], set(x, num(4)), Sn). */



/*
execute([x=3], while(x > num(1), set(x, id(x) - num(1))), Sn).
execute([x=3], while(x < num(1), skip), Sn).

*/
