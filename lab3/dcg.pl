

/*

run(
  [],
  "x:=1; skip; y:=2",
  Res
).

*/





:- [scanner].
:- [imp].

run(In, String, Out) :-
  scan(String, Tokens),
  nl, write(Tokens), nl,
  parse(Tokens, AbstStx),
  nl, write(AbstStx), nl,
  execute(In, AbstStx, Out).

remove(X, [X|T], T).

/*
read_until_token(X, [X|Rs], [], Rs).
read_until_token(X, [H|T], [H|Ls], Rs) :-
  read_until_token(X, T, Ls, Rs).

parse(L, Cmd) :-
  \+ read_until_token(;, L, _, _),
  cmd(L, Cmd).
parse(L, seq(Cmd, Pgm)) :-
  read_until_token(;, L, Ls, Rs),
  cmd(Ls, Cmd),
  parse(Rs, Pgm).

cmd([skip], skip).
cmd([id(X), :=| L], set(id(X), Expr)) :-
  expr(L, Expr).
cmd([if| L], if(Bool, Pgm1, Pgm2)) :-
  read_until_token(then, L, Ls, Rs),
  bool(Ls, Bool),
  parse(Rs, pgm, fasdf)
  parse(P1, Pgm1),
  parse(P2, Pgm2).
*/

parse(L, Pgm) :-
  pgm(L, [], Pgm).

pgm(L, Ln, Cmd) :-
  cmd(L, Ln, Cmd),
  \+remove(;, Ln, _).
pgm(L, Ln, seq(Cmd, Pgm)) :-
  cmd(L, L1, Cmd),
  remove(;, L1, L2),
  pgm(L2, Ln, Pgm).

cmd([skip|L], L, skip).
cmd([id(X), :=|L], Ln, set(X, Expr)) :-
  expr(L, Ln, Expr).
cmd([if|L], Ln, if(Bool, Pgm1, Pgm2)) :-
  bool(L, L1, Bool),
  remove(then, L1, L2),
  parse(L2, L3, Pgm1),
  remove(else, L3, L4),
  parse(L4, L5, Pgm2),
  remove(fi, L5, Ln).
cmd([while|L], Ln, while(Bool, Pgm)) :-
  bool(L, L1, Bool),
  remove(do, L1, L2),
  pgm(L2, Ln, Pgm).

bool(L, Ln, Expr1 > Expr2) :-
  expr(L, L1, Expr1),
  remove(>, L1, L2),
  expr(L2, Ln, Expr2).

expr(L, Ln, Factor * Expr) :-
  factor(L, L1, Factor),
  remove(*, L1, L2),
  expr(L2, Ln, Expr).
expr(L, Ln, Factor) :-
  factor(L, Ln, Factor).

factor(L, Ln, Term + Factor) :-
  term(L, L1, Term),
  remove(+, L1, L2),
  factor(L2, Ln, Factor).
factor(L, Ln, Term) :-
  term(L, Ln, Term).

term([id(X)|Ln], Ln, id(X)).
term([num(X)|Ln], Ln, num(X)).
