
:- [scanner].
:- [imp].

run(In, String, Out) :-
  scan(String, Tokens),
  parse(Tokens, AbstStx),
  execute(In, AbstStx, Out).

remove(X, [X|T], T).

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





cmd(id(X), [:=|T], Tn, set(id(X), AbstStx)) :-
  expr(T, Tn, AbstStx).

expr([H|T], Tn, Fac) :-
  factor(H, T, [], Fac).
expr([H|T], Tn, Fac * Expr) :-
  factor(H, T, T1, Fac),
  remove(*, T1, T2),
  expr(T2, Tn, Expr).

factor(H, T, Tn, Term) :-
  term(H, Term).
factor(H, T, Tn, Term + Fact) :-
  term(H, Term),
  remove(+, T, Tn).

term(id(X), id(X)).
term(num(X), num(X)).
