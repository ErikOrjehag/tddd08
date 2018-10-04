
/*
Import scanner and interpreter.
*/
:- [scanner].
:- [imp].

run(In, String, Out) :-
  scan(String, Tokens),
  parse(Tokens, AbstStx),
  execute(In, AbstStx, Out).

/*
Remove X from beginning of list if possible.
*/
consume(X, [X|T], T).

/*
Parse a program of Tokens in L, return abstract syntax Pgm.
*/
parse(L, Pgm) :-
  pgm(L, [], Pgm).

pgm(L, Ln, Cmd) :-
  cmd(L, Ln, Cmd),
  \+consume(;, Ln, _).
pgm(L, Ln, seq(Cmd, Pgm)) :-
  cmd(L, L1, Cmd),
  consume(;, L1, L2),
  pgm(L2, Ln, Pgm).

/*
predicates for handling commands
*/
cmd([skip|L], L, skip).
cmd([id(X), :=|L], Ln, set(X, Expr)) :-
  expr(L, Ln, Expr).
cmd([if|L], Ln, if(Bool, Pgm1, Pgm2)) :-
  bool(L, L1, Bool),
  consume(then, L1, L2),
  pgm(L2, L3, Pgm1),
  consume(else, L3, L4),
  pgm(L4, L5, Pgm2),
  consume(fi, L5, Ln).
cmd([while|L], Ln, while(Bool, Pgm)) :-
  bool(L, L1, Bool),
  consume(do, L1, L2),
  pgm(L2, L3, Pgm),
  consume(od, L3, Ln).

/*
predicates for handling boolean expressions
*/
bool(L, Ln, Expr1 > Expr2) :-
  expr(L, L1, Expr1),
  consume(>, L1, L2),
  expr(L2, Ln, Expr2).
bool(L, Ln, Expr1 < Expr2) :-
  expr(L, L1, Expr1),
  consume(<, L1, L2),
  expr(L2, Ln, Expr2).

/*
predicates for handling expressions
*/
expr(L, Ln, Factor * Expr) :-
  factor(L, L1, Factor),
  consume(*, L1, L2),
  expr(L2, Ln, Expr).
expr(L, Ln, Factor) :-
  factor(L, Ln, Factor).

/*
predicates for handling factors
*/
factor(L, Ln, Term + Factor) :-
  term(L, L1, Term),
  consume(+, L1, L2),
  factor(L2, Ln, Factor).
factor(L, Ln, Term) :-
  term(L, Ln, Term).

/*
predicates for handling terms
*/
term([id(X)|Ln], Ln, id(X)).
term([num(X)|Ln], Ln, num(X)).


/*

TEST CASE:

| ?- run(
  [x=3],
  "y:=3; z:=0; while x>z do z:=z+1; y:=y*z od",
  Res
).
Res = [x=3,z=3,y=6] ?;
no

| ?- run(
  [x=3],
  "y:=1; z:=0; while x>z do while x+1>y do y:=y+1 od; z:=z+1; y:=y*z od",
  Res
).
Res = [x=3,z=3,y=24] ? ;
no

*/
