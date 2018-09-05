path(a, b).
path(a, c).
path(b, c).
path(c, d).
path(c, e).
path(d, h).
path(d, f).
path(e, f).
path(e, g).
path(f, g).

app([], Ys, Ys).
app([X|Xs], Ys, [X|Zs]) :-
    app(Xs, Ys, Zs).

path(L, X, Y) :-
    path(X, Y),
    app(L, Y, Z),
    write('Current L:'), write(Z);
    path(X, U),
    app(L, U, Z),
    path(Z, U, Y).


