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

/*
Skapar en lista med Head H och Tail(Resten av listan) T.
Skapar relation mellan X och H så att H endast kan vara noder med path från X.
Skickar in T och skapar nästa element i listan.
Kollar om 
*/
path(X, Y, L) :-
    path(X, Y), L =[X, Y];
    L = [X|T],
    path(X, H),
    path(H, Y, T).

npath(X, Y, L) :-
    path(X, Y, Z),
    write('Path: '),
    write(Z),
    nl,
    length(Z, L).

