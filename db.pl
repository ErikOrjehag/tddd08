man(nisse).
man(peter).
man(bosse).

woman(ulrika).
woman(bettan).

beatiful(ulrika).
beatiful(nisse).
beatiful(peter).

rich(nisse).
rich(bettan).

strong(bettan).
strong(peter).
strong(bosse).

kind(bosse).

/*
Person is happy if rich or mutual affection
*/
happy(X) :-
    rich(X);
    (man(X), woman(Y);
     man(Y), woman(X)),
    likes(X, Y),
    likes(Y, X).

/*
All men likes beatiful women!
Vet ej om det här är korrekt!
*/
likes(X, Y) :-
    man(X),
    woman(Y),
    beatiful(Y).

/*
Nisse likes all women who like him
*/
likes(nisse, Y) :-
    woman(Y),
    likes(Y, nisse).

/*
Ulrika likes a man if
he is rich and kind
or beatiful and strong
*/
likes(ulrika, X) :-
    man(X),
    (
      ( rich(X), kind(X) );
      ( beatiful(X), strong(X) )
    ),
    likes(X, ulrika).

likes_ulrika :-
    setof(X, likes(X, ulrika), S),
    length(S, N),
    write('People who like ulrika: '),
    write(S),
    nl,
    write(N),
    write(' People likes ulrika').
