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
    likes(Y, Nisse).

/*
Ulrika likes a man if
he is rich and kind
or beatiful and strong
*/
likes(ulrika, X) :-
    man(X),
    ((rich(X),
    kind(X));
    (beatiful(X),
    strong(X))).
    



