
/* Facts */
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
All men likes all women who are beatiful
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

/*
Produces the set of men who likes ulrika
and puts them in S.
Calculates the length of the set, and then prints
both the set and the length.
*/
likes_ulrika :-
    setof(X, likes(X, ulrika), S),
    length(S, N),
    write('People who like ulrika: '),
    write(S),
    nl,
    write(N),
    write(' People likes ulrika').



/*

TEST CASES:

| ?- happy(X).
X = nisse ? ;
X = bettan ? ;
X = peter ? ;
X = ulrika ? ;
no

| ?- likes(X,Y).
X = nisse,
Y = ulrika ? ;
X = peter,
Y = ulrika ? ;
X = bosse,
Y = ulrika ? ;
X = ulrika,
Y = peter ? ;
no

| ?- likes_ulrika.
People who like ulrika: [bosse,nisse,peter]
3 People likes ulrika
yes

*/
