%14.
%a. Write a predicate to test equality of two sets without using the set difference.
%b. Write a predicate to select the n-th element of a given list.

% a %

%existence of a number in a set
%existElem(L-set, E-element integer)
%flow model (i,i)

existElem([],_,0):- false.
existElem([H|_],E,1):-
    H=:=E,
    true.
existElem([H|T],E,R):-
    H=\=E,
    existElem(T,E,R).

%compute the length of a set
%lengthSet(L-set, R-result integer)
%flow model(i,o)
%flow model(i,i)

lengthSet([],0).
lengthSet([_|T],R):-
     lengthSet(T,NewR),
     R is NewR+1.

%test equality of 2 sets
%equalSets(L-first set, P-second set)
%flow model(i,i)

equalSets([],_):-true.
equalSets(L,P):-
    lengthSet(L,RL),
    lengthSet(P,RP),
    RL =\= RP,
    false.
equalSets([H|_],P):-
    existElem(P,H,R),
    R =:= 0,
    false.

equalSets([H|T],P):-
    existElem(P,H,R),
    R =:= 1,
    equalSets(T,P).

% b %

%select n-th element of a list
%nElem(L-list,I-integer,N-integer,R-result integer)
%flow model(i,i,o)
%flow model(i,i,i)

nElem([],_,_,0).
nElem([H|_],I,N,R):-
    I =:= N,
    R is H.
nElem([_|T],I,N,R):-
    I =\= N,
    NewI is I + 1,
    nElem(T,NewI,N,R).





