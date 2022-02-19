subm([],[]).
subm([_|T],S):-
    subm(T,S).
subm([H|T],[H|S]):-
    subm(T,S).

sum([],0).
sum([H|T],S):-
    sum(T,S1),
    S is H+S1.

len([],0).
len([_|T],R):-
    len(T,R1),
    R is R1+1.

sol(L,R,N):-
    subm(L,R),
    len(R,Len),
    Len >= N,
    sum(R,S),
    S1 is S mod 3,
    S1 =:= 0.

mainSol(L,N,RL):-
    findall(S,sol(L,S,N),RL).

createList(A,L,L,B):-
    A>B.
createList(A,L,R,B):-
    A=<B,
    N1 is B-1,
    createList(A,[B|L],R,N1).

create(A,B,R):-
    createList(A,[],R,B).

checkInc([]).
checkInc([_]).
checkInc([H1,H2|T]):-
    H1<H2,!,
    checkInc([H2|T]).
checkInc(_):-
    false.

insert(E,L,[E|L]).
insert(E,[H|T],[H|R]):-
    insert(E,T,R).

perm([],[]).
perm([H|T],P):-
    perm(T,R),
    insert(H,R,P).


oneSol(L,R):-
    subm(L,S),
    perm(S,R),
    len(R,Len),
    Len >= 2,
    checkInc(R).

allSol(L,RL):-
    findall(S,oneSol(L,S),RL).

comb([H|_],1,[H]).
comb([_|T],K,C):-
    comb(T,K,C).
comb([H|T],K,[H|C]):-
    K>1,
    K1 is K-1,
    comb(T,K1,C).

arr(L,K,A):-
    comb(L,K,AR),
    perm(AR,A).

soll(L,R):-
    subm(L,Sub),
    perm(Sub,R),
    len(R,Len),
    Len1 is Len mod 2,
    Len1 =:= 0,
    sum(R,S),
    S1 is S mod 2,
    S1 =:= 1.

allSoll(L,RL):-
    findall(A,soll(L,A),RL).
