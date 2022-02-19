divSum(N, CurrDiv, 0):-
    N2 is N / 2,
    CurrDiv > N2.
divSum(N, CurrDiv, S):-
    N mod CurrDiv =:= 0,!,
    NextDiv is CurrDiv + 1,
    divSum(N, NextDiv, S1),
    S is S1 + CurrDiv.
divSum(N, CurrDiv, S):-
    NextDiv is CurrDiv + 1,
    divSum(N, NextDiv, S).

dsMain(N, S):-
    divSum(N,2,S).

mul(_,[],[]).
mul(E,[H|T],[H1|R1]):-
    H1 is E*H,
    mul(E,T,R1).

evenProd([],1).
evenProd([H|T],P):-
    H mod 2 =:= 0,!,
    evenProd(T,P1),
    P is P1 * H.
evenProd([_|T],P):-
    evenProd(T,P).

addOnPos(_,P,[],[]):-
    P > 1.
addOnPos(E,P,[],[E]):-
    P =:= 1.
addOnPos(E,P,L,R):-
    P =:= 1,
    R = [E|L].
addOnPos(E,P,[H|T],[H|R]):-
    P > 1,
    P1 is P - 1,
    addOnPos(E,P1,T,R).

add(_,P,_,[],[]):-
    P > 1.
add(E,P,_,[],[E]):-
    P =:= 1.
add(E,P,Pmultiple,L,[E|R]):-
    P =:= 1,
    add(E,Pmultiple,Pmultiple,L,R).
add(E,P,Pmultiple,[H|T],[H|R]):-
    P > 1,
    P1 is P - 1,
    add(E,P1,Pmultiple,T,R).

addMain(E,P,L,R):-
    add(E,P,P,L,R).

delFirst(_,[],[]).
delFirst(E,[H|T],R):-
    E =:= H,!,
    R = T.
delFirst(E,[H|T],[H|R]):-
    delFirst(E,T,R).

delAll(_,[],[]).
delAll(E,[H|T],R):-
    H =:= E,!,
    delAll(E,T,R).
delAll(E,[H|T],[H|R]):-
    delAll(E,T,R).

nrOcc([],_,0).
nrOcc([H|T],E,C):-
    E =:= H,!,
    nrOcc(T,E,C1),
    C is C1 + 1.
nrOcc([_|T],E,C):-
    nrOcc(T,E,C).

nrCol([],_,Col,Col).
nrCol([H|T],E,Col,R):-
    H =:= E,!,
    Col1 is Col + 1,
    nrCol(T,E,Col1,R).
nrCol([_|T],E,Col,R):-
    nrCol(T,E,Col,R).

nrOccMain(L,E,R):-
    nrCol(L,E,0,R).


remInc([],[]).
remInc([H],[H]).
remInc([H1,H2],[]):-
    H1<H2.
remInc([H1,H2,H3|T],R):-
    H1<H2,
    H2<H3,
    remInc([H2,H3|T],R).
remInc([H1,H2,H3|T],R):-
    H1<H2,
    H2>=H3,
    remInc([H3|T],R).
remInc([H1,H2|T],[H1|R]):-
    H1>=H2,
    remInc([H2|T],R).

gcd(M,N,R):-
    M =:= N,
    R is M.
gcd(M,N,R):-
    N>M,
    Y is N-M,
    gcd(M,Y,R).
gcd(M,N,R):-
    N<M,
    Y is M-N,
    gcd(Y,N,R).

listGcd([E],E).
listGcd([H|T],R):-
    listGcd(T,R1),
    gcd(H,R1,R).

sum([],0).
sum([H|T],S):-
    sum(T,S1),
    S is S1+H.

modify([],[]).
modify([H|T],[H1|R]):-
    is_list(H),
    sum(H,S),
    S mod 2 =:= 1,!,
    H=[H1|_],
    modify(T,R).
modify([H|T],[H|R]):-
    modify(T,R).

mountain([_],1).
mountain([H1,H2|T],0):-
    H1<H2,
    mountain([H2|T],0).
mountain([H1,H2|T],_):-
    H1>H2,
    mountain([H2|T],1).

mountainMain([H1,H2|T]):-
    H1<H2,
    mountain([H1,H2|T],0).

remOdd([],[]).
remOdd([H|T],[H|R]):-
    H mod 2 =:= 0,!,
    remOdd(T,R).
remOdd([_|T],R):-
    remOdd(T,R).

process([],[]).
process([H|T],[H1|R]):-
    is_list(H),
    mountainMain(H),!,
    remOdd(H,H1),
    process(T,R).
process([H|T],[H|R]):-
    process(T,R).


valley([_],0).
valley([H1,H2|T],1):-
    H1>H2,
    valley([H2|T],1).
valley([H1,H2|T],_):-
    H1<H2,
    valley([H2|T],0).

valleyMain([H1,H2|T]):-
    H1>H2,
    valley([H1,H2|T],1).

subs([],[]).
subs([H|T],[H|R]):-
    subs(T,R).
subs([_|T],R):-
    subs(T,R).

insert(E,L,[E|L]).
insert(E,[H|T],[H|R]):-
    insert(E,T,R).

perm([],[]).
perm([H|T],P):-
    perm(T,R),
    insert(H,R,P).

oneSol(L,R):-
    subs(L,S),
    perm(S,R),
    valleyMain(R).

allSol(L,RL):-
    findall(R,oneSol(L,R),RL).

