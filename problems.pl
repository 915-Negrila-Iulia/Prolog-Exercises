%product of even number from a list
%mypro(L-list, R-result integer)
%flow model (i,o) - the result will be computed
%flow model (i,i) - the proposed result will be checked

mypro([],1).
mypro([H|T],R):-
    mod(H,2)=:=0,
    mypro(T,RT),
    R is RT*H.
mypro([H|T],R):-
    mod(H,2)=\=0,
    mypro(T,RT),
    R is RT.

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


func([],[]).
func([H|T],[H|S]):-
    func(T,S).
func([H|T],S):-
    H mod 2 =:= 0,
    func(T,S).

all(L,RL):-
    findall(R,func(L,R),RL).

createList(0,L,L).
createList(N,L,Res):-
    N>0,
    N1 is N-1,
    createList(N1,[N|L],Res).

create(N,Res):-
    createList(N,[],Res).

insert(E,L,[E|L]).
insert(E,[H|T],[H|Res]):-
    insert(E,T,Res).

perm([],[]).
perm([H|T],P):-
    perm(T,TR),
    insert(H,TR,P).

checker([_],_):-true.
checker([H1,H2|T],V):-
    H1-H2 >= V | H2-H1 >= V,!,
    checker([H2|T],V).
checker(_,_):- false.

finalPerm(L,P,V):-
    perm(L,P),
    checker(P,V).

allSol(L,V,RL):-
    findall(P,finalPerm(L,P,V),RL).

exists([_],_):-false.
exists([H|_],E):-
    H =:= E,!,
    true.
exists([_|T],E):-
    exists(T,E).

addEnd([],E,[E]).
addEnd([H|T],E,[H|Res]):-
    addEnd(T,E,Res).
%addEnd([H|T],E,Res):-
%   addEnd(T,E,Res1),
%   Res = [H|Res1].


invers([],Col,Col).
invers([H|T],Col,Res):-
    invers(T,[H|Col],Res).

mainInv(L,Res):- invers(L,[],Res).

onlyEven([],[]).
onlyEven([H|T],Res):-
    H mod 2 =:= 1,!,
    onlyEven(T,Res).
onlyEven([H|T],[H|Res]):-
    onlyEven(T,Res).

pairByElem(A,[B|_],[A,B]):-
    A<B.
pairByElem(A,[_|T],P):-
    pairByElem(A,T,P).

pairsInList([H|T],P):-
    pairByElem(H,T,P).
pairsInList([_|T],P):-
    pairsInList(T,P).

allPairs(L,RL):-
    findall(P,pairsInList(L,P),RL).

sum([],0).
sum([H|T],S):-
    sum(T,S1),
    S is S1+H.

totalSum([],0).
totalSum([H|T],S):-
    is_list(H),!,
    sum(H,S2),
    totalSum(T,S1),
    S is S1+S2.
totalSum([H|T],S):-
    number(H),!,
    totalSum(T,S1),
    S is S1+H.
totalSum([_|T],S):-
    totalSum(T,S).

concatenate([],L,L).
concatenate([H|T],L2,[H|Res]):-
    concatenate(T,L2,Res).

sEven([], []).
sEven([_ | T], S):-
    sEven(T, S).
sEven([H | T], [H | S]):-
    H mod 2 =:= 0,!,
    sEven(T, S).
sEven([H | T], [H | S]):-
    sOdd(T, S).

evenMain(L,RL):-
    findall(S,sEven(L,S),RL).

sOdd([H], [H]):-
    H mod 2 =\= 0,!.
sOdd([_ | T], S):-
    sOdd(T, S).
sOdd([H | T], [H | S]):-
    H mod 2 =:= 0,!,
    sOdd(T, S).
sOdd([H | T], [H | S]):-
    sEven(T, S).

oddMain(L,RL):-
    findall(S,sOdd(L,S),RL).

elimin(E,[E|Res],Res).
elimin(E,[H|T],[H|Res]):-
    elimin(E,T,Res).

mainElim(E,L,RL):-
    findall(X,elimin(E,L,X),RL).

comb([H|_],1,[H]).
comb([_|T],K,C):-
    comb(T,K,C).
comb([H|T],K,[H|C]):-
    K>1,
    K1 is K-1,
    comb(T,K1,C).

checkComb(L,K,C,S):-
    comb(L,K,C),
    checkSum(C,S).

csum([],0).
csum([H|T],SL):-
    csum(T,SL1),
    SL is SL1+H.

checkSum(L,S):-
    csum(L,SL),
    S =:= SL.

allComb(L,K,S,RL):-
    findall(C,checkComb(L,K,C,S),RL).

subm([], []).
subm([_ | T], S):-
    subm(T, S).
subm([H | T], [H | S]):-
    subm(T, S).

allOdd([]):- true.
allOdd([H|_]):-
    H mod 2 =:= 0,!,
    false.
allOdd([_|T]):-
    allOdd(T).

checkSet(L,RL):-
    subm(L,RL),
    allOdd(RL),
    csum(RL,S),
    S mod 2 =:= 0.

subsets(L, RL):-
    findall(S, checkSet(L, S), RL).


ap([_],_):-
    true.
ap([H1,H2|T],CD):-
    H2-H1 =:= CD,!,
    ap([H2|T],CD).

checkAP([H1,H2|T]):-
    CD is H2-H1,
    ap([H1,H2|T],CD).

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
