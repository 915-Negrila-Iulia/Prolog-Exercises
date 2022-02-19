
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
