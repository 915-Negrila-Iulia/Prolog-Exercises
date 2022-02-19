% 1.
% Write a predicate to generate the list of all subsets with all elements of a given list. 
% Eg: [2, 3, 4] N=2 => [[2,3],[2,4],[3,4]]

%combinations
%comb(L-list,K-integer,R)
%flow(i,i)
%flow(i,o)

comb([H|_],1,[H]).
comb([_|T],K,R):-
    comb(T,K,R).
comb([H|T],K,[H|R]):-
    K>1,
    K1 is K-1,
    comb(T,K1,R).

allcomb(L,K,RL):-
    findall(R,comb(L,K,R),RL).
