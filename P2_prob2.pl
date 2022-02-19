% 2. 

% a. Sort a list with keeping double valuesin resulted list. 
% E.g.: [4 2 6 2 3 4] --> [2 2 3 4 4 6]

% b. For a heterogeneous list, formed from integer numbers and list of numbers, write a predicate to sort every sublist, keeping the doubles.
% Eg.: [1, 2, [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>
% [1, 2, [1, 4, 4], 3, 6, [1, 3, 7, 9, 10], 5, [1, 1, 1], 7].

% a %

%return minimum element from a given list
%keeps comparing 2 elements and removes the greater one until
%list remains with only one element, then executes the condition to stop
%and attributes to the M variable the remaining element from the list
%minElem(L-list,M-minimum integer)
%flow(i,i)
%flow(i,o)

minElem([E],E).
minElem([H1,H2|T],M):-
    H1 < H2,!,
    minElem([H1|T],M).
minElem([_,H2|T],M):-
    minElem([H2|T],M).


%remove a given element from the list (1st appearance)
%remElem(L-list,E-integer,F-flag,R-result list)
%flow(i,i,i,i)
%flow(i,i,i,o)

remElem([],_,_,[]).
remElem([H|T],E,F,R):-
    H =:= E,
    F =:= 0,!,
    remElem(T,E,1,R).
remElem([H|T],E,F,[H|R]):-
    remElem(T,E,F,R).

%finds minimum element from the initial list,
%adds it to the result list and
%removes it from the intial list
%repeat until initial list is empty
%sortList(L-list,R-result list)
%flow(i,i)
%flow(i,o)

sortList([],[]).
sortList(L,R):-
    minElem(L,E),
    number(E),
    R = [E|NewR],
    remElem(L,E,0,RL),
    sortList(RL,NewR).

% b %
% given a heterogeneous list, sort every sublist, keeping the doubles

%parses the given list and checks if an element is a list (sublist)
%if so, add that list sorted in the result list
%otherwise, simply add the element in the result list
%sortSublists(L-list,R-result list)
%flow(i,i)
%flow(i,o)

sortSublists([],[]).
sortSublists([H|T],R):-
    is_list(H),!,
    sortList(H,SL),
    R = [SL|NewR],
    sortSublists(T,NewR).
sortSublists([H|T],R):-
    R = [H|NewR],
    sortSublists(T,NewR).
