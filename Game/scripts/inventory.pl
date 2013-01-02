:-module(inventory, [inv/1, reset/1, count/1, add/1, sub/1, clear/0, find/2, hasItem/1]).

inv(List) :-
	findall(I, recorded(inventory, I), List).

reset(List) :-
	clear,
	forall(util:member(I, List), add(I)).

count(Count) :-
	inv(List),
	length(List, Count).

add(Idx) :-
	count(Count),
	gamedef:maxInventory(Max),
	Count < Max,
	recordz(inventory, Idx).

sub(Idx) :-
	recorded(inventory, Idx, Ref),
	erase(Ref).

clear :-
	sub(_) -> clear ; true.

find(Idx, Ref) :-
	recorded(inventory, Idx, Ref).

hasItem(Id) :-
	map:objFind(Id, Idx),
	Idx =\= -1,
	find(Idx, _Ref).










