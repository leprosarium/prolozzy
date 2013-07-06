:-module(inventory, [inv/1, reset/1, count/1, add/1, sub/1, clear/0, find/2, hasItem/1]).

inv(List) :-
	findall(I, recorded(inventory, I), List).

reset(List) :-
	clear,
	forall(util:member(I, List), add(I)).

count(Count) :-
	inv(List),
	length(List, Count).

add(Id) :-
	count(Count),
	gamedef:maxInventory(Max),
	Count < Max,
	recordz(inventory, Id).

sub(Id) :-
	recorded(inventory, Id, Ref),
	erase(Ref).

clear :-
	sub(_) -> clear ; true.

find(Id, Ref) :-
	recorded(inventory, Id, Ref).

hasItem(Id) :-
	find(Id, _Ref).










