:-module(roomNames, [reset/1, get/3, get/4, set/4, set/3]).



% IN: int(keep names false/true)
% Call it after map resize or init
reset(false) :-
	eraseAll.
reset(true) :-
	map:size(W, H),
	W1 is W - 1,
	H1 is H - 1,
	eraseRect(W1, H1).

eraseAll :-
	forall(recorded(roomProps, _, Ref), erase(Ref)).

eraseRect(W, H) :-
	forall((recorded(roomProps, room(X, Y, _), Ref), (\+ between(0, W, X); \+ between(0, H, Y))), erase(Ref)).

get(X, Y, Prop, Val) :-
	recorded(roomProps, room(X, Y, Props)),
	gen_assoc(Prop, Props, Val).

get(X, Y, List) :-
	(   recorded(roomProps, room(X, Y, Props))
	->  assoc_to_list(Props, List)
	;   List = []).

set(X, Y, Prop, Val) :-
	(   recorded(roomProps, room(X, Y, Props), Ref)
	->  erase(Ref)
	;   empty_assoc(Props)),
	put_assoc(Prop, Props, Val, NewProps),
	recordz(roomProps, room(X, Y, NewProps)).

set(X, Y, List) :-
	(   recorded(roomProps, room(X, Y, _), Ref)
	->  erase(Ref)
	;   true),
	list_to_assoc(List, Props),
	recordz(roomProps, room(X, Y, Props)).








