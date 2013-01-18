:-module(roomNames, [reset/1, get/3, set/3]).



% IN: int(keep names false/true)
% Call it after map resize or init
reset(false) :-
	eraseAll.
reset(true) :-
	edi:getMapW(MapW),
	edi:getMapH(MapH),
	edi:getRoomW(RoomW),
	edi:getRoomH(RoomH),
	W is (MapW // RoomW) - 1,
	H is (MapH // RoomH) - 1,
	eraseRect(W, H).

eraseAll :-
	forall(recorded(name, _, Ref), erase(Ref)).

eraseRect(W, H) :-
	forall((recorded(name, name(X, Y, _), Ref), (\+ between(0, W, X); \+ between(0, H, Y))), erase(Ref)).

get(X, Y, Name) :-
	recorded(name, name(X, Y, Name));
	Name = "".

set(X, Y, Name) :-
	(   recorded(name, name(X, Y, _), Ref)
	->  erase(Ref)
	;   true),
	recordz(name, name(X, Y, Name)).












