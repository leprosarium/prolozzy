:-module(dlgRoomProps, [create/2]).

create(X, Y) :-
	core:dl(dlgRoomPropsCreate(X, Y)).

