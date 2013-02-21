:- module(fileio, [mapLoad/1,
		   mapLoad2/1,
		  mapSave/1]).

writet(S, T) :-
	write_term(S, T,
		   [ attributes(ignore),
%		     ignore_ops(true),
		     quoted(true),
		     partial(true)
		   ]),
	format(S, '.~n', []).

mapLoad(File) :-
	map:reset,
	map:load(File).

mapLoad2(File) :-
	map:reset,
	setup_call_cleanup(
	    catch(open(File, read, S), Ex, (core:dl(Ex), fail)), loadTerms(S), close(S)).

loadTerms(S) :-
	catch(read_term(S, Term, []), E, (core:dl(readError(E)), fail)),
	loadTerms(Term, S).
loadTerms(end_of_file, _) :- !.
loadTerms(Term, S) :-
	catch(call(Term), E, (core:dl(readError(E)), fail)),
	loadTerms(S), !.

mapSave(File) :-
	core:dl(save(File)),
	setup_call_cleanup(open(File, write, S),
			   saveMapToStream(S),
			   close(S)).
saveMapToStream(S) :-
	map:brushCount(Count),
	once(saveBrsh(0, Count, S)),
	saveRooms(S),
	saveMapCtl(S).

saveMapCtl(S):-
	map:getMapW(MapW),
	map:getMapH(MapH),
	map:getCamX(CamX),
	map:getCamY(CamY),
	map:getRoomW(RoomW),
	map:getRoomH(RoomH),
	writet(S, map:setCamX(CamX)),
	writet(S, map:setCamY(CamY)),
	writet(S, map:setRoomW(RoomW)),
	writet(S, map:setRoomH(RoomH)),
	writet(S, map:resize(MapW,MapH)).

saveBrsh(Count, Count, _).
saveBrsh(I, Count, S) :-
	brush:getProps(I, Props),
	writet(S, brush:new(Props)),
	II is I + 1,
	saveBrsh(II, Count, S).


saveRooms(S) :-
	map:getMapW(MapW),
	map:getMapH(MapH),
	map:getRoomW(RoomW),
	map:getRoomH(RoomH),
	W is (MapW // RoomW) - 1,
	H is (MapH // RoomH) - 1,
	writet(S, roomNames:reset(false)),
	forall((between(0, W, X), between(0, H, Y), roomNames:get(X, Y, Props), Props \= []), writet(S, roomNames:set(X, Y, Props))).



