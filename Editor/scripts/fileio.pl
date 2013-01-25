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
	setup_call_cleanup(open(File, read, S), loadTerms(S), close(S)).

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
	saveMapCtl(S).

saveMapCtl(S):-
	edi:getMapW(MapW),
	edi:getMapH(MapH),
	edi:getCamX(CamX),
	edi:getCamY(CamY),
	edi:getRoomW(RoomW),
	edi:getRoomH(RoomH),
	writet(S, map:resize(MapW,MapH)),
	writet(S, edi:setCamX(CamX)),
	writet(S, edi:setCamY(CamY)),
	writet(S, edi:setRoomW(RoomW)),
	writet(S, edi:setRoomH(RoomH)).

saveBrsh(Count, Count, _).
saveBrsh(I, Count, S) :-
	writet(S, map:brushNew),
	forall(brush:get(I, Props), writet(S, brush:set(I, Props))),
	II is I + 1,
	saveBrsh(II, Count, S).






