:- module(fileio, [mapLoad/1,
		   mapLoad2/1,
		   mapSave/1,
		   fixExt/3,
		   roomsLoadNames/1]).

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
	saveBrushes(S),
	saveRooms(S),
	saveMapCtl(S).

saveBrushes(S) :-
	forall(map:brush(B), (brush:getProps(B, Props), writet(S, brush:new(Props)))).


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



saveRooms(S) :-
	map:getMapW(MapW),
	map:getMapH(MapH),
	map:getRoomW(RoomW),
	map:getRoomH(RoomH),
	W is (MapW // RoomW) - 1,
	H is (MapH // RoomH) - 1,
	writet(S, roomNames:reset(false)),
	forall((between(0, W, X), between(0, H, Y), roomNames:get(X, Y, Props), Props \= []), writet(S, roomNames:set(X, Y, Props))).

fixExt(File, NewFile, Ext) :-
	file_name_extension(Name, _, File),
	file_name_extension(Name, Ext, NewFile).

roomsLoadNames(File) :-
	setup_call_cleanup(
	    catch(open(File, read, S), Ex, (core:dl(Ex), fail)), readNames(S), close(S)).

readNames(S) :-
	readLine(S, Line),
	addName(S, Line).


addName(_, end_of_file) :- !.
addName(Stream, [Xc, Yc, Msgc]) :-
	number_codes(X, Xc),
	number_codes(Y, Yc),
	atom_codes(Msg, Msgc),
	roomNames:set(X, Y, name, Msg),
	readNames(Stream).
addName(Stream, _) :-
	readNames(Stream).


readLine(Stream, Words) :-
	readWord(Stream, Word, EndMark),
	(   EndMark = end_of_file ->
	Words = EndMark;
	readLine(Stream, Word, Words, EndMark)).
readLine(_, Word, [Word], end_of_file) :- !.
readLine(_, Word, [Word], end_of_line) :- !.
readLine(Stream, Word, [Word|Words], end_of_word) :-
	readWord(Stream, NextWord, EndMark),
	readLine(Stream, NextWord, Words, EndMark).


readWord(Fd, Codes, EndMark) :-
	get_code(Fd, C0),
	readWord(C0, Fd, Codes, EndMark).
readWord(-1, _, [], end_of_file) :- !.
readWord(10, _, [], end_of_line) :- !.
readWord(59, _, [], end_of_word) :- !.
readWord(13, Fd, L, EndMark) :-
	get_code(Fd, C2),
	readWord(C2, Fd, L, EndMark).
readWord(C, Fd, [C|T], EndMark) :-
	get_code(Fd, C2),
	readWord(C2, Fd, T, EndMark).
