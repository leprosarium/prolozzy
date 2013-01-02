:- module(roomprops, [roomNamesFile/1,
		      roomPropsFile/1,
		      roomTextsFile/1,
		      roomsLoadNames/1,
		      roomsLoadTexts/1,
		      roomsLoadProps/1]).
:-use_module(def).

roomNamesFile('Data/Map/dizzy.nam').
roomPropsFile('Data/Map/dizzy.rp').
roomTextsFile('Data/Map/dizzy.rt').

roomInMap(X, Y) :-
	game:mapW(W),
	game:mapH(H),
	X >= 0,
	X < W,
	Y >= 0,
	Y < H.


roomsLoadNames(Filename) :-
	open(Filename, read, Stream),
	readNames(Stream),
	close(Stream).

readNames(Stream) :-
	readLine(Stream, Line),
	addName(Stream, Line).

addName(_, end_of_file) :- !.
addName(Stream, [Xc, Yc, Msgc]) :-
	number_codes(X, Xc),
	number_codes(Y, Yc),
	roomInMap(X, Y),
	atom_codes(Msg, Msgc),
	map:roomName(X, Y, Msg),
	readNames(Stream).
addName(Stream, _) :-
	readNames(Stream).


roomsLoadTexts(Filename) :-
	open(Filename, read, Stream),
	readTexts(Stream),
	close(Stream).

readTexts(Stream) :-
	readLine(Stream, Line),
	addTexts(Stream, Line).

addTexts(_, end_of_file) :- !.
addTexts(Stream, [Xc, Yc|Texts]) :-
	number_codes(X, Xc),
	number_codes(Y, Yc),
	roomInMap(X, Y),
	addTexts(X, Y, Texts, 0, 4),
	readTexts(Stream).
addTexts(Stream, _) :-
	readTexts(Stream).

addTexts(_, _, [], _, _).
addTexts(_, _, _, Max, Max).
addTexts(X, Y, [H|T], Cnt, Max) :-
	atom_codes(Txt, H),
	game:roomSetCustomText(X, Y, Cnt, Txt),
	Cnt2 is Cnt + 1,
	addTexts(X, Y, T, Cnt2, Max).




roomsLoadProps(Filename) :-
	open(Filename, read, Stream),
	readProps(Stream),
	close(Stream).


readProps(Stream) :-
	readLine(Stream, Line),
	addProps(Stream, Line).

addProps(_, end_of_file) :- !.
addProps(Stream, [Xc, Yc|Texts]) :-
	number_codes(X, Xc),
	number_codes(Y, Yc),
	def('R_MAX', Max),
	addProps(X, Y, Texts, 0, Max),
	readProps(Stream).
addProps(Stream, _) :-
	readProps(Stream).

addProps(_, _, [], _, _).
addProps(_, _, _, Max, Max).
addProps(X, Y, [H|T], Cnt, Max) :-
	number_codes(Val, H),
	map:roomProp(X, Y, Cnt, Val),
	Cnt2 is Cnt + 1,
	addProps(X, Y, T, Cnt2, Max).




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










