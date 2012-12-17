

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

cond1 :- fail.
cond2 :- fail.


test(A):-
	(   cond1
	->  A = 5
	;   (cond2
	    ->	A = 6)), !.

test(0).

test2 :- a -> b, c -> d.
test2.

a.
b.
c :- fail.
d.






