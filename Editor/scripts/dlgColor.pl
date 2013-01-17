:-module(dlgColor, [init/0, usedMax/1]).

usedMax(14).

init :-
	usedMax(Max),
	gen(Colors, 0xffffffff, Max),
	recorda(colorUsed, Colors).

gen([], _, 0).
gen([V|Y], V, N) :-
	Ns is N - 1,
	gen(Y, V, Ns).


