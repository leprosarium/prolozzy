:- module(update, [register/2,
		   regPop/2,
		   next/1,
		   push/1,
		   pop/1,
		   pop/0,
		   waitFrames/2,
		   waitTime/2]).


waitFrames(_, 0).
waitFrames(Key, Frames) :-
	NewFrames is Frames - 1,
	register(Key, waitFrames(Key, NewFrames)).


waitTime(Key, Seconds) :-
	game:fps(FPS),
	Frames is Seconds * FPS,
	waitFrames(Key, Frames).

clear(Key) :-
	forall(recorded(Key, _Golal, Ref), erase(Ref)).
clear(_).

get(Key, Goals) :-
	findall(Goal, recorded(Key, Goal), Goals).

debug(Key, Goals) :-
	get(Key, Goals).
debug([UI, Player]) :-
	get(ui, UI),
	get(player, Player).

registered(Key) :-
	recorded(Key, _).

register(_Key, []).
register(Key, [Goal|Goals]) :-
	recordz(Key, Goal),
	register(Key, Goals).

register(Key, Goal) :-
	recorda(Key, Goal).


regPop(Key, Module:Goal) :-
	Goal =.. [Functor|Args],
	PopGoal =.. [Functor, Var|Args],
	recorda(Key, (pop(Var), !, Module:PopGoal)).

next(Key) :-
	recorded(Key, Goal, Ref),
	erase(Ref),
	callTop(Goal), !.
next(_) :- !.


callTop(Goal) :-
	catch(Goal, E, core:dl('---exception'(E))).
callTop(Goal) :-
	core:dl('+++fail'(Goal)).

push(Term) :-
	recorded(updateDataStack, Cur, Ref),
	erase(Ref),
	recorda(updateDataStack, [Term|Cur]), !.
push(Term) :-
	recorda(updateDataStack, [Term]).

pop(Term) :-
	recorded(updateDataStack, [Term|Tail], Ref),
	erase(Ref),
	recorda(updateDataStack, Tail).

pop :-
	pop(_).











