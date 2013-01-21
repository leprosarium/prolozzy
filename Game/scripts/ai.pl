:- module(ai, [updateSpider/1,
	      updateChainLink/1,
	      updateTrain/1,
	      updateBubbles/5]).



% IN: int; idx; object index
% Spider AI
% Moves up and down between two positions.
% Used for classic Dizzy spiders.
% user(0) = upper y value
% user(1) = lower y value
% status = direction 0=up, 1=down
updateSpider(Idx) :-
	Idx =\= -1,
	\+ obj:disable(Idx),
	obj:delay(Idx, Delay),
	util:isUpdate(Delay),
	obj:y(Idx, Y),
	obj:status(Idx, Status),
	updateSpider(Idx, Y, Y0, Status),
	obj:y(Idx, Y0).
% up
updateSpider(Idx, Y, Y0, 0) :-
	obj:user(Idx, 0, Ty),
	(   Y > Ty
	->  Y0 is Y - 1
	;   (Y0 = Y, obj:status(Idx, 1))).

% down
updateSpider(Idx, Y, Y0, 1) :-
	obj:user(Idx, 1, Ty),
	(   Y < Ty
	->  Y0 is Y + 4
	;   (Y0 = Y, obj:status(Idx, 0))).


% IN: int; idx; object index
% ChainLink AI
% Updates chain's height, down to the target's y position.
% Used for elevator's chains or spiders wires.
% target = target object's id
updateChainLink(Idx) :-
	Idx =\= -1,
	obj:target(Idx, Id2),
	Id2 =\= 0,
	format(atom(AId2), 'id~d', [Id2]),
	map:objFind(AId2, Idx2),
	Idx2 =\= -1,
	obj:y(Idx2, Y1),
	obj:y(Idx, Y2),
	Len is Y1 - Y2,
	obj:h(Idx, Len).

% IN: int; idx; object index
% Train AI
% Those are objects that move from a waypoint to another.
% Can be used for elevators, moving platforms or a walking creature, like a rat.
% Uses a target waypoint from where it takes speed (O_WAYPOINTSPEED) and flip (O_WAYPOINTFLIP).
% When target waypoint is reached, it gets a new target from the waypoint.
% target = id of the current target waypoint
% status = enable state, 1=moves, 0=stays
% Waypoint object:
% user(0) = speed value
% user(1) = flip value

updateTrain(Idx) :-
	Idx =\= -1,
	\+ obj:disable(Idx),
	obj:delay(Idx, Delay),
	util:isUpdate(Delay),
	obj:status(Idx, Status),
	Status =:= 1,
	obj:target(Idx, Id2),
	Id2 =\= 0,
	format(atom(AId2), 'id~d', [Id2]),
	map:objFind(AId2, Idx2),
	Idx2 =\= -1,
	obj:user(Idx2, 0, Speed),
	obj:user(Idx2, 1, Flip),
	obj:x(Idx2, X2),
	obj:y(Idx2, Y2),
	obj:x(Idx, X),
	obj:y(Idx, Y),
	obj:flip(Idx, Flip),
	updateTrain(X, X2, Xn, Speed),
	updateTrain(Y, Y2, Yn, Speed),
	(   Xn =:= X2, Yn =:= Y2
	->  (obj:target(Idx2, Target),
	    obj:target(Idx, Target))
	;   true),
	obj:x(Idx, Xn),
	obj:y(Idx, Yn).

updateTrain(X, X2, Xn, Speed) :-
	updateTrainUp(X, X2, Xu, Speed),
	updateTrainDown(Xu, X2, Xn, Speed).

updateTrainUp(X, X2, Xn, Speed) :-
	X < X2
	->  (Xx is X + Speed, (Xx > X2 -> Xn = X2 ; Xn = Xx))
	;   Xn = X.

updateTrainDown(X, X2, Xn, Speed) :-
	X > X2
	->  (Xx is X - Speed, (Xx < X2 -> Xn = X2 ; Xn = Xx))
	;   Xn = X.





% IN: int; id; first bubble object's id
% IN: int; count; maximum number of bubble objects, with ids starting from id, id+1, id+2, etc
% IN: int; debit; the spawning debit delay factor, higher values means rare spawns, 0 means stopped
% IN: int; speed; the moving speed factor of bubbles (with some random variation)
% IN: int; life; the number of cycles a bubble lives, until respawned (with some random variation)
% This is used to manage the air bubbles the player spawns, while in water.
% disable = if bubble is active or not (default must be disabled)
% user(0) = bubble particular speed value (O_BUBBLESPEED)
% user(1) = bubble life timer (O_BUBBLETIME)
% Make sure all bubbles are disabled by default !
updateBubbles(Id, Count, Debit, Speed, Life) :-
	updateActiveBubbles(Id, Count, Life, Free), !,
	spawnBubbles(Free, Debit, Speed).
updateBubbles(_, _, _, _, _).

updateActiveBubbles(_, 0, _, []).
updateActiveBubbles(Id, Count, Life, Free) :-
	format(atom(AId), 'id~d', [Id]),
	map:objFind(AId, Idx),
	(   obj:disable(Idx)
	->  Free = [Idx|OtherFree]
	;   Free = OtherFree,
	    updateBubble(Idx, Life)),

	NId is Id + 1,
	NCount is Count - 1,
	updateActiveBubbles(NId, NCount, Life, OtherFree).


updateBubble(Idx, Life) :-
	updateBubbleLive(Idx, Life)
	-> updateBubblePosition(Idx)
	; true.

updateBubbleLive(Idx, Life) :-
	  obj:user(Idx, 1, Time),
	  NTime is Time + 1,
	  (   Time > Life + random(10)
	  ->  obj:disable(Idx, 1), !, fail
	  ;   obj:user(Idx, 1, NTime)).

updateBubblePosition(Idx) :-
	newBubblePosition(Idx, X, Y),
	game:roomSize(RoomW, RoomH),
	obj:w(Idx, W),
	obj:h(Idx, H),
	Cx is X rem RoomW + W // 2,
	Cy is Y rem RoomH + H // 2,
	core:materialRead(Cx, Cy, 1, 1, Mat),
	def:material(water, Water),
	(   Mat =\= 1 << Water
	->  obj:disable(Idx, 1)
	;   obj:x(Idx, X),
	    obj:y(Idx, Y),
	    obj:disable(Idx, 0)).

newBubblePosition(Idx, X, Y) :-
	obj:user(Idx, 0, SpeedY),
	(   random(3) =\= 0
	->  SpeedX = 0
	;   SpeedX is SpeedY // 2 - random(SpeedY)),
	obj:x(Idx, OX),
	obj:y(Idx, OY),
	X is OX + SpeedX,
	Y is OY - SpeedY.

spawnBubbles(Free, Debit, Speed) :-
	player:inWater,
	player:live,
	Debit =\= 0,
	length(Free, FreeCount),
	FreeCount > 0,
	random(Debit) =:= 0,
	Fr is random(FreeCount),
	util:nth0(Fr, Free, Idx),
	player:pos(PX, PY),
	X is PX + 8 - random(16),
	Y is PY - 16,
	game:roomSize(RoomW, RoomH),
	Cx is X rem RoomW,
	Cy is Y rem RoomH,
	core:materialRead(Cx, Cy, 1, 1, Mat),
	def:material(water, Water),
	Mat =:= 1 << Water,
	obj:disable(Idx, 0),
	obj:w(Idx, W),
	obj:h(Idx, H),
	NX is X - W // 2,
	NY is Y - H // 2,
	obj:x(Idx, NX),
	obj:y(Idx, NY),
	BubbleSpeed is Speed // 2 + random(Speed),
	obj:user(Idx, 0, BubbleSpeed),
	obj:user(Idx, 1, 0),
	core:objPresent(Idx), !.
spawnBubbles(_, _, _).












