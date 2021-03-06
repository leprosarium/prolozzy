:- module(ai, [updateSpider/1,
	      updateChainLink/1,
	      updateTrain/1,
	      updateBubbles/3]).



% IN: int; idx; object index
% Spider AI
% Moves up and down between two positions.
% Used for classic Dizzy spiders.
% user(0) = upper y value
% user(1) = lower y value
% status = direction 0=up, 1=down
updateSpider(Id) :-
	brush:find(Id, Spider),
	\+ brush:disabled(Spider),
	brush:getDelay(Spider, Delay),
	util:isUpdate(Delay),
	brush:getY(Spider, Y),
	brush:get(Spider, status, Status),
	updateSpider(Spider, Y, Y0, Status),
	brush:setY(Spider, Y0).

updateSpider(Spider, Y, Y0, up) :-
	brush:get(Spider, upper, Ty),
	(   Y > Ty
	->  Y0 is Y - 1
	;   Y0 = Y, brush:set(Spider, status, down)).

updateSpider(Spider, Y, Y0, down) :-
	brush:get(Spider, lower, Ty),
	(   Y < Ty
	->  Y0 is Y + 4
	;   Y0 = Y, brush:set(Spider, status, up)).


% ChainLink AI
% Updates chain's height, down to the target's y position.
% Used for elevator's chains or spiders wires.
% target = target object's id
updateChainLink(Id) :-
	brush:find(Id, Link),
	brush:get(Link, target, Id2),
	brush:find(Id2, Target),
	brush:getY(Target, Y1),
	brush:getY(Link, Y2),
	Len is Y1 - Y2,
	brush:setH(Link, Len).

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

updateTrain(Id) :-
	brush:find(Id, Obj),
	\+ brush:disabled(Obj),
	brush:getDelay(Obj, Delay),
	util:isUpdate(Delay),
	brush:get(Obj, status, moves),
	brush:get(Obj, target, Id2),
	brush:find(Id2, Obj2),
	brush:get(Obj2, speed, Speed),
	brush:get(Obj2, toflip, Flip),
	brush:getX(Obj2, X2),
	brush:getY(Obj2, Y2),
	brush:getX(Obj, X),
	brush:getY(Obj, Y),
	brush:setFlip(Obj, Flip),
	updateTrain(X, X2, Xn, Speed),
	updateTrain(Y, Y2, Yn, Speed),
	(   Xn =:= X2, Yn =:= Y2
	->  brush:get(Obj2, target, Target),
	    brush:set(Obj, target, Target)
	;   true),
	brush:setX(Obj, Xn),
	brush:setY(Obj, Yn).

updateTrain(X, X2, Xn, Speed) :- X < X2, !, Xn is min(X + Speed, X2).
updateTrain(X, X2, Xn, Speed) :- X > X2, !, Xn is max(X - Speed, X2).
updateTrain(X, _, X, _).



% +debit; the spawning debit delay factor, higher values means rare spawns, 0 means stopped
% +speed; the moving speed factor of bubbles (with some random variation)
% +life; the number of cycles a bubble lives, until respawned (with some random variation)
updateBubbles(Debit, Speed, Life) :-
	updateActiveBubbles(Life, Free), !,
	spawnBubbles(Free, Debit, Speed).
updateBubbles(_, _, _).

updateActiveBubbles(Life, Free) :-
	(recorded(bubbles, Bubbles); initBubbles(Bubbles)),
	partition(brush:disabled, Bubbles, Free, Active),
	forall(member(B, Active), updateBubble(B, Life)).

initBubbles(Bubbles) :-
	findall(Br, brush:get(Br, class, bubble), Bubbles),
	recorda(bubbles, Bubbles).


updateBubble(Br, Life) :-
	updateBubbleLive(Br, Life)
	-> updateBubblePosition(Br)
	; true.

updateBubbleLive(Br, Life) :-
	  brush:get(Br, time, Time),
	  NTime is Time + 1,
	  (   Time > Life + random(10)
	  ->  brush:disable(Br), !, fail
	  ;   brush:set(Br, time, NTime)).

updateBubblePosition(Br) :-
	newBubblePosition(Br, X, Y),
	game:roomSize(RoomW, RoomH),
	brush:getW(Br, W),
	brush:getH(Br, H),
	Cx is X rem RoomW + W // 2,
	Cy is Y rem RoomH + H // 2,
	core:materialRead(Cx, Cy, 1, 1, Mat),
	def:material(water, Water),
	(   Mat =\= 1 << Water
	->  brush:disable(Br)
	;   brush:setX(Br, X),
	    brush:setY(Br, Y),
	    brush:enable(Br)).

newBubblePosition(Br, X, Y) :-
	brush:get(Br, speed, SpeedY),
	(   random(3) =\= 0
	->  SpeedX = 0
	;   SpeedX is SpeedY // 2 - random(SpeedY)),
	brush:getX(Br, OX),
	brush:getY(Br, OY),
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
	util:nth0(Fr, Free, Br),
	player:pos(PX, PY),
	X is PX + 8 - random(16),
	Y is PY - 16,
	game:roomSize(RoomW, RoomH),
	Cx is X rem RoomW,
	Cy is Y rem RoomH,
	core:materialRead(Cx, Cy, 1, 1, Mat),
	def:material(water, Water),
	Mat =:= 1 << Water,
	brush:enable(Br),
	brush:getW(Br, W),
	brush:getH(Br, H),
	NX is X - W // 2,
	NY is Y - H // 2,
	brush:setX(Br, NX),
	brush:setY(Br, NY),
	BubbleSpeed is Speed // 2 + random(Speed),
	brush:set(Br, speed, BubbleSpeed),
	brush:set(Br, time, 0),
	core:setObjPresent(Br), !.
spawnBubbles(_, _, _).













