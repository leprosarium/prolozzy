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
updateSpider(Id) :-
	brush:find(Id, Spider),
	\+ brush:getDisable(Spider, 1),
	brush:getDelay(Spider, Delay),
	util:isUpdate(Delay),
	brush:getY(Spider, Y),
	brush:getEx(Spider, status, Status),
	updateSpider(Spider, Y, Y0, Status),
	brush:setY(Spider, Y0).
% up
updateSpider(Spider, Y, Y0, up) :-
	brush:getEx(Spider, upper, Ty),
	(   Y > Ty
	->  Y0 is Y - 1
	;   (Y0 = Y, brush:setEx(Spider, status, down))).

% down
updateSpider(Spider, Y, Y0, down) :-
	brush:getEx(Spider, lower, Ty),
	(   Y < Ty
	->  Y0 is Y + 4
	;   (Y0 = Y, brush:setEx(Spider, status, up))).


% IN: int; idx; object index
% ChainLink AI
% Updates chain's height, down to the target's y position.
% Used for elevator's chains or spiders wires.
% target = target object's id
updateChainLink(Id) :-
	brush:find(Id, Link),
	brush:getEx(Link, target, Id2),
	brush:find(Id2, Target),
	brush:getY(Target, Y1),
	brush:getY(Link, Y2),
	Len is Y1 - Y2,
	brush:setH(Link, Len).

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

updateTrain(Id) :-
	brush:find(Id, Obj),
	brush:getDisable(Obj, 0),
	brush:getDelay(Obj, Delay),
	util:isUpdate(Delay),
	brush:getEx(Obj, status, moves),
	brush:getEx(Obj, target, Id2),
	brush:find(Id2, Obj2),
	brush:getEx(Obj2, speed, Speed),
	brush:getEx(Obj2, toflip, Flip),
	brush:getX(Obj2, X2),
	brush:getY(Obj2, Y2),
	brush:getX(Obj, X),
	brush:getY(Obj, Y),
	brush:setFlip(Obj, Flip),
	updateTrain(X, X2, Xn, Speed),
	updateTrain(Y, Y2, Yn, Speed),
	(   Xn =:= X2, Yn =:= Y2
	->  (brush:getEx(Obj2, target, Target),
	    brush:setEx(Obj, target, Target))
	;   true),
	brush:setX(Obj, Xn),
	brush:setY(Obj, Yn).

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
	brush:find(Id, Br),
	(   brush:getDisable(Br, 1)
	->  Free = [Br|OtherFree]
	;   Free = OtherFree,
	    updateBubble(Br, Life)),

	NId is Id + 1,
	NCount is Count - 1,
	updateActiveBubbles(NId, NCount, Life, OtherFree).


updateBubble(Br, Life) :-
	updateBubbleLive(Br, Life)
	-> updateBubblePosition(Br)
	; true.

updateBubbleLive(Br, Life) :-
	  brush:getEx(Br, time, Time),
	  NTime is Time + 1,
	  (   Time > Life + random(10)
	  ->  brush:setDisable(Br, 1), !, fail
	  ;   brush:setEx(Br, time, NTime)).

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
	->  brush:setDisable(Br, 1)
	;   brush:setX(Br, X),
	    brush:setY(Br, Y),
	    brush:setDisable(Br, 0)).

newBubblePosition(Br, X, Y) :-
	brush:getEx(Br, speed, SpeedY),
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
	brush:setDisable(Br, 0),
	brush:getW(Br, W),
	brush:getH(Br, H),
	NX is X - W // 2,
	NY is Y - H // 2,
	brush:setX(Br, NX),
	brush:setY(Br, NY),
	BubbleSpeed is Speed // 2 + random(Speed),
	brush:setEx(Br, speed, BubbleSpeed),
	brush:setEx(Br, time, 0),
	brush:idx(Br, Idx),
	core:setObjPresent(Idx), !.
spawnBubbles(_, _, _).













