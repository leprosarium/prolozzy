:- module(movement, [update/0]).


stepx(4). % move step x
stepy(4). % move step y; used in adjustments
stepymax(7). %move step y; used in jumps and falls
boxw(16). %collision box width
boxh(20). %collision box height

update :-
	enterKeyState(Status),
	update(Status),
	update2.

enterKeyState(Status) :-
	player:status(CurStatus),
	(  controllable(CurStatus)
	-> enterKeyState,
	   player:status(Status)
	;  Status = CurStatus).

controllable(idle).
controllable(walk).

update2 :-
	player:status(Status),
	update2(Status).

update2(scripted) :- !.
update2(_) :-
	checkCollidersSnap(Snap),
	player:status(Status),
	(   (controllable(Status), Snap == no)
	->  checkFallY(1, H),
	    (	H > 0
	    ->	enterFall,
		player:y(Y),
		player:pow(Pow),
		NewY is Y + 1,
		NewPow is Pow + 1,
		player:setY(NewY),
		player:setPow(NewPow)
	    ;	true)
	;   true),
	checkCollision.

setTile(NewTile) :- player:tile(NewTile), !.
setTile(NewTile) :-
	player:setFrame(0),
	player:setTile(NewTile).


enterIdle :-
	player:setStatus(idle),
	player:setDir(0),
	player:setPow(0),
	player:setFlipX(false),
	player:costume(Costume),
	player:tileIdle(TileIdle),
	player:emotion(Emotion),
	NewTile is Costume + TileIdle + Emotion,
	setTile(NewTile).


enterWalk(Dir) :-
	player:setStatus(walk),
	player:setDir(Dir),
	player:setPow(0),
	player:setFlipDir(Dir),
	player:costume(Costume),
	player:tileWalk(TileWalk),
	NewTile is Costume + TileWalk,
	setTile(NewTile).

enterJump(Dir, Pow) :-
	player:setStatus(jump),
	player:setDir(Dir),
	player:setPow(Pow),
	player:setFlipDir(Dir),
	enterJumpTile(Dir, Tile),
	player:costume(Costume),
	NewTile is Costume + Tile,
	setTile(NewTile).

enterJumpTile(0, Tile) :- !, player:tileUp(Tile).
enterJumpTile(_, Tile) :- !, player:tileJump(Tile).


enterFall :-
	player:setStatus(fall),
	player:setPow(1).


enterRoll :-
	player:setPow(1), % cut fall power to roll on ground
	enterRoll2,
	enterKeyState,
	handlers:fall,
	player:setStunLevel(0).
enterRoll.

inJumping(Tile) :-
	player:costume(Costume),
	player:tileUp(TileUp),
	player:tileJump(TileJump),
	(   Tile =:= Costume + TileUp;
	Tile =:= Costume + TileJump).

enterRoll2 :-
	player:tile(Tile),
	(   inJumping(Tile)
	->  tile:find(Tile, TileIdx),
	    tile:frames(TileIdx, TileFrames),
	    player:frame(PlayerFrame),
	    player:anim(Anim),
	    computeFrame(0, PlayerFrame, TileFrames, Anim)
	;   true).


enterIdleDead :- player:dead -> enterIdle.

getKeyDir(Dir) :-
	(   util:getKey(right) ->  D1 = 1; D1 = 0),
	(   util:getKey(left) -> Dir is D1 - 1; Dir = D1).

enterJumper(Mat) :-
	\+ enterIdleDead,
	getKeyDir(Dir),
	handlers:jump(Mat, 0, Pow),
	(   Pow > 0
	->  enterJump(Dir, Pow)
	;   enterRoll).
enterJumper(_).

enterSpin(Dir) :-
	enterFall,
	player:setDir(Dir),
	player:setFlipDir(Dir),
	player:costume(Costume),
	player:tileJump(TileJump),
	Tile is Costume + TileJump,
	player:setTile(Tile),
	player:frame(1).


enterKeyState :-
	enterIdleDead -> true;
	getKeyDir(Dir),
	(   util:getKey(jump)
	->  handlers:jump(-1, 0, Pow),
	    (	Pow > 0 -> enterJump(Dir, Pow); true)
	;   (Dir =\= 0 -> enterWalk(Dir); enterIdle)).


nextFrame :-
	player:frame(Frame),
	NextFrame is Frame + 1,
	player:setFrame(NextFrame).


step :-
	player:x(X),
	player:dir(Dir),
	stepx(StepX),
	NewX is X + Dir * StepX,
	player:setX(NewX).

down(Step) :-
	player:y(Y),
	NewY is Y - Step,
	player:setY(NewY).


update(idle) :-
	nextFrame.

update(walk) :-
	( checkWalkX -> step; true),
	nextFrame.

update(jump) :-
	( checkJumpX -> step; true),
	player:pow(Pow),
	stepymax(StepYMax),
	Step is min(Pow, StepYMax),
	checkJumpY(Step, NewStep),
	down(NewStep),
	NewPow is Pow - 1,
	player:setPow(NewPow),
	nextFrame,
	(NewPow < 0->doneJumping;true).

update(fall) :-
	( checkFallX ->  step; true),
	player:pow(Pow),
	stepymax(StepYMax),
	Step is min(Pow, StepYMax),
	checkFallY(Step, Step2),
	DownStep is -Step2,
	down(DownStep),
	nextFrame,
	player:stunLevel(StunLevel),
	NewStunLevel is StunLevel + 1,
	player:setStunLevel(NewStunLevel),
	(   Step2 < Step
	->  (   checkJumper(Mat)
	    ->	enterJumper(Mat)
	    ;	enterRoll)
	;   NewPow is Pow + 1,
	    player:setPow(NewPow)).

update(scripted) :-
	player:anim(0) -> true; nextFrame.

doneJumping :-
	checkFallY(1, Under),
	(  Under =:= 0
	-> enterKeyState
	;  enterFall).



% check side, only above 8 bottom pixels. if bottom is blocked it will step-up on it
checkWalkX :-
	player:makeBB(X1, Y1, X2, Y2),
	player:dir(Dir),
	YY is Y2 - 8,
	stepx(StepX),
	checkWalkX(Dir, StepX, X1, Y1, X2, YY).
checkWalkX(1, StepX, _X1, Y1, X2, Y2) :-
	XX is X2 + StepX,
	core:materialCheckFree(X2, Y1, XX, Y2).
checkWalkX(-1, StepX, X1, Y1, _X2, Y2) :-
	XX is X1 - StepX,
	core:materialCheckFree(XX, Y1, X1, Y2).

checkJumpX :-
	checkWalkX.

%check material above the box and see how far can it go
checkJumpY(Step, NewStep) :-
	player:makeBB(X1, Y1, X2, _Y2),
	YY is Y1 - Step,
	core:materialGetFreeDist(X1, YY, X2, Y1, 1, hard, NewStep). %bottom to top

checkFallX :-
	checkWalkX.

% check material under the box and see how far can it go
checkFallY(Step, NewStep) :-
	player:makeBB(X1, _Y1, X2, Y2),
	YY is Y2 + Step,
	core:materialGetFreeDist(X1, Y2, X2, YY, 0, void, NewStep). %top to cottom


riseUp(Step, StepY) :-

	(   Step < StepY % has some block
	->  player:y(Y),
	    NewY is Y - (StepY - Step),
	    player:setY(NewY)
	;   true).

checkCollision :-
	player:makeBB(X1, _Y1, X2, Y2),
	stepy(StepY),
	YY is Y2 - StepY,
	core:materialGetFreeDist(X1, YY, X2, Y2, 0, hard, Step),
	riseUp(Step, StepY).

jumpMat(jumpFix).
jumpMat(jumpPro).

checkMat(Materials, Mask) :-
	jumpMat(Mat),
	def:material(Mat, Mask),
	Materials /\ (1 << Mask) =\= 0.

%return material if jumper
checkJumper(Mat) :-
	player:makeBB(X1, _Y1, X2, Y2),
	YY is Y2 + 1,
	core:materialRead(X1, Y2, X2, YY, Materials), %read materials under the player
	checkMat(Materials, Mat).

checkCollidersSnap(Snap) :-
	player:makeBBW(X1, Y1, X2, Y2), % bound in world
	core:colliderSnapDistance(X1, Y1, X2, Y2, Dist),
	stepy(StepY),
	(   Dist > 0
	->  Step is min(Dist, StepY),
	    down(Step),
	    Snap = up
	;   YY is Y2 + StepY + 1,
	    core:colliderSnapDistance(X1, Y2, X2, YY, Dist2),
	    Step2 is Dist2 - StepY - 1,
	    (	Step2 >= -StepY
	    ->	down(Step2),
		Snap = down
	    ;	Snap = no)
	;   Snap = no),
	(   Snap \== no, player:status(fall) %stop falls
	->  enterRoll;
	true).

computeFrame(ResFrame, Frame, Count, 1) :- !,
	Frame > Count - 1
	->  ResFrame = Count -1
	;   (Frame < 0 -> ResFrame = 0; ResFrame = Frame).
computeFrame(ResFrame, Frame, Count, 2) :- !,
	Count > 0
	-> ResFrame is Frame rem Count
	;  ResFrame = 0.
computeFrame(Frame, Frame, _Count, _Anim).








