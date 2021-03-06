:- module(player, [property/1, property/2,
		   hurt/1,
		   isUpdate/0,
		   isMaterialInsidePlayer/1,
		   isMaterialUnderPlayer/1,
		   inWater/0,
		   dead/0,
		   live/0,
		   safe/0, safe/1, xSafe/1, ySafe/1, posSafe/2,
		   stable/0,
		   stun/0,
		   stopStun/0,
		   inStun/0,
		   enterIdle/0,
		   pos/2,
		   setPos/2,
		   credits/1,
		   coins/1,
		   death/1,
		   airLevel/1,
		   enterFall/0,
		   enterSpin/1,
		   touchObject/1,
		   touchObjectInRoom/1,
		   playAnim/1,
		   playDead/0,
		   playDeadWater/0,
		   loseLife/0,
		   respawn/0,
		   updateWaterPlay/0,
		   update/0,
		   canSwim/0,
		   flip/1,
		   setFlip/1,
		   setFlipDir/1]).
:-use_module(def).
:-use_module(util).

:-	recorda(coins, 0),
	recorda(credits, 3),
	recorda(xSafe, 0),
	recorda(ySafe, 0),
	recorda(safe, 0),
	recorda(airLevel, 0),
	recorda(death, def).

property(w).
property(h).
property(xSafe).
property(ySafe).
property(safe).
property(coins).
property(credits).
property(airLevel).
property(death).

property(x, setX).
property(y, setY).
property(status, setStatus).
property(life, setLife).
property(frame, setFrame).
property(costume, setCostume).
property(tile, setTile).
property(stunLevel, setStunLevel).
property(delay, setDelay).
property(layer, setLayer).
property(color, setColor).
property(shader, setShader).
property(flip, setFlip).
property(dir, setDir).
property(tileIdle, setTileIdle).
property(tileWalk, setTileWalk).
property(tileUp, setTileUp).
property(tileJump, setTileJump).
property(emotion, setEmotion).
property(pow, setPow).
property(anim, setAnim).
property(disabled, setDisable).
property(customMove, setCustomMove).


recVar(Tag, Val) :-
	recorded(Tag, CurVal, Ref),
	( var(Val) -> Val = CurVal; ( erase(Ref), recorda(Tag, Val) ) ).

coins(C) :- recVar(coins, C).
credits(Cr) :- recVar(credits, Cr).
xSafe(X) :- recVar(xSafe, X).
ySafe(Y) :- recVar(ySafe, Y).
safe(Safe) :- recVar(safe, Safe).
airLevel(Level) :- recVar(airLevel, Level).
death(D) :- recVar(death, D).

disabled(true) :- disable, !.
disabled(false).

customMove(true) :- customMove, !.
customMove(false).

flip(x)  :-   flipX, \+flipY.
flip(y)  :- \+flipX,   flipY.
flip(xy) :-   flipX,   flipY.
flip(no) :- \+flipX, \+flipY.

setFlip(x)  :-setFlipX(true),  setFlipY(false).
setFlip(y)  :-setFlipX(false), setFlipY(true).
setFlip(xy) :-setFlipX(true),  setFlipY(true).
setFlip(no) :-setFlipX(false), setFlipY(false).

setFlipDir(-1) :- !, setFlipX(true).
setFlipDir(_) :- !, setFlipX(false).

dead :-
	life(Life),
	Life =< 0.
live :- \+ dead.


pos(X, Y) :-
	x(X), y(Y).
setPos(X, Y) :-
	setX(X), setY(Y).

posSafe(X, Y) :-
	xSafe(X), ySafe(Y).

enterIdle :-
	setStatus(idle),
	setDir(0),
	setPow(0),
	setFlipX(false),
	setFrame(0),
	setAnim(2),
	costume(Costume),
	tileIdle(TileIdle),
	emotion(Emotion),
	NewTile is Costume + TileIdle + Emotion,
	setTile(NewTile),
	setStunLevel(0).

damageLife(Damage) :-
	life(Life),
	L is max(0, Life - Damage),
	setLife(L).

hurt(Damage) :-
	util:doRumble(6),
	util:doShake(2),
	game:clearVibrate,
	game:vibrate(100, 100, 2),
	game:vibrate(0, 100, 4),
	sample:play(hurt),
	damageLife(Damage),
	safe(0).
hurt(_).


% Tests if player should update this frame.
% It can also be used to syncronize the update of other objects with the update of the player.
isUpdate :-
	delay(Delay),
	util:isUpdate(Delay).

% +material
% Tests if the specified material is found inside player's bounding box.
isMaterialInsidePlayer(Mat) :-
	matInside(MatI),
	isMaterialMask(MatI, Mat).

% +material
% Tests if the material is found just under player's bounding box.
isMaterialUnderPlayer(Mat) :-
	matUnder(MatU),
	isMaterialMask(MatU, Mat).

isMaterialMask(Mat, MatMask) :-
	def:material(MatMask, MatIdx), !,
	Mat /\ (1 << MatIdx) =\= 0.

inWater :-
	material(water, Water),
	matCenter(Water).

% Tests if player's position is considered safe for respawning or dropping objects
safe :-
	material(air, M1),
	material(water, M2),
	material(cloud, M3),
	material(climb, M4),
	material(wind, M5),
	Mask is (1 << M1) \/ (1 << M2) \/ (1 << M3) \/ (1 << M4) \/ (1 << M5),
	matInside(Mat),
	Mat /\ \Mask =:= 0. % no other materials are allowed

stable :-
	material(block, Bl),
	material(jumpFix, Jf),
	material(jumpPro, Jp),
	material(climb, Cl),
	Mask is (1 << Bl) \/ (1 << Jf) \/ (1 << Jp) \/ (1 << Cl),
	matUnder(Mat),
	Mat /\ Mask =\= 0.

stopStun:-
	setStunLevel(0).

inStun :-
	stunLevel(S),
	def:maxStunLevel(SL),
	S >= SL.

stun :-
	stable,
	live,
	util:doShake(20),
	game:vibrate(100, 0, 20),
	sample:play(stun),
	update:register(player, player:enterIdle),
	playAnimFrames(stun, [0,1,0,1,0,1,0,1,2,3,2,3,2,3,2,3,4,4,4,4], true), !.
stun.

% +tile id
% Player plays all frames from the tile's animation.

playAnim(Tile) :-
	setStatus(scripted),
	def:ptile(Tile, TILEi),
	costume(Costume),
	Tile2 is TILEi + Costume,
	setTile(Tile2),
	setFrame(0),
	setAnim(1),
	tile:find(Tile2, TileIdx),
	tile:frames(TileIdx, TileFrames),
	delay(Delay),
	Frames is TileFrames * Delay,
	update:waitFrames(player, Frames).

playAnimFramesInit(Tile, Delay) :-
	def:ptile(Tile, TILEi),
	costume(Costume),
	Tile2 is TILEi + Costume,
	setStatus(scripted),
	setTile(Tile2),
	setAnim(0),
	delay(Delay).



playAnimFrames(Tile, Frames) :-
	playAnimFrames(Tile, Frames, false).

playAnimFrames(Tile, Frames, Breakdead) :-
	playAnimFramesInit(Tile, Delay),
	playAnimFramesUpdate(Frames, Delay, Breakdead).

playAnimFramesUpdate(_, _, true) :-
	dead.
playAnimFramesUpdate([], _, _) :- !.
playAnimFramesUpdate([Frame|Frames], Delay, Breakdead) :-
	setFrame(Frame),
	update:register(player, player:playAnimFramesUpdate(Frames, Delay, Breakdead)),
	update:waitFrames(player, Delay).

% Set player in fall state
enterFall :-
	setStatus(fall),
	setPow(1),
	setAnim(2).

% +direction -1=left, 0=up; 1=right
% Set player in fall state
enterSpin(Dir) :-
	enterFall,
	setDir(Dir),
	setPow(0),
	setFlipDir(Dir),
	setFrame(1),
	setAnim(2),
	costume(C),
	tileJump(TJ),
	T is C + TJ,
	setTile(T).

% Tests if player touches an object (bounding boxes intersect)
touchObject(Obj) :-
	touchObject(Obj, _OX1, _OY1, _OX2, _OY2).

touchObject(Obj, OX1, OY1, OX2, OY2) :-
	w(W),
	PW is W + 4, % use a small horisontal boggus (-2/+2) so it can pick hard objects
	h(PH),
	pos(X, Y),
	PX1 is X - PW // 2,
	PY1 is Y - PH // 2,
	PX2 is PX1 + PW,
	PY2 is PY1 + PH,
	brush:getX(Obj, OX1),
	brush:getY(Obj, OY1),
	brush:getW(Obj, OW),
	brush:getH(Obj, OH),
	OX2 is OX1 + OW,
	OY2 is OY1 + OH,
	PX1 < OX2, PX2 > OX1,
	PY1 < OY2, PY2 > OY1.

% Tests if player touches an object that's visible in the current room (bounding boxes intersect)
touchObjectInRoom(Obj) :-
	touchObject(Obj, OX1, OY1, OX2, OY2),
	% check room visibility
	game:roomSize(RW, RH),
	game:roomPos(X, Y),
	RX is X * RW,
	RY is Y * RH,
	OX2 >= RX,
	OX1 < RX + RW,
	OY2 >= RY,
	OY1 < RY + RH.


%Dead event requested by player:update when player died on ground.
playDead:-
	sample:play(death),
	update:register(player, player:loseLife),
	player:playAnim(dead).

% Dead in water event requested by player:update when player died in water.
playDeadWater :-
	sample:play(death),
	setStatus(scripted),
	def:ptile(drawn, TILEi),
	costume(Costume),
	Tile is TILEi + Costume,
	setTile(Tile),
	playDeadWaterNext.

playDeadWaterNext :-
	matCenter(MatCenter),
	material(water, MatCenter), !,
	(   isUpdate
	->  y(Y),
	    Y2 is Y - 2,
	    setY(Y2)
	;   true),
	update:register(player, player:playDeadWaterNext).
playDeadWaterNext :-
	delay(Delay),
	D is Delay * 10,
	update:register(player, player:loseLife),
	update:waitFrames(player, D).

% Lose life event.
% Calls game:respawn/1 callback, if available
loseLife :-
	credits(Credits),
	Credits2 is Credits - 1,
	credits(Credits2),
	death(Death),
	(   game:deathMessage(Death, Msg)
	->  update:register(player, player:loseLifeNext(Death, Credits2)),
	    dialog:openMessage(Msg)
	;   loseLifeNext(Death, Credits2)).

loseLifeNext(Death, Credits) :-
	(   Credits =:= 0
	->  update:register(player, game:command(start)),
	    dialog:openMessage('YOU HAVE LOST\nALL YOUR LIVES!')
	;   catch(game:respawn(Death), _, respawn)).


% Default player respawn
% Can also be called at the beginning of the game:respawn/1 callback,
% since it resets some general properties
respawn :-
	sample:play(respawn),
	death(def),
	setLife(100),
	posSafe(XSafe, YSafe),
	setPos(XSafe, YSafe),
	enterIdle,
	music:fade(0, 3),
	util:musicRestore.

update :-
	updateCostume,
	checkWind,
	checkClouds,
	checkWaterPlay,
	checkDamage,
	checkMustDie,
	checkRespawnPosition, !.
	% Custom movement test
%	setCustomMove(true),
%	movement:update.

updateCostume :-
	gamedef:scuba(ScubaId),
	inventory:hasItem(ScubaId)
	-> setCostume(30)
	; setCostume(0).

checkWind :-
	(   isMaterialInsidePlayer(wind)
	->  Power = BasePower + random(WindPow)
	;   isMaterialUnderPlayer(wind)
	->  Power = BasePower),
	def:windPow(WindPow),
	BasePower = WindPow - random(WindPow * 2 + 1),
	y(Y),
	Y1 is Y - Power,
	setY(Y1), !.
checkWind.

checkClouds :-
	(   isMaterialUnderPlayer(cloud)
	->  y(Y),
	    Y1 is Y + 1,
	    setY(Y1)
	;   true).

checkWaterPlay :-
	gamedef:supportWaterPlay
	-> updateWaterPlay
	; true.

checkDamage :-
	(   live
	->  checkDamageMat
	;   true).

checkDamageMat :-
	(   isMaterialInsidePlayer(kill)
	->  setLife(0),
	    death(def)).

checkDamageMat :-
	(   isMaterialInsidePlayer(hurt)
	->  def:dizHurt(Hurt),
	    hurt(Hurt),
	    (	dead
	    ->  death(def)
	    ;	true)).
checkDamageMat.

checkMustDie :-
	checkMustDie1,
	checkMustDie2.

checkMustDie1 :-
	life(Life),
	L is max(0, Life),
	setLife(L).

checkMustDie2 :-
	(   dead,
	    \+ status(scripted)
	->  checkMustDie21
	;    true).

checkMustDie21 :-
	(   matCenter(Mat),
	    material(water, Mat)
	->  update:register(player, player:playDeadWater)
	;   status(idle)
	    ->  update:register(player, player:playDead)
	    ;   true).

checkRespawnPosition :-
	status(Status),
	(Status = idle; Status = walk),
	safe,
	stable,
	safe(Safe),
	Safe =\= 0,
	live,
	pos(X, Y),
	posSafe(X, Y),
	util:musicStore.
checkRespawnPosition.

% Update water stuff, called from HandlerPlayerUpdate(), if your game supports WaterPlay (non-latent).
updateWaterPlay :-
	costume(Costume),
	updateWaterPlay(Costume, BubbleParam),
	updateBubbles(BubbleParam).

updateBubbles(bubble(Debit, Speed)) :-
	(   gamedef:bubbles
	->  ai:updateBubbles(Debit, Speed, 24)
	;   true).

updateWaterPlay(Costume, Bubble) :-
	matCenter(Mat),
	material(water, Mat), !,
	updateWaterPlayUnderWater(Costume, Bubble),
	setDelay(4),
	swim.
updateWaterPlay(Costume, bubble(0, 0)) :-
	def:maxAirLevel(MaxAirLevel),
	airLevel(MaxAirLevel),
	emotion(EmotionID),
	(   def:noairEmotion(Emotion), def:emotion(Emotion, EmotionID)
	->  setEmotion(0)
	;   true),
	setDelay(3),
	def:ptile(up, UpID),
	def:ptile(jump, JumpID),
	setTileUp(UpID),
	setTileJump(JumpID),
	tile(CurTile),
	Tile is CurTile - Costume,
	(   def:ptile(swimup, Tile)
	->  NewTile is Costume + UpID,
	    setTile(NewTile)
	;   true),
	(   (def:ptile(swim, Tile); def:ptile(swimjump, Tile))
	->  NewTile is Costume + JumpID,
	    setTile(NewTile)
	;   true).

% no scuba
updateWaterPlayUnderWater(0, Bubble) :- !,
	airLevel(AirLevel),
	(AirLevel > 0 ->  NewAirLevel is AirLevel - 1, airLevel(NewAirLevel);true),
	def:maxAirLevel(MaxLevel),
	updateWaterPlayUnderWaterLevel(AirLevel, MaxLevel, Bubble, Emotion),
	def:emotion(Emotion, Em),
	setEmotion(Em).
% scuba
updateWaterPlayUnderWater(_, bubble(2, 6)) :-
	def:maxAirLevel(AirLevel),
	airLevel(AirLevel),
	setEmotion(0).

updateWaterPlayUnderWaterLevel(0, _, bubble(0, 0), noair3) :- !,
	(   live
	->  damageLife(2),
	    (dead -> death(water); true)
	;   true).

updateWaterPlayUnderWaterLevel(Level, Max, bubble(2, 6), noair2) :-
	Level < Max // 4, !.
updateWaterPlayUnderWaterLevel(Level, Max, bubble(12, 4), noair1) :-
	Level < Max // 2, !.
updateWaterPlayUnderWaterLevel(_, _, bubble(12, 4), ok).

canSwim :-
	gamedef:flippers(ID),
	inventory:hasItem(ID).
canSwim(true) :-
	canSwim, !.
canSwim(false).

swim :-
	swimPower,
	stopStun,
	canSwim(CanSwim),
	swimTile(CanSwim),
	status(Status),
	(   (Status = jump; Status = fall)
	->  swimJump(CanSwim)
	;   true).

swimTile(CanSwim) :-
	def:ptile(swimup, SwimUp),
	setTileUp(SwimUp),
	swimTile(CanSwim, TileJump),
	setTileJump(TileJump).
swimTile(true, T) :-
	def:ptile(swim, T),!.
swimTile(_, T) :-
	def:ptile(swimjump, T).


swimJump(_) :-
	stable,!,
	enterIdle.
swimJump(CanSwim) :-
	swimClamp(CanSwim),
	costume(Costume),
	swimDir(Costume),
	swimCanSwim(CanSwim).

swimClamp(true) :-
	dir(Dir),
	frame(Frame),
	Dir =\= 0,
	Frame > 3,
	setFrame(3).
swimClamp(_).


swimCanSwim(true) :-
	(   recorded(swimOldJump, OldJump, Ref)
	->  erase(Ref)
	;   OldJump = false),
	(   util:getKey(jump)
	->  NewJump = true
	;   NewJump = false),
	dir(Dir),
	swimCanSwimPow(OldJump, NewJump, Dir),
	recorda(swimOldJump, NewJump).
swimCanSwim(_).


swimCanSwimPow(false, true, Dir) :-
	Dir =\= 0,
	pow(Pow),
	PowClip is max(4, min(7, Pow)),
	setPow(PowClip),
	setStatus(jump),
	setFrame(-1).
swimCanSwimPow(_, _, _).

swimDir(Costume) :-
	swimDirKey(TileDiff),
	Tile is Costume + TileDiff,
	setTile(Tile).

swimDirKey(Tile) :-
	util:getKey(left), !,
        setDir(-1),
	tileJump(Tile),
	setFlipX(true).

swimDirKey(Tile) :-
	util:getKey(right), !,
        setDir(1),
	tileJump(Tile),
	setFlipX(false).

swimDirKey(Tile) :-
        setDir(0),
	tileUp(Tile).

swimPower :-
	pow(Pow),
	P is min(10, Pow),
	setPow(P).













