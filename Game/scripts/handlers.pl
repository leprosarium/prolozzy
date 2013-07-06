:- module(handlers, [event/1]).

:- use_module(def).
:- use_module(gamedef).
:- use_module(util).
:- use_module(start).

gameSupportJump :-
	gamedef:supportJumpUp -> util:useUpForJump ; true.

nearRoom(X, Y, Xn, Yn) :-
	(   game:viewportMode
	->  X1 is X - 1,
	    X2 is X + 1,
	    Y1 is Y - 1,
	    Y2 is Y + 1,
	    between(X1, X2, Xn),
	    between(Y1, Y2, Yn)
	;   Xn = X, Yn = Y).


updateRoom :-
	game:roomPos(X, Y),
	forall(nearRoom(X, Y, Xn, Yn), catch(game:updateRoom(Xn, Yn), _, true)).
updateRoom.

afterUpdateRoom :-
	game:roomPos(X, Y),
	catch(game:afterUpdateRoom(X, Y), _, true).


scroll :-
	game:viewportMode,
	game:roomSize(RoomW, RoomH),
	game:roomPos(RoomX, RoomY),
	player:pos(X, Y),
	ViewportX is integer(RoomX * RoomW - X + RoomW / 2),
	ViewportY is integer(RoomY * RoomH - Y + RoomH / 2),
	game:setViewportX(ViewportX),
	game:setViewportY(ViewportY).
scroll.

playerDeath(Obj) :-
	brush:getEx(Obj, death, Death),
	player:death(Death).


collision(Obj, hurt, Mode) :-
	Mode =\= 0,
	player:hurt(5),
	(player:dead -> playerDeath(Obj); true).

collision(Obj, kill, Mode) :-
	Mode =\= 0,
	playerDeath(Obj),
	player:setLife(0).
collision(_, _, _).

drawCover(Menuid) :-
	core:hudDrawTile(Menuid, rect(8,48,240,136), rect(8,48,240,136), 0, 0 ), !.

drawMenu(Menuid) :-
	core:hudShader(alpha),
	core:hudColor(0xffffffff),
	core:hudDrawTile(Menuid, rect(0,0,256,48), rect(0,0,256,48), 0, 0 ),
	core:hudDrawTile(Menuid, rect(0,48,8,136), rect(0,48,8,136), 0, 0 ),
	core:hudDrawTile(Menuid, rect(248,48,8,136), rect(248,48,8,136), 0, 0 ),
	core:hudDrawTile(Menuid, rect(0,184,256,8), rect(0,184,256,8), 0, 0 ), !.

drawLifebar :-
	Lifeid = 2,
	player:life(Life),
	W is 55 * Life // 100,
	H = 6,
	core:hudDrawTile(Lifeid, rect(152, 5, W, H), rect(0, 0, W, H), 0, 0 ), !.

drawCredits(Fontid) :-
	core:hudColor(0xffffff00),
	player:credits(Credits),
	drawCredits(Fontid, 0, Credits), !.

drawCredits(_, 3, _) :- !.
drawCredits(_, Credits, Credits) :- !.
drawCredits(Fontid, Credit, Credits) :-
	X is 78 + Credit * 8,
	core:hudDrawText(Fontid, rect(X, 4, 8, 8), '@', 0),
	NextCredit is Credit + 1,
	drawCredits(Fontid, NextCredit, Credits).


drawCoins(Fontid) :-
	player:coins(Coins),
	format(atom(Text), '~`0t~d~2|', Coins),
	core:hudGetTextWidth(Text, W),
	X is 56 - W // 2,
	core:hudDrawText(Fontid, rect(X, 4, W, 8), Text, 0 ), !.

drawTitle(Fontid) :-
	game:roomPos(Xr, Yr),
	roomNames:get(Xr, Yr, name, Name),
	core:hudGetTextWidth(Name, W),
	X is 128 - W // 2,
	core:hudDrawText(Fontid, rect(X, 29, W, 8), Name, 0 ), !.
drawTitle(_) :- !.

jump(-1, 0) :-
	player:inStun, !.
jump(-1, Pow) :-
	def:dizPow(Pow), !.
jump(Mat, Pow) :-
	def:material(jumpFix, Mat),
	def:jumpFixPow(Pow).
jump(Mat, Pow) :-
	def:material(jumpPro, Mat),
	player:pow(Pw),
	Pow1 is Pw // 2,
	(  util:getKey(jump)
	-> def:dizPow(DizPow),
	   Pow2 is Pow1 + DizPow
	;  Pow2 = Pow1),
	def:jumpProPow(ProPow),
	(  Pow2 > ProPow
	-> Pow = ProPow
	;  Pow = Pow2).

event(gameInit) :- !,
	game:state(init).


event(Event) :-
	game:state(State),
	event(State, Event).

event(init, gameStart) :-
	start:load.

event(restart, gameStart) :-
	start:restart.

event(init, gameUpdate) :-
	update:next(init).
event(restart, gameUpdate).
event(cover, gameUpdate) :-
	update:next(ui).

event(_, gameUpdate) :-
	( update:registered(ui) -> update:next(ui); update:next(player)),
	gameSupportJump,
	util:updateShakeAndRumble,
	\+ game:paused,
	player:safe(1),
	updateRoom,
	core:debugData(3, "...Update").


event(init, gameAfterUpdate).
event(restart, gameAfterUpdate).
event(cover, gameAfterUpdate).

event(_, gameAfterUpdate) :-
	afterUpdateRoom,
	scroll.

event(_, roomOpen) :-
	game:roomPos(X, Y),
	catch(game:openRoom(X, Y), _, true).

event(_, roomClose) :-
	game:roomPos(X, Y),
	catch(game:closeRoom(X, Y), _, true).

event(_, roomOut) :-
	game:roomPos(X, Y),
	catch(game:outRoom(X, Y), _, true).

event(_, collision(ID, Mode)) :-
	\+ player:dead,
	brush:find(ID, Obj),
	(   brush:getEx(Obj, class, Class); Class=none),
	collision(Obj, Class, Mode),
	game:collideObject(ID, Mode).


event(_, fall) :-
	(player:dead; player:inWater) ->
	player:stopStun;
	(player:inStun -> update:register(player, player:playStun) ; true).


event(State, drawHud) :- (State = init; State = restart),!,
	core:hudDrawTile(6, rect(0,0,256,192), rect(0,0,256,192), 0, 0 ).


event(cover, drawHud) :-
	Menuid = 1,
	gamedef:fontDefault(Fontid),
	core:hudFont(Fontid),
	core:hudShader(alpha),
	core:hudColor(0xffffffff),
	drawCover(Menuid),
	dialog:drawAll,
	drawMenu(Menuid),
	drawTitle(Fontid).

event(_, drawHud) :-
	Menuid = 1,
	gamedef:fontDefault(Fontid),
	core:hudFont(Fontid),
	core:hudShader(alpha),
	core:hudColor(0xffffffff),
	dialog:drawAll,
	drawMenu(Menuid),
	drawLifebar,
	drawCredits(Fontid),
	drawCoins(Fontid),
	drawTitle(Fontid).


event(attract, menu).
event(cover, menu).
event(_, menu):-
	dialog:empty -> update:register(ui, menu:openDialogGameMenu) ; true.

event(cover, action).
event(attract, action).
event(_, action) :-
	dialog:empty -> update:register(ui, action:action) ; true.

event(_, jump(Mat, _Clean, Pow)) :-
	jump(Mat, Pow0),
	( Pow0 =< 2 -> Pow = 0 ; Pow = Pow0),
	player:stopStun,
	( Pow > 0 -> sample:play(jump) ; true).


% Handler PlayerUpdate
% This handler is called each player update (depending on P_DELAY value).
% It is supposed to customise player behaviour and respond to life lose.
% It may check materials inside player's bound or under it and deal with wind, clouds, water, hurting or killing.
% It is called before keys are checked and player updated by engine.
event(_, playerUpdate) :-
	player:update.


event(_, debug).

event(_, reloadMap) :-
	start:reloadMap.











