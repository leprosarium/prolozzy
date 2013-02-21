:- module(game, [state/1,
		 restart/0,
		 property/3,
		 property/2,
		 updateRoom/2,
		 afterUpdateRoom/2,
		 openRoom/2,
		 closeRoom/2,
		 outRoom/2,
		 collideObject/2,
		 objectsSetNames/0,
		 beginNewGame/0,
		 roomPos/2,
		 roomSize/2,
		 shake/1,
		 shake/2,
		 setShake/2,
		 rumble/1,
		 musicSafe/1,
		 musicPosSafe/1,
		 deathMessage/2]).

:- use_module(def).
:- use_module(gamedef).





property(shake, 0).
property(rumble, 0).
property(musicSafe, 0).
property(musicPosSafe, 0).
property(state, init).

:- forall(property(Prop, Def), recorda(Prop, Def)).




property(fps, setFps, 36).
property(keys, setKeys, 0).
property(keysHit, setKeysHit, 0).
property(roomX, setRoomX, 0).
property(roomY, setRoomY, 0).
property(viewX, setViewX, 8).
property(viewY, setViewY, 48).
property(shakeX, setShakeX, 0).
property(shakeY, setShakeY, 0).
property(isPaused, setPaused, true).
property(mapColor, setMapColor, 0xff000000).
property(borderColor, setBorderColor, 0xff000000).
property(ffMagnitude, setFFMagnitude, 0).
property(ffPeriod, setFFPeriod, 50).
property(isViewportMode, setViewportMode, false).
property(viewportX, setViewportX, 0).
property(viewportY, setViewportY, 0).
property(isViewportFlipX, setViewportFlipX, false).
property(isViewportFlipY, setViewportFlipY, false).
property(isFullMaterialMap, setFullMaterialMap, false).

isPaused(true) :- paused, !.
isPaused(false).

setPaused(true) :- pause, !.
setPaused(false) :- unpause.

isViewportMode(true) :- viewportMode, !.
isViewportMode(false).

isViewportFlipX(true) :- viewportFlipX, !.
isViewportFlipX(false).

isViewportFlipY(true) :- viewportFlipY, !.
isViewportFlipY(false).

isFullMaterialMap(true) :- fullMaterialMap, !.
isFullMaterialMap(false).

recVar(Tag, Val) :-
	recorded(Tag, CurVal, Ref),
	( var(Val) -> Val = CurVal; ( erase(Ref), recorda(Tag, Val) ) ).

musicPosSafe(P) :- recVar(musicPosSafe, P).
musicSafe(S) :- recVar(musicSafe, S).
rumble(R) :- recVar(rumble, R).
shake(S) :- recVar(shake, S).
shake(X, Y) :- shakeX(X), shakeY(Y).
setShake(X, Y) :- setShakeX(X), setShakeY(Y).
restart :- state(restart), game:command(start).
state(S) :- recVar(state, S).
roomPos(X, Y) :- roomX(X), roomY(Y).
setRoomPos(X, Y) :- setRoomX(X), setRoomY(Y).
roomSize(W, H) :- roomW(W), roomH(H).

updateRoom(2, 1) :-
	map:objFind(id3000, Platform),
	ai:updateTrain(Platform).
updateRoom(2, 1) :- !.


updateRoom(3, 1) :-
	map:objFind(id7010, Spider),
	ai:updateSpider(Spider),
	map:objFind(id7009, Link),
	ai:updateChainLink(Link),
	map:objFind(id7011, Fly),
	ai:updateTrain(Fly).
updateRoom(3, 1) :- !.

updateRoom(X, Y) :-
	core:debugData(4, u(X, Y)).

afterUpdateRoom(X, Y) :-
	core:debugData(5, au(X, Y)).

openRoom(X, Y) :-
	core:debugData(6, opr(X, Y)).



closeRoom(X, Y) :-
	core:debugData(7, clr(X, Y)).

outRoom(X, Y) :-
	core:debugData(8, our(X, Y)).

collideObject(100, 1) :-
	update:register(player, player:playStun),
	core:debugData(9, coll(100, 1)).


objectsSetNames :-
	map:objFind(id8000, D),
	map:objName(D, 'DIAMOND'),
	map:objFind(id2000, L),
	map:objName(L, 'LIVES'),
	map:objFind(id9000, F),
	map:objName(F, 'FLIPPERS'),
	map:objFind(id9001, S),
	map:objName(S, 'SCUBA'),
	core:dlog('objectsSetNames\n').


beginNewGame :-
	game:state(game),
	game:unpause,
	player:setDisable(false),
	playerBeginX(X),
	playerBeginY(Y),
	player:setPos(X, Y),
	core:musicFade(0, 1),
	musicDefault(DefMusic),
	core:musicPlay(DefMusic),
	color(magenta, C1),
	color(green, C2),
	update:register(ui, message:pop),
	message:msg(14, 6, 'HELLO {c:0xffffff00}WORLD\nNEW LINE!', C1, C2).


%actionObject(Id) :-
%	core:dl(action(Id)).

useObject(Id, Idx) :-
	core:dl(use(Id, Idx)).


actionObject(1000) :-
	map:objFind(id1000, Idx),
	update:register(ui, message:pop),
	obj:status(Idx, Status),
	(   Status =:= 0
	->  obj:status(Idx, 1),
	    message:qmsg(1, 2, 2, '"DORA SEEMS\nTO NEED SOMETHING."')
	;   message:qmsg(1, 2, 2, '"DORA NEEDS SOME LEAFS"')).


actionObject(1001) :-
	update:register(ui, action:useObject),
	update:register(ui, menu:openDialogInventory),
	update:register(ui, message:pop),
	message:qmsg(1, 6, 5, '"I NEED LEAFS!"').

actionObject1 :-
	message:pop,
	update:register(ui, action:useObject),
	menu:openDialogiInventory.



% IN: int; death; cause of death
% Returns the player death message. Called by PlayerLoseLife().
% Declare more death defines in gamedef.gs (like DEATH_INFIRE, or DEATH_BATS)
% and set them to hurt and kill objects or just set them in the player's
% P_DEATH property, then return specific messages in this callback,
% for each cacuse of death .

deathMessage(-1, '') :- !.
deathMessage(water, 'YOU HAVE DROWNED!') :- !.
deathMessage(_, 'YOU HAVE DIED!').






































