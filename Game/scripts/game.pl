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
property(isViewportMode, setViewportMode, false).
property(viewportX, setViewportX, 0).
property(viewportY, setViewportY, 0).
property(isViewportFlipX, setViewportFlipX, false).
property(isViewportFlipY, setViewportFlipY, false).
property(isFullMaterialMap, setFullMaterialMap, false).

isPaused(X) :- paused -> X = true; X = false.
setPaused(true) :- pause.
setPaused(false) :- unpause.

isViewportMode(X) :- viewportMode -> X = true; X = false.
setViewportMode(true) :- setViewportMode.
setViewportMode(false) :- unsetViewportMode.

isViewportFlipX(X) :- viewportFlipX -> X = true; X = false.
setViewportFlipX(true) :- setViewportFlipX.
setViewportFlipX(false) :- unsetViewportFlipX.

isViewportFlipY(X) :- viewportFlipY -> X = true; X = false.
setViewportFlipY(true) :- setViewportFlipY.
setViewportFlipY(false) :- unsetViewportFlipY.

isFullMaterialMap(X) :- fullMaterialMap -> X = true; X = false.
setFullMaterialMap(true) :- setFullMaterialMap.
setFullMaterialMap(false) :- unsetFullMaterialMap.

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
roomSize(W, H) :- map:roomW(W), map:roomH(H).

updateRoom(2, 1) :-
	ai:updateTrain(train).
updateRoom(2, 1) :- !.


updateRoom(3, 1) :-
	ai:updateSpider(spider),
	ai:updateChainLink(spider_link),
	ai:updateTrain(fly).
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

collideObject(collide_handler, 1) :-
	update:register(player, player:stun),
	core:debugData(9, coll(ch, 1)).


beginNewGame :-
	game:state(game),
	game:unpause,
	player:setDisable(false),
	playerBeginX(X),
	playerBeginY(Y),
	player:setPos(X, Y),
	music:fade(0, 1),
	musicDefault(DefMusic),
	music:play(DefMusic),
	color(magenta, C1),
	color(green, C2),
	update:register(ui, message:pop),
	message:msg(4, 6, 'HELLO\n{c:ffffffff}{t:1236 0 0}    \n {c:ffffff00}WORLD\n', C1, C2).

%actionObject(Id) :-
%	core:dl(action(Id)).

useObject(Id1, Id2) :-
	core:dl(use(Id1, Id2)),
	findall(Br-V, brush:get(Br, bubble, V), Brs),
	core:dl(Brs).


actionObject(dylan) :-
	brush:find(dylan, Obj),
	update:register(ui, message:pop),
	(   brush:get(Obj, status, st1)
	->  brush:set(Obj, status, st2),
	    message:qmsg(1, 2, 2, '"DORA SEEMS\nTO NEED SOMETHING."')
	;   message:qmsg(1, 2, 2, '"DORA NEEDS SOME LEAFS"')).


actionObject(dora) :-
	update:regPop(ui, action:useObject),
	update:register(ui, menu:openDialogInventory),
	update:register(ui, message:pop),
	message:qmsg(1, 6, 5, '"I NEED LEAFS!"').

actionObject1 :-
	message:pop,
	update:register(ui, action:useObject),
	menu:openDialogiInventory.



deathMessage(none, '') :- !.
deathMessage(water, 'YOU HAVE DROWNED!') :- !.
deathMessage(_, 'YOU HAVE DIED!').






































