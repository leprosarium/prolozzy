:-module(start, [load/0,
		 restart/0]).

setMatProps(Mat, Idx, Dens, Color) :-
	core:materialDensity(Idx, Dens),
	core:materialColor(Idx, Color),
	core:dlog(setDens(Mat, Dens, Color)),core:dlog("\n").

initMaterials :-
	forall(def:material(Mat, Idx, Dens, Color), setMatProps(Mat, Idx, Dens, Color)).

initTitle :-
	gamedef:gameTitle(Title),
	game:setName(Title).


load:-
	initTitle,
	initMaterials,
	core:ticktime(Start),
	tile:load("data\\tiles\\loading\\"),
	update:register(init, start:load(Start)).

load(Start) :-
	tile:unload,
	tile:load("data\\tiles\\"),
	tile:count(Cnt),
	load(Cnt, Start).

load(0, _) :-
	game:command(exit).
load(_, Start) :-
	music:load("data\\music"),
	sample:load("data\\samples"),
	core:fontLoad("data\\fonts"), !,
	core:ticktime(End),
	loaded(Start, End).
load(_, _) :-
	game:command(exit).


loaded(Start, End) :-
	update:register(init, game:restart),
	gamedef:loadingTime(LoadingTime),
	(  End - Start >= LoadingTime * 1000
	-> update:waitTime(init, LoadingTime)
	;  true).

%--------------------------

resetStaticVars :-
	forall((game:property(_, Setter, Def); game:property(Setter, Def)), (Set =.. [Setter, Def], call(game:Set))).

initPlayer :-
	gamedef:playerLayer(L),
	player:setLayer(L),
	inventory:clear.

restart :-
	game:pause, % start paused
	resetStaticVars,
%	game:setViewportMode(true),
	initPlayer,
	sample:stopAll,
	reloadMap.


reloadMap:-
	File = "data\\map\\dizzy.pmp",
	map:reset,
	setup_call_cleanup(
	    catch(core:open_resource(File, S), Ex, (core:dl(Ex), fail)), file:loadTerms(S), close(S)),
	game:objectsSetNames,
	menu:main.

	% INTRO
 % update:register(game:beginNewGame). % use this to skip the MainMenu, for quick testing


reloadMap :-
	game:command(exit).



