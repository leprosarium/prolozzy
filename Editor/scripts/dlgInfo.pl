:-module(dlgInfo, [mapFile/1,
		   setMapFile/1]).

:-recorda(mapFile, "noname.pmp").

mapFile(X) :-
	recorded(mapFile, X).
setMapFile(F) :-
	recorded(mapFile, _, Ref),
	erase(Ref),
	recorda(mapFile, F).
