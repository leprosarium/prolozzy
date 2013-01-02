:- module(file, [saveGame/1,
		 loadGame/1
		]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
% file.gs
% Save and load game operations.
% Static brushes with ids are saved and loaded automated, from the .brs exported file.
% Advanced users can also save and load global script variables.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

writet(S, T) :-
	write_term(S, T,
		   [ attributes(ignore),
%		     ignore_ops(true),
		     quoted(true),
		     partial(true)
		   ]),
	format(S, '.~n', []).

saveGame(File) :-
	setup_call_cleanup(open(File, write, S),
			   (   saveGameToStream(S)
			   ->  saveOk
			   ;   saveFail),
			   close(S)).

saveGameToStream(S) :-
	saveGameVars(S),
	saveInv(S),
	saveObjects(S),
	savePlayer(S),
	saveStaticBrushes(S),
	saveMusic(S),
	saveUpdate(player, S),
	saveState(S).

	%saveRoomProperties(S),
	%saveUserData(S).

saveOk :-
	core:samplePlay(coin),
	def:color(yellow, Color),
	update:push(yes),
	dialog:openMessage('SAVE SUCCESSFUL!', Color).

saveFail :-
	def:color(red, Color),
	update:push(no),
	dialog:openMessage('FILE ERROR!', Color).

saveGameVars(F) :-
	forall( game:property(Name, SetName, _),
	       (Pr =.. [Name, Val],
		call(game:Pr),
		SetPr =.. [SetName, Val],
		writet(F, game:SetPr))),
	forall( game:property(Name, _),
	       (Pr =.. [Name, Val],
		call(game:Pr),
		writet(F, game:Pr))).

saveInv(F) :-
	inventory:inv(Inv),
	writet(F, inventory:clear),
	forall(util:member(I, Inv), writet(F, inventory:add(I))).

saveObjects(F):-
	map:objCount(Count),
	once(saveObj(0, Count, F)).

saveObj(Count, Count, _).
saveObj(I, Count, F) :-
	forall(obj:var(I, Var, Val), writet(F, obj:var(I, Var, Val))),
	II is I + 1,
	saveObj(II, Count, F).

savePlayer(F) :-
	forall( player:property(Name, SetName),
	       (Pr =.. [Name, Val],
		call(player:Pr),
		SetPr =.. [SetName, Val],
		writet(F, player:SetPr))),
	forall( player:property(Name),
		(Pr =.. [Name, Val],
		 call(player:Pr),
		 writet(F, player:Pr))).

saveUpdate(Key, F) :-
	update:get(Key, Goals),
	writet(F, update:clear(Key)),
	writet(F, update:register(Key, Goals)).

saveState(F) :-
	game:state(State),
	writet(F, game:state(State)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
% IN: int; file; file handler
% OUT: int; 0=fail, 1=success
% Saves brushes from the brushes ids file exported from editor in the dizzy.brs file.
% Called from the SaveGame() function.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
saveStaticBrushes(F) :-
	setup_call_cleanup(open('data/map/dizzy.brs', read, Brs),
			   saveStaticBrushes(F, Brs),
			   close(Brs)).

saveStaticBrushes(F, Brs) :-
	read_term(Brs, Term, []),
	saveStaticBrushes(Term, F, Brs).
saveStaticBrushes(end_of_file, _, _).
saveStaticBrushes(Id, F, Brs) :-
%	number(Id),
	saveBrush(F, Id),
	saveStaticBrushes(F, Brs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
% IN: int; file; file handler
% IN: int; id; brush id
% OUT: int; 0=fail, 1=success
% Saves a brush into a file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
saveBrush(F, Id) :-
	map:brushFind(Id, B),
	forall(brush:var(B, Var, Val), writet(F, brush:var(B, Var, Val))).

saveMusic(F) :-
	core:musicPlaying(M),
	core:musicPosition(P),
	writet(F, core:sampleStopAll),
	writet(F, core:musicStop),
	writet(F, core:musicFade(0, 3)),
	writet(F, core:musicPlay(M, P)).

loadGame(File) :-
	setup_call_cleanup(open(File, read, S),
			   (   loadTerms(S)
			   ->  loadOk
			   ;   loadFail),
			   close(S)).

loadTerms(S) :-
	catch(read_term(S, Term, []), E, (core:dl(readError(E)), fail)),
	loadTerms(Term, S).
loadTerms(end_of_file, _) :- !.
loadTerms(Term, S) :-
	catch(call(Term), E, (core:dl(readError(E)), fail)),
	loadTerms(S), !.

loadOk :-
	game:command(refresh),
	core:objPresentGather, % refresh present list
	core:samplePlay(respawn),
	def:color(yellow, Color),
	update:push(yes),
	dialog:openMessage('LOAD SUCCESSFUL!', Color).

loadFail :-
	def:color(red, Color),
	update:register(ui, game:command(exit)),
	update:push(no),
	dialog:openMessage('FILE ERROR!', Color).








