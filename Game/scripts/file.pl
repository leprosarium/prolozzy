:- module(file, [saveGame/1,
		 loadGame/1
		]).


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
	saveMusic(S),
	saveUpdate(player, S),
	saveState(S).

saveOk :-
	sample:play(coin),
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
	forall(map:brush(Br), saveObj(Br, F)).

saveObj(Obj, F) :-
	brush:getID(Obj, Id),
	brush:get(Obj, Props),
	writet(F, brush:set(Id, Props)).

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

saveMusic(F) :-
	music:playing(M),
	music:position(P),
	writet(F, sample:stopAll),
	writet(F, music:stop),
	writet(F, music:fade(0, 3)),
	writet(F, music:play(M, P)).

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
	sample:play(respawn),
	def:color(yellow, Color),
	update:push(yes),
	dialog:openMessage('LOAD SUCCESSFUL!', Color).

loadFail :-
	def:color(red, Color),
	update:register(ui, game:command(exit)),
	update:push(no),
	dialog:openMessage('FILE ERROR!', Color).
















