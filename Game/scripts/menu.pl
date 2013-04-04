:- module(menu, [main/0
		]).


% Deals with main menu, game menu, inventory menu and other related menus
% The MainMenu is a looping game sequence called when the game starts, from start.gs
% It usually shows a cover screen and it allows players to start a new game.
% It can also open a dialog menu, allowing players to load a saved game, read info, exit, etc.
% The game menu pauses the game and open a dialog menu allowing player to save, load, or restart the current game.
% The inventory pauses the game and allows the player to select an item from the inventory.
% Advanced users can rewrite menus behaiour, to match their game's needs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
% MainMenu
% For classic game menu, with cover screen and optional attract mode.
% Uses KEY_ACTION to begin game and KEY_MENU to open dialog menu.
% Set MAINMENU_ATTRACT to 1 in gamedef.gs if you want to allow attract mode.
% For attract mode, your mainmenu room must have some valid content.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/

main :-
	game:unpause,
	core:musicStop,
	core:musicFade(0, 0),
	gamedef:musicDefault(MD),
	core:musicPlay(MD),
	cover.

cover :-
	game:state(cover),
	player:setDisable(true),
	game:roomPos(X, Y),
	roomNames:set(X, Y, name, 'PRESS ACTION TO START'),
	coverLoop.

coverLoop :-
	coverLoop(0).

coverLoop(Frame) :-
	update:register(ui, menu:cover(Frame)).

cover(Frame) :-
	NewFrame is Frame + 1,
	NewFrame < 474,
	(   util:getKeyHit(action)
	->  menu:mainMenuClose(start)
	;   (   util:getKeyHit(menu)
	    ->	openDialogMainMenu
	    ;   coverLoop(NewFrame))).
cover(_) :-
	gamedef:mainMenuAttract(1) -> attract ; coverLoop.

attract :-
	game:state(attract),
	gamedef:playerMainMenuX(MMX),
	gamedef:playerMainMenuY(MMY),
	player:setPos(MMX, MMY),
	player:setDisable(false),
	game:roomPos(X, Y), roomNames:set(X, Y, name, '...ATTRACT MODE...'),
	attractLoop.

attractLoop :-
	attractLoop(0).

attractLoop(Frame) :-
	update:register(ui, menu:attract(Frame)).

attract(Frame) :-
	NewFrame is Frame + 1,
	NewFrame < 474,
	attrDir,
	attrDirKeys,
	attrJump,
	attrSpin,
	game:keysHit(0),
	attractLoop(NewFrame).
attract(_) :-
	cover.


attrDir :-
	random(10) =:= 1,
	player:status(walk),
	(   random(2) =:= 1 ->
	player:setDir(-1);
	player:setDir(1)).
attrDir.

attrDirKeys :-
	player:dir(Dir),
	(   Dir =:= -1 ->
	util:setKey(left);
	util:setKey(right)).

attrJump :-
	random(20) =:= 0 ->
	util:setKey(jump);
	true.

attrSpin :-
	random(20) =:= 0,
	player:status(walk),
	(   random(2) =:= 1 ->
	player:enterSpin(-1);
	player:enterSpin(1)).

attrSpin.


mainMenuClose(load) :-
	util:clearKeys.
mainMenuClose(nothing) :-
	coverLoop.
mainMenuClose(start) :-
	util:clearKeys,
	player:setDisable(false),
	player:enterIdle,
	game:beginNewGame.

% Opens the MainMenu dialog.
% Uses DialogMainMenu() function for dialog creation.
% Returns 1 if game must start with intro, 2 if game was loaded, 0 otherwise

openDialogMainMenu :-
	game:pause,
	update:regPop(ui, menu:mainMenuClose),
	update:regPop(ui, menu:openDialogMainMenuFinish),
	dialog:runSelect2(menu:dialogMainMenum, start).
openDialogMainMenuReturn(Ret) :-
	game:unpause,
	update:push(Ret).

openDialogMainMenuFinish(start) :- !,
	openDialogMainMenuReturn(start).
openDialogMainMenuFinish(load) :- !,
	update:regPop(ui, menu:openDialogMainMenuFinish1),
	openDialogFiles(load).

openDialogMainMenuFinish(options) :- !,
	update:register(ui, menu:openDialogMainMenuReturn(nothing)),
	openDialogOptions.

openDialogMainMenuFinish(credits) :- !,
	update:register(ui, menu:openDialogMainMenuReturn(nothing)),
	openDialogCredits.

openDialogMainMenuFinish(exit) :- !,
	update:regPop(ui, menu:openDialogMainMenuFinish4),
	dialog:openQuestion('EXIT GAME ?', no).

openDialogMainMenuFinish(_) :- !,
	openDialogMainMenuReturn(nothing).

openDialogMainMenuFinish1(yes) :- !, openDialogMainMenuReturn(load).
openDialogMainMenuFinish1(no) :- !, openDialogMainMenuReturn(nothing).
openDialogMainMenuFinish4(yes) :- !, game:command(exit), openDialogMainMenuReturn(nothing).
openDialogMainMenuFinish4(_) :- !, openDialogMainMenuReturn(nothing).

dialogMainMenum(state, start, down, load).
dialogMainMenum(state, start, up, exit).
dialogMainMenum(state, start, right, load).
dialogMainMenum(state, start, left, exit).
dialogMainMenum(state, load, down, options).
dialogMainMenum(state, load, up, start).
dialogMainMenum(state, load, right, options).
dialogMainMenum(state, load, left, start).
dialogMainMenum(state, options, down, credits).
dialogMainMenum(state, options, up, load).
dialogMainMenum(state, options, right, credits).
dialogMainMenum(state, options, left, load).
dialogMainMenum(state, credits, down, exit).
dialogMainMenum(state, credits, up, options).
dialogMainMenum(state, credits, right, exit).
dialogMainMenum(state, credits, left, options).
dialogMainMenum(state, exit, down, start).
dialogMainMenum(state, exit, up, credits).
dialogMainMenum(state, exit, right, start).
dialogMainMenum(state, exit, left, credits).
dialogMainMenum(state, State, action, return(State)).
dialogMainMenum(state, _, menu, return(none)).
dialogMainMenum(final, return(State), State).


% IN: int; select; default selection
% Dialog creation function used by OpenDialogMainMenu().
dialogMainMenum(draw, State) :-
	Header = '{a:center}GAME MENU\n\n{c:0xff0000}',
	dialogMainMenum2(State, [start/'START', load/'LOAD', options/'OPTIOS', credits/'CREDITS', exit/'EXIT'], MenuText),
	atom_concat(Header, MenuText, Text),
	def:color(dialog, Color),
	gamedef:fontDefault(Font),
	dialog:fitCenter(dialog(_, _, style(Font, Color), Text), Dialog),
	dialog:push(Dialog).

dialogMainMenum2(State, [H], Text) :-
	dialogMainMenum21(State, H, Text).
dialogMainMenum2(State, [H|T], Text) :-
	dialogMainMenum21(State, H, TextHead),
	dialogMainMenum2(State, T, TextTail),
	format(atom(Text), '~a\n~a', [TextHead, TextTail]), !.

dialogMainMenum21(State, State/Elem, Text) :-
	format(atom(Text), '{f:1}~a{f:0}', Elem), !.
dialogMainMenum21(_, _/Elem, Elem).


dialogMainMenu(Select, Cnt, [H], Text) :-
	dialogMainMenu1(Select, Cnt, H, Text).
dialogMainMenu(Select, Cnt, [H|T], Text) :-
	dialogMainMenu1(Select, Cnt, H, TextHead),
	Cnt1 is Cnt + 1,
	dialogMainMenu(Select, Cnt1, T, TextTail),
	format(atom(Text), '~a\n~a', [TextHead, TextTail]), !.

dialogMainMenu1(Select, Select, Elem, Text) :-
	format(atom(Text), '{f:1}~a{f:0}', Elem), !.
dialogMainMenu1(_, _, Elem, Elem).



% Opens the Credits dialog with the authors information.
openDialogCredits :-
	gamedef:gameAuthor(Autor),
	format(atom(Text), '{a:center}CREDITS\n\n{c:0xffffffff}\c
	GAME CREATED BY\n\c
	{c:0xff0080ff}~a{c:0xffffffff}\n\n\c
	ORIGINAL DIZZY GAMES BY\n\c
	{c:0xff0080ff}OLIVER TWINS\nAND CODEMASTERS{c:0xffffff00}\n', Autor),
	dialog:openMessage(Text).


% Opens the Options dialog.
% Uses DialogOptions() function for dialog creation.
openDialogOptions :-
	game:pause,
	update:register(ui, game:unpause),
	openDialogOptionsLoop(fx).
openDialogOptionsLoop(State) :-
	update:regPop(ui, menu:openDialogOptionsSelect0),
	dialog:runSelect2(menu:dialogOption, State).

openDialogOptionsSelect0(Select) :-
	dialog:popAll,
	openDialogOptionsSelect(Select).

openDialogOptionsSelect(fx) :-
	getOpt('dizzy.ini', 'AUDIO', 'volfx', 100, Vol),
	addVol(Vol, Vol1),
	core:ini('dizzy.ini', 'AUDIO', 'volfx', Vol1),
	sample:volume(Vol1),
	sample:play(beep2),
	openDialogOptionsLoop(fx).
openDialogOptionsSelect(music) :-
	getOpt('dizzy.ini', 'AUDIO', 'volmusic', 100, Vol),
	addVol(Vol, Vol1),
	core:ini('dizzy.ini', 'AUDIO', 'volmusic', Vol1),
	core:musicVolume(Vol1),
	openDialogOptionsLoop(music).
openDialogOptionsSelect(control) :-
	openDialogControls.
openDialogOptionsSelect(_).

getOpt(File, Section, Key, _, Val) :-
	core:ini(File, Section, Key, Val), !.
getOpt(_File, _Section, _Key, Def, Def).

addVol(VolA, Vol1) :-
	atom_number(VolA, Vol),
	Vol2 is Vol + 10,
	(Vol2 > 100 -> Vol1 = 0; Vol1 = Vol2).


dialogOption(state, fx, left, return).
dialogOption(state, fx, up, return).
dialogOption(state, fx, right, music).
dialogOption(state, fx, down, music).
dialogOption(state, music, left, fx).
dialogOption(state, music, up, fx).
dialogOption(state, music, right, control).
dialogOption(state, music, down, control).
dialogOption(state, control, left, music).
dialogOption(state, control, up, music).
dialogOption(state, control, right, return).
dialogOption(state, control, down, return).
dialogOption(state, return, left, control).
dialogOption(state, return, up, control).
dialogOption(state, return, right, fx).
dialogOption(state, return, down, fx).
dialogOption(state, State, action, return(State)).
dialogOption(state, _State, menu, return(none)).
dialogOption(final, return(State), State).


dialogOption(draw, State) :-
	getOpt('dizzy.ini', 'AUDIO', 'volfx', 100, Volfx),
	getOpt('dizzy.ini', 'AUDIO', 'volmusic',100, Volmusic),
	Header = '{a:center}GAME OPTIONS\n\n{c:0xff0000}',
	format(atom(Text0), 'FX VOLUME ~a%', Volfx),
	format(atom(Text1), 'MUSIC VOLUME ~a%', Volmusic),
	dialogMainMenum2(State, [fx/Text0, music/Text1, control/'CONTROLS', return/'RETURN'], MenuText),
	atom_concat(Header, MenuText, Text),
	gamedef:fontDefault(Font),
	def:color(dialog, Color),
	Dialog = dialog(pos(X, Y), size(W, H), style(Font, Color), Text),
	W is 18 * 8, % keep large and fixed
	dialog:textH(Dialog, H1),
	H is H1 + 8, % add some space
	game:roomSize(RoomW, RoomH),
	X is (RoomW - W) // 2,
	Y is (RoomH - H) // 2,
	dialog:push(Dialog).

% Opens the Controls dialog.
openDialogControls :-
	Text = '{a:center}\c
{c:0xffff00}GAME CONTROLS\n\n{a:left}\c
{c:0xffffff}Z{c:0xffff00} OR {c:0xffffff}LEFT{c:0xffff00}   - MOVE LEFT\n\c
{c:0xffffff}X{c:0xffff00} OR {c:0xffffff}RIGHT{c:0xffff00}  - MOVE RIGHT\n\c
{c:0xffffff}SPACE{c:0xffff00} OR {c:0xffffff}UP{c:0xffff00} - JUMP\n\c
{c:0xffffff}ENTER{c:0xffff00}       - ACTION\n\c
{c:0xffffff}ESCAPE{c:0xffff00} OR {c:0xffffff}Q{c:0xffff00} - MENU\n\n\c
{c:0xffffff}F9{c:0xffff00}  - MUTE ALL SOUND\n\c
{c:0xffffff}F10{c:0xffff00} - TOGGLE SCREEN\n',
	dialog:openMessage(Text).


% Opens the GameMenu dialog.
% Uses DialogGameMenu() function for dialog creation.
openDialogGameMenu :-
	game:pause,
	update:register(ui, game:unpause),
	update:regPop(ui, menu:openDialogGameMenuFinish),
	dialog:runSelect(menu:dialogGameMenu, 6, 0).


openDialogGameMenuFinish(1) :-
	update:register(ui, update:pop),
	openDialogFiles(save), !.
openDialogGameMenuFinish(2) :-
	update:register(ui, update:pop),
	openDialogFiles(load), !.

openDialogGameMenuFinish(3) :-
	menu:openDialogOptions, !.

openDialogGameMenuFinish(4) :-
	update:regPop(ui, menu:openDialogGameMenuFinishCmd(game:restart)),
	dialog:openQuestion('RESTART GAME ?', no), !.

openDialogGameMenuFinish(5) :-
	update:regPop(ui, menu:openDialogGameMenuFinishCmd(game:command(exit))),
	dialog:openQuestion('EXIT GAME ?', no), !.

openDialogGameMenuFinish(_).

openDialogGameMenuFinishCmd(yes, Cmd) :- !, call(Cmd).
openDialogGameMenuFinishCmd(_, _).


% IN: int; select; default selection
% Dialog creation function used by OpenDialogGameMenu().
dialogGameMenu(Select) :-
	Header = '{a:center}GAME PAUSED\n\n{c:0xff0000}',
	dialogMainMenu(Select, 0, ['RESUME', 'SAVE', 'LOAD', 'OPTIOS', 'RESTART', 'EXIT'], MenuText),
	atom_concat(Header, MenuText, Text),
	def:color(dialog, Color),
	gamedef:fontDefault(Font),
	dialog:fitCenter(dialog(_, _, style(Font, Color), Text), Dialog),
	dialog:push(Dialog).

% [IN]: int; defaultexit=0; default selection, 0=top item (last picked), 1=exit without dropping
% OUT: int; object's index, or -1 for exiting without selecting an item
% Opens the Inventory dialog and allows the player to choose one item for use (or drop).
% Internally, the selection goes from [0 to InventoryCount()] (last selection means return without dropping).
% Uses DialogInventory() function for dialog creation.

openDialogInventory :-
	openDialogInventory(0).
openDialogInventory(Def) :-
	game:pause,
	update:regPop(ui, menu:openDialogInventoryFinish),
	(   Def = exit
	->  dialog:runSelect2(menu:dialogInventory, exit)
	;   (   inventory:find(Idx, _)
	    ->	dialog:runSelect2(menu:dialogInventory, Idx)
	    ;	dialog:runSelect2(menu:dialogInventory, exit))).

openDialogInventoryFinish(exit) :- !, openDialogInventoryReturn(-1).
openDialogInventoryFinish(Idx) :- !, openDialogInventoryReturn(Idx).

openDialogInventoryReturn(ObjIdx) :-
	dialog:popAll,
	game:unpause,
	update:push(ObjIdx).

dialogInventory2([exit], _Idx, action, return(exit)) :-!.
dialogInventory2(Inv, Idx, Action, NewIdx) :-
	dialogInventoryRight(Inv, Idx, Action, NewIdx).
dialogInventory2(Inv, Idx, Action, NewIdx) :-
	dialogInventoryLeft(Inv, Idx, Action, NewIdx).
dialogInventoryNext(Inv, Idx, NextIdx) :-
	util:append(_, [Idx, NextIdx|_End], Inv);
	util:append([NextIdx|_], [Idx], Inv).

dialogInventoryRight(Inv, Idx, Action, NewIdx) :-
	dialogInventoryNext(Inv, Idx, NewIdx),
	(   Action = right
	;   Action = down).
dialogInventoryLeft(Inv, Idx, Action, NewIdx) :-
	dialogInventoryNext(Inv, NewIdx, Idx),
	(   Action = left
	;   Action = up).
% IN: int; select; default selection
% Dialog creation function used by OpenDialogInventory().

dialogInventory(state, Idx, Action, NewIdx) :-
	inventory:inv(Inv),
	InvR = [exit|Inv],
	dialogInventory2(InvR, Idx, Action, NewIdx).

dialogInventory(state, Idx, action, return(Idx)).
dialogInventory(start, _, menu, return(exit)).
dialogInventory(final, return(Idx), Idx).


dialogInventory(draw, State) :-
	dialog:popAll,
	Header = '{a:center}YOU ARE CARRYING\n\n{c:0xff0000}',

	% push elements names in reverse order (last is first)
	dialogInventoryText(State, Text),
	dialogMainMenum2(State, [exit/'\nEXIT AND DON\'T DROP'], TextExit),

	format(atom(Text2), '~a~a~a', [Header, Text, TextExit]),

	% construct dialog
	def:color(dialog, Color),
	gamedef:fontDefault(Font),
	Dialog = dialog(pos(X, Y), size(W, H), style(Font, Color),Text2),
	% compute position considering the tooltip dialog
	Border = 16,
	dialog:textWH(Dialog, Wt, Ht),
	W is Wt + 8,   % add some space
	H is Ht + 8,   % add some space
	game:roomSize(RoomW, RoomH),
	X is (RoomW - W) // 2,
	Y is (RoomH - H - 40) //2, % tooltip dialog (40)
	dialog:push(Dialog),
	Y2 is Y + H + Border,
	dialogTooltip(Y2).

dialogInventoryText(State, Text) :-
	findall(Idx/Name, (inventory:find(Idx, _), map:objName(Idx, Name)), Names),
	dialogMainMenum2(State, Names, Text), !.
dialogInventoryText(_State, '{c:0xffffffff}N O T H I N G\n').





% tooltip dialog
dialogTooltip(Y2) :-
	def:color(white, Color),
	gamedef:fontDefault(Font),
	Dialog = dialog(pos(X, Y2), size(W, H), style(Font, Color), '{c:0xff00ffff}CHOOSE ITEM TO\nUSE OR DROP'),
	dialog:textWH(Dialog, Wt, Ht),
	W is Wt + 8,   % add some space
	H is Ht + 8,   % add some space
	game:roomW(RoomW),
	X is (RoomW - W) // 2,
	dialog:push(Dialog).


%str g_dialogfiles_title;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
% IN: int; mode; 0=load mode, 1=save mode
% Opens the game files dialog for load or for save.
% Uses DialogFiles() function for dialog creation.
% Returns 1 if game was loaded or saved, 0 otherwise
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
openDialogFiles(Mode) :-
	openDialogFilesTitle(Mode, Title),
	update:regPop(ui, menu:openDialogFilesSelect(Mode)),
	dialog:runSelect2(menu:dialogFiles(Title), 0).

openDialogFilesTitle(load, 'LOAD GAME').
openDialogFilesTitle(save, 'SAVE GAME').

openDialogFilesSelect(exit, _Mode) :- !, update:push(no).

openDialogFilesSelect(Select, Mode) :- !,
	Num is Select + 1,
	format(atom(File), 'save~a.gam', Num),
	openDialogFilesSelectFile(Mode, File).

openDialogFilesSelectFile(load, File) :-
	exists_file(File)
	-> file:loadGame(File)
	; update:push(no), dialog:openMessage( 'SLOT IS EMPTY', 0xffff0000 ).

openDialogFilesSelectFile(save, File) :-
	exists_file(File)
	-> update:regPop(ui, menu:openDialogFilesSelectSave(File)), dialog:openQuestion('OVERWRITE FILE ?',no)
	; file:saveGame(File).
openDialogFilesSelectSave(yes, File) :- !, file:saveGame(File).
openDialogFilesSelectSave(_, _) :- !, update:push(no).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
% IN: int; select; default selection
% Dialog creation function used by OpenDialogFiles().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
dialogFiles(state, exit, right, 0).
dialogFiles(state, exit, down, 0).
dialogFiles(state, 0, left, exit).
dialogFiles(state, 0, up, exit).

dialogFiles(state, 0, right, 1).
dialogFiles(state, 0, down, 1).
dialogFiles(state, 1, left, 0).
dialogFiles(state, 1, up, 0).

dialogFiles(state, 1, right, 2).
dialogFiles(state, 1, down, 2).
dialogFiles(state, 2, left, 1).
dialogFiles(state, 2, up, 1).

dialogFiles(state, 2, right, exit).
dialogFiles(state, 2, down, exit).
dialogFiles(state, exit, left, 2).
dialogFiles(state, exit, up, 2).
dialogFiles(state, S, action, return(S)).
dialogFiles(state, _, menu, return(exit)).
dialogFiles(final, return(S), S).


dialogFiles(draw, Select, Title) :-
	dialogFiles2(Select, 3, Body),
	(Select = exit -> Exit = '{f:1}RETURN{f:0}' ; Exit = 'RETURN'),
	format(string(Text), '{a:center}~a\n\n{c:0xff0000}~a\n\n~a', [Title, Body, Exit]),
	def:color(dialog, Color),
	gamedef:fontDefault(Font),
	dialog:fitCenter(dialog(_, _, style(Font, Color), Text), Dialog),
	dialog:push(Dialog).



dialogFiles2(Select, M, Text) :-
	dialogFilesList(M, [], List),
	dialogMainMenum2(Select, List, Text).
dialogFilesList(0, List, List) :- !.
dialogFilesList(I, CurList, List) :-
	format(atom(File), 'save~a.gam', I),
	(  exists_file(File)
	-> time_file(File, TimeStamp),
	   format_time(string(Time), '%d.%m.%y %T', TimeStamp),
	   format(atom(Text), 'FILE ~d ~s', [I, Time])
	;  Text = ' EMPTY  SLOT        '),
	NI is I - 1,
	dialogFilesList(NI, [NI/Text|CurList], List).























