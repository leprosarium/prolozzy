:-module(dialog, [ empty/0,
		   push/1,
		   pop/0,
		   popAll/0,
		   pos/3,
		   size/3,
		   color/2,
		   font/2,
		   text/2,
		   textW/2,
		   textH/2,
		   draw/1,
		   drawAll/0,
		   fitAt/3,
		   fitCenter/2,
		  run/0,
		  openMessage/1,
		  openMessage/2,
		  runSelect/2,
		  runSelect/3,
		  runSelect/4,
		  openQuestion/1,
		  openQuestion/2]).

empty :-
	\+ recorded(dialogStack, _).

push(Dialog) :-
	recorda(dialogStack, Dialog).

pop :-
	recorded(dialogStack, _Dialog, Ref),
	erase(Ref), !.

popAll :-
	pop,
	popAll.
popAll.


textW(dialog(_, _, style(Font, _), Text), Width) :-
	core:hudFont(Font),
	core:hudGetTextWidth(Text, Width).

textH(dialog(_, _, style(Font, _), Text), Height) :-
	core:hudFont(Font),
	core:hudGetTextHeight(Text, Height).

textWH(dialog(_Pos, _Size, style(Font, _Color), Text), Width, Height) :-
	core:hudFont(Font),
	core:hudGetTextWidth(Text, Width),
	core:hudGetTextHeight(Text, Height).

pos(dialog(pos(X, Y), _, _, _), X, Y).
size(dialog(_, size(Width, Height), _, _), Width, Height).
color(dialog(_, _, style(_, Color), _), Color).
font(dialog(_, _, style(Font, _), _), Font).
text(dialog(_, _, _, Text), Text).


drawAll :-
	findall(D, recorded(dialogStack, D), L1),
	reverse(L1, L2),
	forall(member(D, L2), draw(D)).


% IN: int; idx; dialog index
% Draw a dialog with border and text.
% Should be called only from HandlerDrawHud().
% Do NOT abuse it !
draw(dialog(pos(Dx, Dy), size(W, H), style(Font, Color), Text)) :-
	game:viewX(Vx),
	game:viewY(Vy),
	X is Dx + Vx,
	Y is Dy + Vy,
	core:hudColor(Color),

	Boxid = 3, % border tile
	X1 is X - 16,
	Y1 is Y - 16,
	X2 is X + W,
	Y2 is Y + H,
	core:hudDrawTile( Boxid, rect(X1, Y1,16,16), rect(0,0,16,16), 0, 0),
	core:hudDrawTile( Boxid, rect(X, Y1, W,16), rect(16,0,8,16), 0, 0),
	core:hudDrawTile( Boxid, rect(X2, Y1,16,16), rect(24,0,16,16), 0, 0),

	core:hudDrawTile( Boxid, rect(X1, Y,16, H), rect(0,16,16,8), 0, 0),
	core:hudDrawTile( Boxid, rect(X, Y, W, H), rect(16,16,8,8), 0, 0),
	core:hudDrawTile( Boxid, rect(X2, Y, 16, H), rect(24,16,16,8), 0, 0),

	core:hudDrawTile( Boxid, rect(X1, Y2,16,16), rect(0,24,16,40), 0, 0),
	core:hudDrawTile( Boxid, rect(X, Y2, W,16), rect(16,24,8,40), 0, 0),
	core:hudDrawTile( Boxid, rect(X2, Y2,16,16), rect(24,24,16,40), 0, 0),

	core:hudFont(Font),
	Yt is Y + 4,
	core:hudDrawText(Font, rect(X, Yt, W, H), Text, 0).




% IN: int; x; horizontal coordinate in pixels
% IN: int; y; verical coordinate in pixels
% Sets position of the last dialog.
% Auto sets the dialog size to contain all the text.
% It prevents the dialog from getting out of the screen, if possible, by adjusting the dialog position.

fitAt(Dialog, pos(X, Y), dialog(pos(X2, Y2), size(W, H), Style, Text)) :-
	Dialog = dialog(_, _, Style, Text),
	textWH(Dialog, Wt, Ht),
	W is Wt + 8, % plus some space
	H is Ht + 8, % plus some space
	game:roomSize(Roomw, Roomh),
	Border = 16,
	(X + W + Border > Roomw ->
	X1 is Roomw - W - Border; X1 = X),
	(X1 < Border -> X2 = Border ; X2 = X1),
	(Y + H + Border > Roomh ->
	Y1 is Roomh - H - Border; Y1 = Y),
	(Y1 < Border -> Y2 = Border; Y2 = Y1).

% Auto sets position of the last dialog in the center of the screen.
% Auto sets the dialog size to contain all the text.
fitCenter(Dialog, dialog(pos(X, Y), size(W, H), Style, Text)) :-
	Dialog = dialog(_, _, Style, Text),
	textWH(Dialog, Wt, Ht),
	W is Wt + 8, % plus some space
	H is Ht + 8, % plus some space
	game:roomSize(Roomw, Roomh),
	X is (Roomw - W) // 2,
	Y is (Roomh - H) // 2.


% Runs a simple dialog, waiting until the MENU key or the ACTION key are pressed.
% Latent function.
run :-
	runUpdate.
runUpdate :-
	(  util:getKeyHit(menu);util:getKeyHit(action))
	-> util:clearKeys
	;  update:register(ui, dialog:runUpdate).

% IN: str; text; message text
% [IN]: int; color=COLOR_DIALOG; border color
% Opens a simple message dialog and runs it, waiting for MENU or ACTION key to be pressed.

openMessage(Text) :-
	def:color(dialog, Color),
	openMessage(Text, Color).

openMessage(Text, Color) :-
	game:pause,
	gamedef:fontDefault(Font),
	fitCenter(dialog(_, _, style(Font, Color), Text), Dialog),
	push(Dialog),
	update:register(ui, dialog:openMessagePop),
	run.

openMessagePop :-
	pop,
	game:unpause.


runSelect2(Module:Dialog, Begin) :-
	Dialog =.. [Functor|Args],
	runSelect2Update(dialog(Module, Functor, Args), Begin).
runSelect2(Dialog, State):-
	(   runSelect2Final(Dialog, State, Result)
	->  runSelect2Return(Result)
	;   runSelect2Trans(Dialog, State, Trans),
	    (   runSelect2Test(Trans, NewState)
	    ->  pop, runSelect2Update(Dialog, NewState)
	    ;   update:register(ui, dialog:runSelect2(Dialog, State)))).
runSelect2Return(Result) :-
	pop,
	util:clearKeys,
	update:push(Result).


runSelect2Update(Dialog,  State) :-
	Dialog = dialog(M, F, A),
	Cl =.. [F, draw, State| A],
	call(M:Cl),
	update:register(ui, dialog:runSelect2(Dialog, State)).

runSelect2Final(dialog(M, F, _A), State, Result) :-
	Cl =..[F, final, State, Result],
	call(M:Cl).

runSelect2Trans(dialog(M, F,_A), State, Trans) :-
	Cl =..[F, state, State, Event, NewState],
	findall(trans(Event, NewState), M:Cl, Trans).

runSelect2Test([], _) :- !, false.
runSelect2Test([trans(Event, NewState)|_], NewState) :-
	util:getKeyHit(Event), !.
runSelect2Test([_|Trs], NewState) :-
	runSelect2Test(Trs, NewState).


% IN: int; fid_dialog; id of the dialog function that creates a dialog
% IN: int; count; how many selections are available in the dialog
% [IN]: int; default=0; default selection when the dialog is opened
% [IN]: int; norefuse=0; if the dialog can be closed without choosing a selection
% OUT: int; selection index or -1 if dialog was refused and nothing was choosed
% Runs a multi-selection dialog.
% Uses LEFT and RIGHT (or UP and DOWN) keys to navigate and ACTION key to select.
% If norefuse, you can't press MENU key to close without selecting something.
% The dialog creation function (fid_dialog) receives the current selection index as a parameter.
% Latent function.
%
runSelect(Dialog, Count) :-
	runSelect(Dialog, Count, 0, false).
runSelect(Dialog, Count, Default) :-
	runSelect(Dialog, Count, Default, false).
runSelect(Functor, Count, Default, Norefuse) :-
	Last is Count - 1,
	runSelectUpdate(Functor, Last, Default, Norefuse).

runSelectUpdate(Functor, Last, Select, Norefuse) :-
	callName(Functor, Select,  Dialog),
	call(Dialog),
	update:register(ui, dialog:runSelectUpdateKey(Functor, Last, Select, Norefuse)).

runSelectUpdate2(Functor, Last, Select, Norefuse) :-
	dialog:pop,
	runSelectUpdate(Functor, Last, Select, Norefuse).

callName(M:F, Val, M:F2) :-
	callName(F, Val, F2), !.
callName(F, Val, Func) :-
	F =.. [Fun | Args],
	Func =.. [Fun, Val | Args].


runSelectUpdateKey(Functor, Last, Select, Norefuse):-
	(   util:getKeyHit(left);util:getKeyHit(up)),
	runSelectUpdateLeft(Select, Last, NewSelect),
	dialog:runSelectUpdate2(Functor, Last, NewSelect, Norefuse), !.
runSelectUpdateKey(Functor, Last, Select, Norefuse):-
	(   util:getKeyHit(right);util:getKeyHit(down)),
	runSelectUpdateRight(Select, Last, NewSelect),
	dialog:runSelectUpdate2(Functor, Last, NewSelect, Norefuse), !.

runSelectUpdateKey(_Functor, _Last, Select, _Norefuse) :-
	util:getKeyHit(action),
	runSelectFinish(Select),!.
runSelectUpdateKey(_Functor, _Last, _Select, false) :-
	util:getKeyHit(menu),
	runSelectFinish(-1), !.

runSelectUpdateKey(Functor, Last, Select, Norefuse) :-
	update:register(ui, dialog:runSelectUpdateKey(Functor, Last, Select, Norefuse)).

runSelectFinish(Select) :-
	dialog:pop,
	util:clearKeys,
	update:push(Select).


runSelectUpdateLeft(0, Last, Last) :- !.
runSelectUpdateLeft(Select, _Last, NewSelect):-
	NewSelect is Select - 1.

runSelectUpdateRight(Last, Last, 0):- !.
runSelectUpdateRight(Select, _Last, NewSelect):-
	NewSelect is Select + 1.


% IN: str; text; question text
% [IN]: int; default=0; default selected answer, 0=yes, 1=no
% OUT: int; selected answer, 0=yes, 1=no
% Opens a simple question yes/no dialog.
% Uses DialogQuestion() function for dialog creation.
openQuestion(Text) :-
	openQuestion(Text, 0).
openQuestion(Text, Default) :-
	runSelect2(dialog:question(Text), Default).

question(state, yes, left, no).
question(state, yes, up, no).
question(state, yes, right, no).
question(state, yes, down, no).
question(state, no, left, yes).
question(state, no, up, yes).
question(state, no, right, yes).
question(state, no, down, yes).
question(state, State, action, return(State)).
question(final, return(State), State).


% IN: int; select; current selection index 0=yes, 1=no
% Dialog creation function used by OpenDialogQuestion().

question(draw, State, Text) :-
	format(atom(Text1), '{a:center}~a\n\n{c:ffff0000}', Text),
	(   State = yes
	->  Text2 = '{f:1}YES{f:0}   NO'
	;   Text2 = 'YES   {f:1}NO{f:0}'),
	atom_concat(Text1, Text2, Text3),
	gamedef:fontDefault(Font),
	def:color(dialog, Color),
	fitCenter(dialog(_, _, style(Font, Color), Text3), Dialog),
	push(Dialog).













