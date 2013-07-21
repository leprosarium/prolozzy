:- module(scripts, [brushSearch/0,
		    brushChange/0,
		    brushInvert/0,
		    brushMove/0,
		    selectByIdx/0,
		    brushKeepTopmost/0,
		    brushGroupIds/0,
		    forallBrush/2]).

:-use_module(gui, [waitCall/1]).

brushSearch :-
	dlgProps2:create(search),
	gui:dlgSetTitle('Brush Search'),
	dlg:getRect(X1, Y1, _X2, Y2),
	Y is Y2 - Y1 + 8,
	gui:createButton(8, Y, 80, 'SEARCH', scripts:brushSearchApply),
	gui:itemSetToolTip('Select brushes that match checked properties\nusing the specified selection operation.'),

	createOpt(104, Xo1, Y, 1, set, 'select brushes', 'SET'),
	createOpt(Xo1, Xo2, Y, 0, add, 'add to current selection', 'ADD'),
	createOpt(Xo2, Xo3, Y, 0, del, 'remove from current selection', 'SUB'),
	YY is Y + 22 + 8,
	W is Xo3 - X1,
	gui:dlgResize(0, 0, W, YY).

createOpt(X, XO, Y, V, ID, ToolTip, Text) :-
	gui:createRadio(X, Y, V, 2),
	gui:itemSetID(ID),
	gui:itemSetToolTip(ToolTip),
	XX is X + 20,
	gui:createText( XX, Y, 32, Text),
	XO is XX + 40.

brushSearchMode(Mode) :- member(Mode, [set, add, del]), gui:itemSelect(Mode), gui:itemGetValue(1).


getCheckProp(p(Prop, C, V)) :-
	brush:varDef(Prop,_),
	gui:itemSelect(id(Prop, check)),
	gui:itemGetValue(C),
	C =\= 0,
	edi:toolBrush(B),
	brush:get(B, Prop, V).

getCheckProps(Props) :-
	findall(P, getCheckProp(P), Props).

brushSearchApply :-
	brushSearchMode(Mode),
	getCheckProps(Props),
	(   length(Props, 0)
	->  gui:msgBoxOk('Message', 'no props checked.', icon_info)
	;   brushSearchDo(Props, Mode)).

brushSearchDo(Props, Mode) :-
	forallBrush(brushSearch(_, _, Props, Mode), Count),
	gui:dlgClose,
	(   Mode == del
	->  SZop = deselected
	;   SZop = selected),
	format(string(Msg), '~d brushes ~a.', [Count, SZop]),
	gui:msgBoxOk('Message', Msg, icon_info),
	selection:refresh,
	map:refresh.

brushSearch(Br, C, Props, Mode) :-
	brushSearchMatch(Props, Br, Mch),
	brushSearchDoSelect(Mch, Mode, Br, C).

brushSearchDoSelect(yes, set, Br, 1) :-
	brush:setSelect(Br, 1).
brushSearchDoSelect(yes, add, Br, C) :-
	(brush:getSelect(Br, 0)->C=1;C=0),
	brush:setSelect(Br, 1).
brushSearchDoSelect(yes, del, Br, C) :-
	(brush:getSelect(Br, 1)->C=1;C=0),
	brush:setSelect(Br, 0).
brushSearchDoSelect(no, Mode, Br, 0) :-
	Mode == set ->  brush:setSelect(Br, 0); true.

brushSearchMatch(Props, Brush, yes) :-
	forall(member(Prop, Props), brushSearchMatch(Prop, Brush)), !.
brushSearchMatch(_, _, no).

brushSearchMatch(p(Prop, C, V), Br) :-
	brush:get(Br, Prop, Val),
	(C=\=1,V \= Val; C==1,V = Val).

brushChange :-
	dlgProps2:create(change),
	gui:dlgSetTitle('Brush Change'),
	dlg:getRect(X1, Y1, X2, Y2),
	Y is Y2 - Y1 + 8,
	gui:createButton(8, Y, 80, 'CHANGE', scripts:brushChangeApply),
	gui:itemSetToolTip('Set checked properties of the selected brushes\nwith values from the current brush.'),
	YY is Y + 22 + 8,
	W is X2 - X1,
	gui:dlgResize(0, 0, W, YY).

brushChangeApply :-
	getCheckProps(Props),
	(   length(Props, 0)
	->  gui:msgBoxOk('Message', 'no props checked.', icon_info)
	;   brushChangeDo(Props)).


brushChangeDo(Props) :-
	forallBrush(brushChange(_, _, Props), Count),
	gui:dlgClose,
	format(string(Msg), 'changes applied on ~d  brushes.', [Count]),
	gui:msgBoxOk('Message', Msg, icon_info),
	map:refresh.


brushChange(Br, C, Props) :-
	(   brush:getSelect(Br, 0)
	->  C = 0
	;   C = 1,
	    forall(member(p(Prop, 1, V), Props), brush:set(Br, Prop, V))).

brushInvert :-
	forallBrush(brushInvert(_, _), C),
	format(string(Msg), '~d brushes selected. ', [C]),
	gui:msgBoxOk('Message', Msg, icon_info),
	selection:refresh,
	map:refresh.


brushInvert(Br, Sel):-
	(   brush:getSelect(Br, 1)
	->  Sel = 0
	;   Sel = 1),
	brush:setSelect(Br, Sel).


brushMove :-
	gui:dlgTitleH(TitleH),
	W is 48*3 + 16,
	H is TitleH + 8 + 20,
	gui:createDlgTitleModal(0, 0, W, H, 'Brush Move by offset'),
	dlg:setCloseOut,
	X = 4,
	Y is TitleH + 4,
	gui:createEdit(X, Y, 48, '0'),
	gui:itemSetToolTip('X offset'),
	def:dlg(item(0), ID),
	gui:itemSetID(ID),
	X2 is X + 48 + 4,
	gui:createEdit(X2, Y, 48, '0'),
	gui:itemSetToolTip('Y offset'),
	def:dlg(item(1), ID1),
	gui:itemSetID(ID1),
	X3 is X2 + 48 + 4,
	gui:createButton(X3, Y, 48, 'MOVE', scripts:brushMoveApply),
	gui:itemSetToolTip('Move selected brushes'),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

brushMoveApply :-
	gui:select(0), gui:itemGetTxt(XX), atom_number(XX, X),
	gui:select(1), gui:itemGetTxt(YY), atom_number(YY, Y),
	forallBrush(brushMove(_, _, X, Y), C),
	gui:dlgClose,
	format(string(Msg), '~d brushes moved. ', [C]),
	gui:msgBoxOk('Message', Msg, icon_info),
	map:refresh.


brushMove(Br, 0, _, _):-
	brush:getSelect(Br, 0), !.
brushMove(Br, 1, DX, DY):- !,
	brush:getX(Br, X),
	brush:getY(Br, Y),
	XX is X + DX,
	YY is Y + DY,
	brush:setX(Br, XX),
	brush:setY(Br, YY).


forallBrush(Action, Sum) :-
	waitCall(forallBrush2(Action, Sum)).

forallBrush2(Action, Sum) :-
	findall(Cnt, (map:brush(Br), term_variables(Action, [Br, Cnt|_]), call(Action)), Cnts),
	sum_list(Cnts, Sum).

selectByIdx :-
	gui:dlgTitleH(TitleH),
	W is 8+64+4+48,
	H is TitleH + 8 + 20 + 20 * 3 + 4,

	gui:createDlgTitleModal(0, 0, W, H, 'Brush Select by idx'),
	gui:addKey(escape > gui:dlgClose),
	dlg:setCloseOut,
	X = 4,
	Y is TitleH + 4,
	XX is X + 20,
	gui:createRadio(X, Y, 1, 1 ),
	def:dlg(item(1), ID1), gui:itemSetID(ID1),
	gui:itemSetToolTip('count all brushes'),
	gui:createText(XX, Y, 32, 'ALL' ),
	Y2 is Y + 20,

	gui:createRadio(X, Y2, 0, 1 ),
	def:dlg(item(2), ID2), gui:itemSetID(ID2),
	gui:itemSetToolTip('count static brushes only'),
	gui:createText(XX, Y2, 32, 'STATIC'),
	Y3 is Y2 + 20,

	gui:createRadio(X, Y3, 0, 1 ),
	def:dlg(item(3), ID3), gui:itemSetID(ID3),
	gui:itemSetToolTip('count dynamic brushes only'),
	gui:createText(XX, Y3, 32, 'DYNAMIC'),
	Y4 is Y3 + 20 + 2,
	gui:createEdit(X, Y4, 64, "0"),
	def:dlg(item(0), ID0), gui:itemSetID(ID0),
	XX2 is X + 64 + 4,
	gui:createButton(XX2, Y4, 48, 'Select', scripts:selectByIdxApply),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


selectByIdxApply :-
	selectByIdxApplyParams(Mode, Idx),
	(   waitCall(selectByIdxApply(Mode, Idx, Br))
	->  format(string(Msg), '~p selected.', [Br]),
	    deselectAll,
	    brush:setSelect(Br, 1),
	    selection:goto(1),
	    map:setSelect(1)
	;   Msg = 'nothing to select.'),
	map:refresh,
	gui:dlgClose,
	gui:msgBoxOk('Message', Msg, icon_info).

selectByIdxApplyParams(Mode, Idx) :-
	selectByIdxApplyMode(Mode),
	gui:select(0),
	gui:itemGetTxt(V),
	atom_number(V, Idx).

selectByIdxApplyMode(all) :- gui:select(1), gui:itemGetValue(1).
selectByIdxApplyMode(static) :- gui:select(2), gui:itemGetValue(1).
selectByIdxApplyMode(dynamic) :- gui:select(3), gui:itemGetValue(1).

deselectAll :-
       forall(map:brush(Br), brush:setSelect(Br, 0)).

selectByIdxApply(all, Idx, Br) :-
	C = count(0),
	map:brush(Br),
	arg(1, C, N),
	N1 is N + 1,
	nb_setarg(1, C, N1),
	N = Idx.

selectByIdxApply(Mode, Idx, Br) :- !,
	def:brushType(Mode, Type),
	C = count(0),
	map:brush(Br),
	brush:getType(Br, Type),
	arg(1, C, N),
	N1 is N + 1,
	nb_setarg(1, C, N1),
	N = Idx.

brushKeepTopmost :-
	waitCall(
	    (findall(Br, (map:brush(Br), brush:getSelect(Br, 1), brush:setSelect(Br, 0)), Selected),
	     last(Selected, Topmost))),
	brush:setSelect(Topmost, 1),
	gui:msgBoxOk('Message', 'Topmost brush selected.', icon_info),
	selection:refresh,
	map:refresh.

brushGroupIds :-
	map:getSelect(0)
	-> gui:msgBoxOk('Message', 'No selected brushes.', icon_info);
	gui:dlgTitleH(TitleH),
	W is 8 + 132,
	H is TitleH + 4 + 14 * 6 + 10 + 20 + 4,
	gui:createDlgTitleModal(0, 0, W, H, 'Brush Group Ids'),
	gui:addKey(escape > gui:dlgClose),
	dlg:setCloseOut,
	X = 4,
	Y is TitleH + 4,
	WW is 14 * 6 + 6,
	gui:createBar(X, Y, 132, WW, gui1),
	brushGroupIdsCreateText(['Choose the first id and',
				 'the selected brushes',
				 'will have incrementing',
				 'ids starting from it.',
				 'Make sure not to',
				 'generate duplicated ids!'
				], X, Y, W, YO),
	YY is YO + 10,
	gui:createEdit(X, YY, 64, '0'),
	def:dlg(item(0), ID),
	gui:itemSetID(ID),
	XX is 8 + 132 - 48 - 4,
	gui:createButton(XX, YY, 48, 'Apply', scripts:brushGroupIdsApply),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


brushGroupIdsCreateText([], _, Y, _, Y).
brushGroupIdsCreateText([T|Ts], X, Y, W, YO) :-
	gui:createText(X, Y, W, T),
	Y2 is Y +14,
	brushGroupIdsCreateText(Ts, X, Y2, W, YO).


brushGroupIdsApply :-
	gui:select(0),
	gui:itemGetTxt(V),
	(   atom_number(V, ID0), ID0 > 0
	->  waitCall(brushGroupIdsApply(ID0, ID)),
	    map:refresh,
	    gui:dlgClose,
	    format(string(Msg), 'Done.\nIds set from ~d to ~d.', [ID0, ID])
	;   Msg = 'Choose a valid starting id (non zero).'),
	gui:msgBoxOk('Message', Msg, icon_info).

brushGroupIdsApply(FirstId, LastId) :-
	ID = id(FirstId),
	(   map:brush(Br),
	    brush:getSelect(Br, 1),
	    arg(1, ID, Id),
	    brush:setID(Br, Id),
	    Id1 is Id + 1,
	    nb_setarg(1, ID, Id1),
	    fail
	;   arg(1, ID, IDL),
	    LastId is IDL - 1).







