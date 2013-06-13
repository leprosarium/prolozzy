:- module(scripts, [brushSearch/0,
		    brushChange/0,
		    brushInvert/0,
		    brushMove/0,
		    selectByIdx/0,
		    brushKeepTopmost/0,
		    brushGroupIds/0]).

:-use_module(gui, [waitCall/1]).

brushSearch :-
	dlgProps:create(search),
	gui:dlgSetTitle('Brush Search'),
	dlg:getRect(X1, Y1, X2, Y2),
	Y is Y2 - Y1 + 8,
	gui:createButton(8, Y, 80, 'SEARCH', scripts:brushSearchApply),
	gui:itemSetToolTip('Select brushes that match checked properties\nusing the specified selection operation.'),

	createOpt(104, Xo1, Y, 1, 1000, 'select brushes', 'SET'),
	createOpt(Xo1, Xo2, Y, 0, 1001, 'add to current selection', 'ADD'),
	createOpt(Xo2, _Xo3, Y, 0, 1002, 'remove from current selection', 'SUB'),
	YY is Y + 22 + 8,
	W is X2 - X1,
	gui:dlgResize(0, 0, W, YY).

createOpt(X, XO, Y, V, N, ToolTip, Text) :-
	gui:createRadio(X, Y, V, 2),
	def:dlg(item(N), ID),
	gui:itemSetID(ID),
	gui:itemSetToolTip(ToolTip),
	XX is X + 20,
	gui:createText( XX, Y, 32, Text),
	XO is XX + 40.

brushSearchMode(set) :- gui:select(1000), gui:itemGetValue(1).
brushSearchMode(add) :- gui:select(1001), gui:itemGetValue(1).
brushSearchMode(del) :- gui:select(1002), gui:itemGetValue(1).


getCheckProp(p(Idx, C, V)) :-
	brush:varDef(Prop,Idx,_),
	dlgProps:select(Idx, 1),
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
	map:brushSetSelect(Br, 1).
brushSearchDoSelect(yes, add, Br, C) :-
	(map:brushGetSelect(Br, 0)->C=1;C=0),
	map:brushSetSelect(Br, 1).
brushSearchDoSelect(yes, del, Br, C) :-
	(map:brushGetSelect(Br, 1)->C=1;C=0),
	map:brushSetSelect(Br, 0).
brushSearchDoSelect(no, Mode, Br, 0) :-
	Mode == set ->  map:brushSetSelect(Br,0); true.

brushSearchMatch(Props, BrushIdx, yes) :-
	forall(member(Prop, Props), brushSearchMatch(Prop, BrushIdx)), !.
brushSearchMatch(_, _, no).

brushSearchMatch(p(Idx, C, V), Br) :-
	map:brushGet(Br, Idx, Val),
	(C=\=1,V=\=Val; C==1,V==Val).

brushChange :-
	dlgProps:create(change),
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
	(   map:brushGetSelect(Br, 0)
	->  C = 0
	;   C = 1,
	    forall(member(p(Idx, 1, V), Props), map:brushSet(Br, Idx, V))).

brushInvert :-
	forallBrush(brushInvert(_, _), C),
	format(string(Msg), '~d brushes selected. ', [C]),
	gui:msgBoxOk('Message', Msg, icon_info),
	selection:refresh,
	map:refresh.


brushInvert(Br, Sel):-
	(   map:brushGetSelect(Br,1)
	->  Sel = 0
	;   Sel = 1),
	map:brushSetSelect(Br, Sel).


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
	map:brushGetSelect(Br, 0), !.
brushMove(Br, 1, DX, DY):- !,
	map:brushGetX(Br, X),
	map:brushGetY(Br, Y),
	XX is X + DX,
	YY is Y + DY,
	map:brushSetX(Br, XX),
	map:brushSetY(Br, YY).


forallBrush(Action, C) :-
	map:brushCount(BC),
	waitCall(forallBrush(0, BC, Action, 0, C)).

forallBrush(N, N, _, C, C).
forallBrush(Br, BC, Action, C, Count) :-
	copy_term(Action, Cmd),
	term_variables(Cmd, [Br, Cn|_]),
	call(Cmd),
	C2 is C + Cn,
	Br2 is Br + 1,
	forallBrush(Br2, BC, Action, C2, Count).


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
	map:brushCount(0)
	->  gui:dlgClose,
	    gui:msgBoxOk('Message', 'nothing to select.', icon_info)
	;   selectByIdxApplyParams(Mode, Idx),
	    waitCall(
		(selectByIdxApply(Mode, Idx, IdxO),
		 deselectAll)),
	    (	IdxO == -1
	    ->	Msg = 'nothing to select.'
	    ;	format(string(Msg), 'brush #~d selected.', [IdxO]),
		map:brushSetSelect(IdxO, 1),
		selection:goto(1),
		map:setSelect(1)),
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
       map:brushCount(BC),
       BCN is BC - 1,
       forall(between(0, BCN, Idx), map:brushSetSelect(Idx, 0)).

selectByIdxApply(all, Idx, Idx) :-
	map:brushCount(BC),
	Idx >= 0,
	Idx < BC.
selectByIdxApply(all, _, -1).


selectByIdxApply(Mode, IdxI, IdxO) :-
	map:brushCount(BC),
	def:brushType(Mode, Type),
	selectByIdxApply(0, BC, Type, 0, IdxI, IdxO).

selectByIdxApply(BC, BC, _, _, _, -1).
selectByIdxApply(Br, BC, Type, C, IdxI, IdxO) :-
	map:brushGetType(Br, Type)
	->  (   C == IdxI
	    ->	IdxO = Br
	    ;	C1 is C + 1,
		Br2 is Br + 1,
		selectByIdxApply(Br2, BC, Type, C1, IdxI, IdxO))
	;   Br2 is Br + 1,
	selectByIdxApply(Br2, BC, Type, C, IdxI, IdxO).


brushKeepTopmost :-
	map:brushCount(BC),
	BC1 is BC - 1,
	waitCall((
	(   brushKeepTopmost(BC1, Topmost)
	->  Mx is Topmost - 1,
	    forall(between(0, Mx, Br), map:brushSetSelect(Br, 0)),
	    gui:msgBoxOk('Message', 'Topmost brush selected.', icon_info)
	;   true))),
	selection:refresh,
	map:refresh.

brushKeepTopmost(-1, _) :- !, fail.
brushKeepTopmost(Br, Br) :- map:brushGetSelect(Br, 1).
brushKeepTopmost(Br, T) :-
	Br2 is Br - 1,
	brushKeepTopmost(Br2, T).


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
	atom_number(V, ID0),
	(   ID0 == 0
	->  gui:msgBoxOk('Message', 'Choose a valid starting id (non zero).', icon_info)
	;   map:brushCount(BC),
	    waitCall(brushGroupIdsApply(ID0, 0, BC, ID)),
	    map:refresh,
	    gui:dlgClose,
	    format(string(Msg), 'Done.\nIds set from ~d to ~d.', [ID0, ID]),
	    gui:msgBoxOk('Message', Msg, icon_info)).

brushGroupIdsApply(ID, BC, BC, IDO) :- IDO is ID - 1.
brushGroupIdsApply(ID, Br, BC, IDO) :-
	(   map:brushGetSelect(Br, 1)
	->  map:brushSetID(Br, ID),
	    IDN is ID + 1
	;   IDN = ID),
	Br2 is Br + 1,
	brushGroupIdsApply(IDN, Br2, BC, IDO).






