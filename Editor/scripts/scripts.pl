:- module(scripts, [brushSearch/0,
		    brushChange/0,
		    brushInvert/0,
		    brushMove/0]).

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
	mod:brushProp(_,Idx,_,_,_),
	dlgProps:select(Idx, 1),
	gui:itemGetValue(C),
	C =\= 0,
	edi:toolBrushGet(Idx, V).

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
	edi:waitCursor(1),
	map:brushCount(BC),
	forallBrush(0, BC, Action, 0, C),
	edi:waitCursor(0).

forallBrush(N, N, _, C, C).
forallBrush(Br, BC, Action, C, Count) :-
	copy_term(Action, Cmd),
	term_variables(Cmd, [Br, Cn|_]),
	call(Cmd),
	C2 is C + Cn,
	Br2 is Br + 1,
	forallBrush(Br2, BC, Action, C2, Count).



