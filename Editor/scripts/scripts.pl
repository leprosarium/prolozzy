:- module(scripts, [brushSearch/0]).

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
	edi:waitCursor(1),
	brushSearchDo(Props, Mode, Count),
	edi:waitCursor(0),
	gui:dlgClose,
	(   Mode == del
	->  SZop = deselected
	;   SZop = selected),
	format(string(Msg), '~d brushes ~a.', [Count, SZop]),
	gui:msgBoxOk('Message', Msg, icon_info),
	selection:refresh,
	map:refresh.


brushSearchDo(Props, Mode, C) :-
	map:brushCount(BC),
	brushSearchDo(0, BC, Props, Mode, C).
brushSearchDo(B, B, _, _, 0).
brushSearchDo(Br, BC, Props, Mode, C) :-
	brushSearchMatch(Props, Br, Mch),
	brushSearchDoSelect(Mch, Mode, Br, CB),
	BrN is Br + 1,
	brushSearchDo(BrN, BC, Props, Mode, CO),
	C is CO + CB.

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




