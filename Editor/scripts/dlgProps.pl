:-module(dlgProps, [create/0,
		    create/1,
		   create/2]).

propsCount(16).

readOnly(x).
readOnly(y).
readOnly(w).
readOnly(h).
readOnly(select).


create :- create(noraml).
create(Mode) :-	edi:toolBrush(B), create(Mode, B).


create(Mode, Brush ) :-
	gui:dlgTitleH(TitleH),
	propsCount(PC),
	DlgH is TitleH + 8 + 24 + 22 * PC + 8,
	format(string(Title), '#~p props', [Brush]),
	gui:createDlgTitleModal(0, 0, 200, DlgH, Title),
	def:dlg(props, ID),
	dlg:setID(ID),
	gui:addKey(escape > gui:dlgClose),
	dlg:setCloseOut,
	dlg:setCloseCmd(dlgProps:close),
	createProps(100, WMax, Mode, Brush),
	editor:param(props_page, 0, Page),
	gui:dlgTitleH(TitleH),
	X1 = 8,
	Y is TitleH + 8,
	W is (WMax - 16) // 3,
	(Page=:=0->V1=1;V1=0),
	(Page=:=1->V2=1;V2=0),
	(Page=:=2->V3=1;V3=0),
	X2 is X1 + W,
	X3 is X2 + W,
	gui:createRadioButton(X1, Y, W, "BRUSH",  V1, 1, dlgProps:setPage(0, Mode)),
	gui:createRadioButton(X2, Y, W, "OBJECT", V2, 1, dlgProps:setPage(1, Mode)),
	gui:createRadioButton(X3, Y, W, "USER",   V3, 1, dlgProps:setPage(2, Mode)),

	setPage(Page, Mode),
	gui:dlgResize(0, 0, WMax, DlgH),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


createProps(WMax, CurMax, Mode, Brush) :-
	gui:dlgTitleH(TitleH),
	Y is TitleH + 8 + 24,
	findall(bp(Id, Idx, Value, Name, ToolTip, Type), (mod:brushProp(Id, Idx, Name, ToolTip, Type), brush:getProp(Brush, Idx, Value)), Props),
	createProps(Props, 0, Y, W, Mode, Brush),
	CurMax is max(WMax, W).

createProps([], _, _, 0, _, _).
createProps([P|Ps], X, Y, CurW, Mode, Brush) :-
	createProp(P, X, Y, PW, Mode, Brush),
	Y2 is Y + 22,
	createProps(Ps, X, Y2, OW, Mode, Brush),
	CurW is max(OW, PW).

createProp(Prop, X0, Y, TotalW, Mode, Brush) :-
	X is X0 + 8,
	createIndex(Prop, X, Y, X1),
	createMultiChecks(Prop, X1, Y, Mode, X2),
	createValueEdit(Prop, X2, Y, X3, Brush),
	createName(Prop, X3, Y, X4, Brush),
	TotalW is X4 - X0.

setID(Idx, N) :-
	IIDX is Idx * 10 + N,
	def:dlg(item(IIDX), ID),
	gui:itemSetID(ID).

createIndex(bp(_,Idx,_,_,_,_), X, Y, XX):-
	format(string(Text), '#~d', [Idx]),
	gui:createText(X, Y, 32, Text),
	setID(Idx, 0),
	gui:styleCode([backgr, border], Style),
	gui:itemSetStyle(Style),
	gui:itemSetProps([color(0, gui1), color(1, gui1), color(2, black)]),
	XX is X + 32 + 8.

createMultiChecks(_, X, _, normal, X) :- !.
createMultiChecks(bp(Prop, Idx,_,_,_,_), X, Y, Mode, XX) :-
	gui:createImage(X, Y, 20, 20, check1),
	gui:itemSetValue(0),
	gui:itemSetCmdAction(dlgProps:multiCheck(Mode)),
	setID(Idx, 1),
	(   readOnly(Prop)
	->  gui:itemSetDisable(1),
	    C = gui1
	;   C = edit),
	def:color(C, Color),
	gui:itemSetImgColor(Color),
	XX is X + 20 + 8.


createValueEdit(bp(Prop, Idx,Value,_,_,Type), X, Y, XX, Brush) :-
	(   Type == color
	->  Format = '~`0t~16r~8|'
	;   Format = '~d'),
	format(string(SZValue), Format, [Value]),
	gui:createEdit(X, Y, 80, SZValue),
	setID(Idx, 2),
	gui:itemSetCmdAction(dlgProps:inputSet(Idx, Brush)),
	(readOnly(Prop)->gui:itemSetDisable(1);true),
	XX is X + 80 + 8.

createName(bp(Id, Idx, Value, Name, ToolTip, Type), X, Y, XX, Brush) :-
	getPropBtName(Name, Value, Type, BtName),
	gui:createButton(X, Y, 140, BtName),
	setID(Idx, 3),
	gui:alignCode([left, centery], Align),
	gui:itemSetTxtAlign(Align),
	gui:itemSetToolTip(ToolTip),
	gui:styleCode([backgr, border3d], Style),
	gui:itemSetStyle(Style),
	(   Type \= common
	->  gui:itemSetCmdAction(dlgProps:browse(Idx, Brush))
	;   true),
	(readOnly(Id)->gui:itemSetDisable(1);true),
	XX is X + 140 + 8.

getPropBtName(Name, Value, Type, BtName) :-
	getPropValueName(Value, Type, Caption),
	format(string(BtName), '~a ~s', [Name, Caption]).

getPropValueName(Value, select(Variants), Caption) :-
	(   member(Value-V, Variants);	V = unknown),
	format(string(Caption), '= ~a', [V]), !.
getPropValueName(Value, color, Caption) :-
	format(string(Caption),  ' = ~`0t~16r~8|', [Value]), !.
getPropValueName(_, _, '').

setPage(Page, Mode) :-
	forall(mod:brushProp(Key,Idx,_,_,_), setPage(Page, Mode, Key, Idx)).
setPage(Page, Mode, Key, Idx) :-
	propsCount(PC),
	ItemPage is Idx // PC,
	gui:dlgTitleH(TitleH),
	Y is  (TitleH + 8 + 24) + (Idx - Page * PC) * 22,
	Y2 is Y + 20,
	(ItemPage==Page->HV=0;HV=1),
	setPage(Key, Idx, 0, Y, Y2, HV),
	(   Mode \= normal
	->  setPage(Key, Idx, 1, Y, Y2, HV)
	;   true),
	setPage(Key, Idx, 2, Y, Y2, HV),
	setPage(Key, Idx, 3, Y, Y2, HV),
	editor:param(props_page, Page).

setPage(Key, Idx, N, Y, Y2, HV) :-
	select(Idx, N),
	gui:itemSetY(Y),
	gui:itemSetY2(Y2),
	gui:itemSetHidden(HV),
	gui:itemSetDisable(HV),
	(readOnly(Key)->gui:itemSetDisable(1);true).

select(Idx, N) :-
	IIDX is Idx * 10 + N,
	gui:select(IIDX).


update(Brush) :-
	forall(mod:brushProp(_, Idx,Name,_,Type), (brush:getProp(Brush, Idx, Value), updateItem(Name, Value, Type, Idx))).

updateItem(Name, Value, Type, Idx) :-
	select(Idx, 3),
	getPropBtName(Name, Value, Type, BtName),
	gui:itemSetTxt(BtName).

close :-
	map:refresh.


browsePos(XX, YY) :-
	dlg:getPos(X, Y),
	gui:itemGetX(IX),
	gui:itemGetY(IY),
	XX is X + IX,
	YY is Y + IY + 20.

browse(Idx, Brush) :-
    mod:brushProp(_, Idx, _, _, Type),
    brush:getProp(Brush, Idx, Value),
    browse(Idx, Value, Brush, Type).

browse(Idx, Value, Brush, select(Variants)) :-
	browsePos(X, Y),
	gui:createPullDownSelect(X, Y, act(V, dlgProps:browseSet(V, Idx, Brush)), Variants, Value),
	gui:dlgMoveInBound.


browse(Idx, Color, Brush, color) :-
	browsePos(X, Y),
	dlgColor:create(X, Y, dlgProps:browseSet(_V, Idx, Brush), Color),
	gui:dlgMoveInBound.


browseSet(V, Idx, Brush) :-
	gui:dlgSelect(props),
	brush:setProp(Brush, Idx, V),
	select(Idx, 2),
	(   mod:brushProp(_, Idx, _, _, color)
	->  Format = '~`0t~16r~8|'
	;   Format = '~d'),
	format(string(SZValue), Format, [V]),
	gui:itemSetTxt(SZValue),
	update(Brush).

inputSet(Idx, Brush) :-
	gui:itemGetTxt(SZValue),
	(   mod:brushProp(color, Idx, _, _, color)
	->  (format(string(SS), '0x~a', [SZValue]),atom_number(SS, Value))
	;   atom_number(SZValue, Value)),

	brush:setProp(Brush, Idx, Value),
	update(Brush).

multiCheck(search, 0, 0, check1).
multiCheck(search, 1, 1, check3).
multiCheck(search, 2, 2, check4).
multiCheck(search, 3, 0, check1).

multiCheck(change, 0, 0, check1).
multiCheck(change, 1, 1, check2).
multiCheck(change, 2, 0, check1).


multiCheck(Mode) :-
	gui:itemGetDisable(1);
	gui:itemGetValue(Val),
	V is Val + 1,
	multiCheck(Mode, V, NV, C),
	gui:getImg(C, Img),
	gui:itemSetImg0(Img),
	gui:itemSetValue(NV).





