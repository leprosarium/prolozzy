:-module(dlgProps2, [create/0,
		     create/1,
		     create/2]).

propsCount(22).

readOnly(x).
readOnly(y).
readOnly(w).
readOnly(h).
readOnly(select).


system(id).
system(layer).
system(x).
system(y).
system(w).
system(h).
system(tile).
system(frame).
system(x1).
system(y1).
system(x2).
system(y2).
system(flip).
system(color).
system(shader).
system(scale).
system(material).
system(draw).
system(disable).
system(delay).
system(anim).
system(collider).

create :- create(normal).
create(Mode) :-	edi:toolBrush(B), create(Mode, B).


create(Mode, Brush) :-
	gui:dlgTitleH(TitleH),
	propsCount(PC),
	DlgH is TitleH + 8 + 24 + 22 * PC + 8,
	format(string(Title), '#~p props 2', [Brush]),
	gui:createDlgTitleModal(0, 0, 200, DlgH, Title),
	def:dlg(props, ID),
	dlg:setID(ID),
	gui:addKey(escape > gui:dlgClose),
	dlg:setCloseOut,
	dlg:setCloseCmd(dlgProps2:dlgClose(Brush)),
	createProps(100, WMax, Mode, Brush),
	editor:param(props_page, 0, Page),
	X1 = 8,
	Y is TitleH + 8,
	W is (WMax - 16) // 3,
	(Page=:=0->V1=1;V1=0),
	(Page=:=1->V2=1;V2=0),
	(Page=:=2->V3=1;V3=0),
	X2 is X1 + W,
	X3 is X2 + W,
	gui:createRadioButton(X1, Y, W, "BRUSH",  V1, 1, dlgProps2:setPage(0, Mode)),
	gui:createRadioButton(X2, Y, W, "OBJECT", V2, 1, dlgProps2:setPage(1, Mode)),
	gui:createRadioButton(X3, Y, W, "USER",   V3, 1, dlgProps2:setPage(2, Mode)),
	setPage(Page, Mode),
	gui:dlgResize(0, 0, WMax, DlgH),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


createProps(WMax, CurMax, Mode, Brush) :-
	gui:dlgTitleH(TitleH),
	Y is TitleH + 8 + 24,
	findall(bp(Id, Value, Name, ToolTip, Type),
		(   mod:brushProp(Id, Name, ToolTip, Type),
		    (	brush:get(Brush, Id, Value)->true;brush:varDef(Id, Value))), Props),
	createProps(Props, 0, Y, W, Mode, Brush),
	CurMax is max(WMax, W).

createProps([], _, _, 0, _, _).
createProps([P|Ps], X, Y, CurW, Mode, Brush) :-
	createProp(P, X, Y, PW, Mode, Brush),
	Y2 is Y + 22,
	createProps(Ps, X, Y2, OW, Mode, Brush),
	CurW is max(OW, PW).


createProp(Prop, X0, Y, TotalW, Mode, Brush) :-
	X1 is X0 + 8,
	createMultiChecks(Prop, X1, Y, Mode, X2),
	createName(Prop, X2, Y, X3, Brush),
	createValueEdit(Prop, X3, Y, X4, Brush),
	TotalW is X4 - X0.

createMultiChecks(_, X, _, normal, X) :- !.
createMultiChecks(bp(Prop,_,_,_,_), X, Y, Mode, XX) :-
	gui:createImage(X, Y, 20, 20, check1),
	gui:itemSetValue(0),
	gui:itemSetCmdAction(dlgProps2:multiCheck(Mode)),
	gui:itemSetID(id(Prop, check)),
	(   readOnly(Prop)
	->  gui:itemSetDisable(1),
	    C = gui1
	;   C = edit),
	def:color(C, Color),
	gui:itemSetImgColor(Color),
	XX is X + 20 + 8.


propFormat(atom, '~a') :- !.
propFormat(color, '~`0t~16r~8|') :- !.
propFormat(_, '~d').

createValueEdit(bp(Prop, Value,_,_,Type), X, Y, XX, Brush) :-
	propFormat(Type, Format),
	format(string(SZValue), Format, [Value]),
	gui:createEdit(X, Y, 80, SZValue),
	gui:itemSetID(id(Prop, edit)),
	gui:itemSetCmdAction(dlgProps2:inputSet(Prop, Brush)),
	(readOnly(Prop)->gui:itemSetDisable(1);true),
	XX is X + 80 + 8.

createName(bp(Id, Value, Name, ToolTip, Type), X, Y, XX, Brush) :-
	getPropBtName(Name, Value, Type, BtName),
	gui:createButton(X, Y, 140, BtName),
	gui:itemSetID(id(Id, name)),
	gui:alignCode([left, centery], Align),
	gui:itemSetTxtAlign(Align),
	gui:itemSetToolTip(ToolTip),
	gui:styleCode([backgr, border3d], Style),
	gui:itemSetStyle(Style),
	(   Type \= common
	->  gui:itemSetCmdAction(dlgProps2:browse(Id, Brush))
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
	findall(Key, mod:brushProp(Key,_,_,_), Props),
	setPage(Props, Page, Mode, 0),
	editor:param(props_page, Page).


setPage([], _, _, _).
setPage([Key|Props], Page, Mode, I) :-
	propsCount(PC),
	ItemPage is I // PC,
	gui:dlgTitleH(TitleH),
	Y is  (TitleH + 8 + 24) + (I - Page * PC) * 22,
	Y2 is Y + 20,
	(ItemPage==Page->HV=0;HV=1),
	(   Mode \= normal
	->  setPage(Key, check, Y, Y2, HV)
	;   true),
	setPage(Key, edit, Y, Y2, HV),
	setPage(Key, name, Y, Y2, HV),
	I2 is I + 1,
	setPage(Props, Page, Mode, I2).

setPage(Key, N, Y, Y2, HV) :-
	gui:itemSelect(id(Key, N)),
	gui:itemSetY(Y),
	gui:itemSetY2(Y2),
	gui:itemSetHidden(HV),
	gui:itemSetDisable(HV),
	(readOnly(Key)->gui:itemSetDisable(1);true).

update(Brush) :-
	forall(mod:brushProp(Prop, Name,_,Type), (brush:get(Brush, Prop, Value), updateItem(Name, Value, Type, Prop))).

updateItem(Name, Value, Type, Prop) :-
	gui:itemSelect(id(Prop, name)),
	getPropBtName(Name, Value, Type, BtName),
	gui:itemSetTxt(BtName).

dlgClose(Brush) :-
	(edi:toolBrush(Brush) -> dlgMenuBar:refresh; true),
	map:refresh.

browsePos(XX, YY) :-
	dlg:getPos(X, Y),
	gui:itemGetX(IX),
	gui:itemGetY(IY),
	XX is X + IX,
	YY is Y + IY + 20.

browse(Prop, Brush) :-
    mod:brushProp(Prop, _, _, Type),
    brush:get(Brush, Prop, Value),
    browse(Prop, Value, Brush, Type).

browse(Prop, Value, Brush, select(Variants)) :-
	browsePos(X, Y),
	gui:createPullDownSelect(X, Y, act(V, dlgProps2:browseSet(V, Prop, Brush)), Variants, Value),
	gui:dlgMoveInBound.


browse(Prop, Color, Brush, color) :-
	browsePos(X, Y),
	dlgColor:create(X, Y, dlgProps2:browseSet(_V, Prop, Brush), Color),
	gui:dlgMoveInBound.


browseSet(V, Prop, Brush) :-
	gui:dlgSelect(props),
	brush:set(Brush, Prop, V),
	gui:itemSelect(id(Prop, edit)),
	mod:brushProp(Prop, _, _, Type),
	propFormat(Type, Format),
	format(string(SZValue), Format, [V]),
	gui:itemSetTxt(SZValue),
	update(Brush).

textToValue(atom, V, V) :- !.
textToValue(color, I, O) :- !,
	format(string(SS), '0x~a', [I]),
	atom_number(SS, O).
textToValue(_, I, O) :- !,
	atom_number(I, O).

inputSet(Prop, Brush) :-
	gui:itemGetTxt(SZValue),
	mod:brushProp(Prop, _, _, Type),
	textToValue(Type, SZValue, Value),
	brush:set(Brush, Prop, Value),
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















