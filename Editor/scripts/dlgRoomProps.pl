:-module(dlgRoomProps, [create/2, dlgClose/2]).

create(RX, RY) :-
	format(string(Title), 'ROOM_~d_~d', [RX, RY]),
	gui:dlgTitleH(TitleH),
	gui:createDlgTitleModal(0, 0, 216, 100, Title),
	def:dlg(roomProps, ID),
	dlg:setID(ID),
	gui:addKey(escape > gui:dlgClose),
	dlg:setCloseOut,
	dlg:setCloseCmd(dlgRoomProps:dlgClose(RX, RY)),
	Y0 is TitleH + 8,
	gui:styleCode([backgr, border], Style),
	gui:createText(8, Y0, 64, 'Property'),
	gui:itemSetStyle(Style),
	gui:itemSetProps([color(0, gui1), color(1, gui1), color(2, black)]),
	gui:createText(76, Y0, 136, 'Value'),
	gui:itemSetStyle(Style),
	gui:itemSetProps([color(0, gui1), color(1, gui1), color(2, black)]),
	YP is Y0 + 22,
	createProps(room(RX, RY), 8, YP, 22, Y),
	gui:dlgResize(0, 0, 216, Y),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

extendProps([], N, NP) :-
	NN is N - 1,
	extendProps([name-''], NN, NP).

extendProps(P, Max, NP) :-
	length(P, Num),
	(   Num < Max
	->  N is Max - Num,
	    appendEmpty(P, N, NP)
	;   NP = P).

appendEmpty(Props, N, NProps) :-
	length(Emp, N),
	fill(Emp),
	append(Props, Emp, NProps).
fill([]).
fill([''-''|O]) :- fill(O).

createProps(room(RX, RY), X0, Y0, H, YY) :-
	roomNames:get(RX, RY, Props),
	extendProps(Props, 10, NProps),
	createProps(NProps, 0, X0, Y0, H, YY).

createProps([], _, _, Y, _, Y).
createProps([P|Ps], I, X, Y, H, YY) :-
	createProp(P, I, X, Y),
	I2 is I + 1,
	Y2 is Y + H,
	createProps(Ps, I2, X, Y2, H, YY).

createProp(Prop-Val, I, X, Y) :-
	gui:createEdit(X, Y, 64, Prop),
	II is I * 2,
	def:dlg(item(II), ID0),
	gui:itemSetID(ID0),
	gui:itemSetUser(1, 128),
	gui:itemBuild,
	X2 is X + 68,
	term_to_atom(Val, VV),
	gui:createEdit(X2, Y, 136, VV),
	II1 is II + 1,
	def:dlg(item(II1), ID1),
	gui:itemSetID(ID1),
	gui:itemSetUser(1, 128),
	gui:itemBuild.


dlgClose(RX, RY):-
	collectProps(0, Props),
	roomNames:set(RX, RY, Props).

collectProps(N, Props) :-
	(   gui:select(N)
	->  gui:itemGetTxt(Key),
	    (	Key == ''
	    ->	N2 is N + 2,
		collectProps(N2, Props)
	    ;	N1 is N + 1,
		gui:select(N1),
		gui:itemGetTxt(V),
		atom_to_term(V, Val, _),
		N2 is N1 + 1,
		collectProps(N2, Other),
		Props = [Key-Val|Other])
	;   Props = []).














