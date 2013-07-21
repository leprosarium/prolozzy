:-module(dlgBrushProps, [create/1, dlgClose/1]).

create(Brush) :-
	format(string(Title), '~p', [Brush]),
	gui:dlgTitleH(TitleH),
	gui:createDlgTitleModal(0, 0, 216, 100, Title),
	def:dlg(brushProps, ID),
	dlg:setID(ID),
	gui:addKey(escape > gui:dlgClose),
	dlg:setCloseOut,
	dlg:setCloseCmd(dlgBrushProps:dlgClose(Brush)),
	Y0 is TitleH + 8,
	gui:styleCode([backgr, border], Style),
	gui:createText(8, Y0, 64, 'Property'),
	gui:itemSetStyle(Style),
	gui:itemSetProps([color(0, gui1), color(1, gui1), color(2, black)]),
	gui:createText(76, Y0, 136, 'Value'),
	gui:itemSetStyle(Style),
	gui:itemSetProps([color(0, gui1), color(1, gui1), color(2, black)]),
	YP is Y0 + 22,
	createProps(Brush, 8, YP, 22, Y),
	gui:dlgResize(0, 0, 216, Y),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

%extendProps([], N, NP) :-
%	NN is N - 1,
%	extendProps([name-''], NN, NP).

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

createProps(Br, X0, Y0, H, YY) :-
	brush:get(Br, Props),
	extendProps(Props, 20, NProps),
	createProps(NProps, 0, X0, Y0, H, YY).

createProps([], _, _, Y, _, Y).
createProps([P|Ps], I, X, Y, H, YY) :-
	createProp(P, I, X, Y),
	I2 is I + 1,
	Y2 is Y + H,
	createProps(Ps, I2, X, Y2, H, YY).

createProp(Prop-Val, I, X, Y) :-
	gui:createEdit(X, Y, 64, Prop),
	gui:itemSetID(id(I,name)),
	X2 is X + 68,
	term_to_atom(Val, VV),
	gui:createEdit(X2, Y, 136, VV),
	gui:itemSetID(id(I,value)).

dlgClose(Br):-
	findall(Key-Val, (gui:itemSelect(id(N,name)),
			  gui:itemGetTxt(Key),
			  Key \= '',
			  gui:itemSelect(id(N,value)),
			  gui:itemGetTxt(V),
			  atom_to_term(V, Val, _)), Props),
	brush:set(Br, Props).








