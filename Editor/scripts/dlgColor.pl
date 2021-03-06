:-module(dlgColor, [init/0,
		    usedMax/1,
		    create/4,
		    colors/1]).

usedMax(14).

init :-
	usedMax(Max),
	genColors(Max, 0xffffffff, Colors),
	recorda(colorUsed, Colors),
	recorda(pal, 0).

genColors(0, _, []).
genColors(N, C, [C|Cs]) :-
	NN is N - 1,
	genColors(NN, C, Cs).

colors(Colors) :-
	recorded(colorUsed, Colors).

create(X, Y, Act, Color) :-
	Space = 8,
	DlgW is  128 + Space * 2,
	gui:dlgTitleH(DLGTITLEH),
	DlgH is DLGTITLEH + 128 + Space * 2,
	gui:createDlgTitleModal(X, Y, DlgW, DlgH, "Color"),
	def:dlg(color, ID),
	dlg:setID(ID),
	dlg:setCloseOut,
	dlg:setCloseCmd(dlgColor:dlgClose(Act, Color)),
	gui:addKey(escape > gui:dlgClose),
	gui:addKey(pgup > dlgColor:setPalettePrev),
	gui:addKey(pgdn > dlgColor:setPaletteNext),
	gui:addKey(mouse_wheelup > dlgColor:setPalettePrev),
	gui:addKey(mouse_wheeldn > dlgColor:setPaletteNext),
	gui:alignCode([left, centery], AlignCode),
	gui:itemSetTxtAlign(AlignCode),
%	dlg:setUser(0, Color),

	YY is  DLGTITLEH + 8,
	YY1 is YY - 1,
	gui:createRect( 7, YY1 , 130, 130, gui, true, true),
	gui:createImage( 8, YY, 128, 128),
	def:dlg(item(0), IID),
	gui:itemSetID(IID),
	gui:createItem(cGUIColorPick, 8, YY, 128, 128),
	def:dlg(item(1), IID1),
	gui:itemSetID(IID1),
	gui:itemSetCmdAction(dlgColor:pick(_Param, Act, Color)),

	( core:ini('editor.ini', 'editor', 'color_pal', Pal); Pal = 0),
	(   setPalette(Pal);
	setPalette(0);
	gui:dlgClose(0)).


setPalette(PPal) :-
	(   PPal < 0
	->  Pal = 0
	;   Pal = PPal),

	format(string(ImgName), 'editor\\graphics\\pal~d.tga', [Pal]),
	(   gui:imgFind(ImgName, Img);
	gui:imgLoad(ImgName, Img);false),

	recorded(pal, _, Ref),
	erase(Ref),
	recorda(pal, Pal),
	gui:select(0),
	gui:itemSetImg0(Img),
	gui:select(1),
	gui:itemColorPickLoadImg(ImgName),
	gui:select(title),
	P is Pal + 1,
	format(string(Title), 'Color palette #~d', [P]),
	gui:itemSetTxt(Title).

setPaletteNext :-
	recorded(pal, Pal),
	PN is Pal + 1,
	setPalette(PN).

setPalettePrev:-
	recorded(pal, Pal),
	PN is Pal - 1,
	setPalette(PN).


dlgClose(Act, Color) :-
	recorded(pal, Pal),
	core:ini('editor.ini', 'editor', 'color_pal', Pal),
	term_variables(Act, [Color|_]),
	call(Act).

pick(Param, Act, C) :-
	gui:itemGetColor(ARGB),
	RGB is ARGB /\ 0x00ffffff,
	A is (ARGB /\ 0x000000ff) << 24,
	(   Param =:= 1
	->  C1 is (C /\ 0xff000000) \/ RGB
	;   C1 = C),
	(   Param == 2
	->  C2 is (C1 /\ 0x00ffffff) \/ A
	;   C2 = C1),
	dlg:setCloseCmd(dlgColor:dlgClose(Act, C2)),
	gui:dlgClose(1).


push(C) :-
	recorded(colorUsed, Colors, Ref),
	erase(Ref),
	(select(C, Colors, Other);
	 append(Other, [_], Colors)),
	recorda(colorUsed, [C | Other]).



















