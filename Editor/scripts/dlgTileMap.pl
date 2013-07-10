:-module(dlgTileMap, [
		      create/0
		     ]).

create :-
	edi:tileCount(0), !.
create :-
	edi:toolBrush(B),
	brush:getTile(B, TileID),
	edi:tileFind(TileID, _TileIdx),
	editor:param(tilemap_scale, 2, Scale),
	editor:param(tilemap_snap, 1, Snap),
	editor:param(tilemap_grid, 1, Grid),
	editor:param(tilemap_axes, 0, Axes),
	(Scale < 1 -> SScale = 1; SScale = Scale),
	recorda(params, params(SScale, Snap, Grid, Axes)),
	X = 0,
	Y = 0,
	gui:createDlgTitleModal(X, Y, 128, 128, "Tile Map"),

	def:dlg(tileMap, ID),
	dlg:setID(ID),
	dlg:setCloseOut,
	dlg:setCloseCmd(dlgTileMap:close),
	gui:addKey(equals > dlgTileMap:key(add)),
	gui:addKey(minus > dlgTileMap:key(minus)),
	gui:addKey(add > dlgTileMap:key(add)),
	gui:addKey(subtract > dlgTileMap:key(minus)),
	gui:addKey(pgdn > dlgTileMap:key(pgdn)),
	gui:addKey(pgup > dlgTileMap:key(pgup)),
	gui:addKey(mouse_wheeldn > dlgTileMap:key(pgdn)),
	gui:addKey(mouse_wheelup > dlgTileMap:key(pgup)),
	gui:addKey(home > dlgTileMap:key(home)),
	gui:addKey(end > dlgTileMap:key(end)),
	gui:addKey(escape > gui:dlgClose),
	gui:alignCode([left, centery], Align),
	gui:itemSetTxtAlign(Align),

	gui:itemNew(cGUITile),
	def:dlg(item(0), IID),
	gui:itemSetID(IID),
	gui:itemSetValue(-1),
	gui:itemSetGuiTileScale(SScale),
	def:color(tilebkgr, Color),
	gui:itemSetColor(1, Color),

	gui:itemNew(cGUITileMap),
	def:dlg(item(1), IID1),
	gui:itemSetID(IID1),
	gui:itemSetValue(-1),
	gui:itemSetGuiTileMapScale(SScale),
	gui:itemSetGuiTileMapSnap(Snap),
	gui:itemSetGuiTileMapGrid(Grid),
	gui:itemSetGuiTileMapAxes(Axes),
	brush:getMapX1(B, MapX1),
	brush:getMapY1(B, MapY1),
	brush:getMapX2(B, MapX2),
	brush:getMapY2(B, MapY2),
	gui:itemSetGuiTileMapMap(MapX1, MapY1, MapX2, MapY2),

	createColors(Cur),
	createCur(Cur),

	brush:getTile(B, TileID),
	setTile(TileID),
	update,
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


createColors(Cur) :-
	gui:dlgTitleH(TitleH),
	X = 16,
	Y is TitleH + 6,
	W = 12,
	dlgColor:colors(Colors),
	edi:toolBrush(B),
	brush:getColor(B, BrColor),
	createColors(Colors, X, Y, 0, W, BrColor, Cur).

createColors([], _, _, _, _, _, _).
createColors([C|Cs], X, Y, I, W, BrColor, Cur) :-
	( var(Cur), BrColor == C -> Cur = cur(I, C); true),
	gui:createBar(X, Y, W, W, C),
	Idx is I + 10,
	def:dlg(item(Idx), ID),
	gui:itemSetID(ID),
	gui:styleCode([backgr, border], Style),
	gui:itemSetStyle(Style),
	def:color(black, Color),
	gui:itemSetColor(2, Color),
	gui:itemSetCmdAction(dlgTileMap:setColor(I, C)),
	X2 is X + W,
	I2 is I + 1,
	createColors(Cs, X2, Y, I2, W, BrColor, Cur).

createCur(Cur) :-
	gui:createBar(0, 0, 4, 4, 0xff000000),
	def:dlg(item(9), ID),
	gui:itemSetID(ID),
	(   var(Cur)
	->  gui:itemSetHidden(1)
	;   Cur = cur(I, C),
	    setColor(I, C)).


setColor(I, C) :-
	gui:select(9),
	W = 12,
	gui:dlgTitleH(TitleH),
	X is 16 + I * W + W // 2 - 2,
	Y is TitleH + 6 + W // 2 - 2,
	X2 is X + 4,
	Y2 is Y + 4,
	gui:itemSetRect(X, Y, X2, Y2),
	gui:itemSetHidden(0),
	edi:toolBrush(B),
	brush:setColor(B, C).

setTile(TileID) :-
	edi:toolBrush(B),
	brush:setTile(B, TileID),
	gui:select(0),
	gui:itemSetValue(TileID),
	gui:select(1),
	gui:itemSetValue(TileID),
	gui:select(title),
	edi:tileCount(TC),
	(   edi:tileFind(TileID, IDX)
	->  format(string(Txt), 'Mapping ~d / ~d  id=~d', [IDX, TC, TileID]),
	    edi:tileGetName(IDX, Name)
	;   format(string(Txt), 'Mapping - / ~d  id=~d', [TC, TileID]),
	    Name = ''),
	gui:itemSetTxt(Txt),
	gui:itemSetToolTip(Name).



update :-
	gui:select(0),
	gui:itemGetValue(TileID),
	edi:tileFind(TileID, Idx),
	recorded(params, params(Scale, _Snap, _Grid, _Axes)),
	edi:tileGetW(Idx, TileW),
	edi:tileGetH(Idx, TileH),
	TW is min(TileW, 512),
	TH is min(TileH, 600-32-16),
	Space = 16,
	gui:dlgTitleH(TitleH),
	DlgW is TW * Scale + Space * 2,
	DH is TH * Scale + Space * 2 + TitleH,
	DW is max(DlgW, 200),
	dlg:getPos(X, Y),
	gui:dlgResize(X, Y, DW, DH),
	ItemX is DW // 2 - TW * Scale // 2,
	ItemY is TitleH + 16 + (DH - TitleH - 16) // 2 - TH * Scale // 2,
	gui:select(0),
	X2 is ItemX + TW * Scale,
	Y2 is ItemY + TH * Scale,
	gui:itemSetRect(ItemX, ItemY, X2, Y2),
	gui:itemSetGuiTileScale(Scale),
	gui:select(1),
	gui:itemSetRect(ItemX, ItemY, X2, Y2),
	gui:itemSetGuiTileMapScale(Scale).
update.

close :-
	gui:select(1),
	gui:itemGetGuiTileMapMap(M0, M1, M2, M3),
	(   M2 - M0 > 0,
	    M3 - M1 > 0
	->  edi:toolBrush(B),
	    brush:setProps(B, [x1=M0, y1=M1, x2=M2, y2=M3])
	;   true),
	gui:itemGetGuiTileMapScale(Scale),
	gui:itemGetGuiTileMapSnap(Snap),
	gui:itemGetGuiTileMapGrid(Grid),
	gui:itemGetGuiTileMapAxes(Axes),
	recorded(params, _, Ref),
	erase(Ref),
	editor:param(tilemap_scale, Scale),
	editor:param(tilemap_snap, Snap),
	editor:param(tilemap_grid, Grid),
	editor:param(tilemap_axes, Axes).

key(add) :-
	recorded(params, params(Scale, Snap, Grid, Axes), Ref),
	(   Scale < 4
	->  NS is Scale + 1
	;   NS = Scale),
	erase(Ref),
	recorda(params, params(NS, Snap, Grid, Axes)),
	update.

key(minus) :-
	recorded(params, params(Scale, Snap, Grid, Axes), Ref),
	(   Scale > 1
	->  NS is Scale - 1
	;   NS = Scale),
	erase(Ref),
	recorda(params, params(NS, Snap, Grid, Axes)),
	update.



 key(pgup) :-
	edi:toolBrush(B),
	brush:getTile(B, ID),
	edi:tileFind(ID, IDX),
	IDX2 is IDX - 1,
	(   IDX2 == -1
	->  edi:tileCount(TC),
	    IDX3 is TC - 1
	;   IDX3 = IDX2),
	edi:tileGetID(IDX3, ID2),
	setTile(ID2),
	update.

key(pgdn) :-
	edi:toolBrush(B),
	brush:getTile(B, ID),
	edi:tileFind(ID, IDX),
	edi:tileCount(TC),
	IDX2 is IDX + 1,
	(   IDX2 == TC
	->  IDX3 = 0
	;   IDX3 = IDX2),
	edi:tileGetID(IDX3, ID2),
	setTile(ID2),
	update.
key(home) :-
	edi:tileGetID(0, ID),
	setTile(ID),
	update.
key(end) :-
	edi:tileCount(TC),
	IDX is TC - 1,
	edi:tileGetID(IDX, ID),
	setTile(ID),
	update.


