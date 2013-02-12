:- module(dlgOptions, [load/0,
		      create/0,
		      close/0]).

getOpt(Opt, Def, Val) :-
	format(string(Key), 'options_~a', [Opt]),
	editor:param(Key, Def, Val).


load :-
	getOpt(axes, 0, Axes ),
	getOpt(grid, 1, Grid ),
	getOpt(snap, 1, Snap ),
	getOpt(roomgrid, 0, Roomgrid),
	getOpt(brushrect, 0, Brushrect),
	def:roomW(DefRoomW),
	getOpt(roomw, DefRoomW, Roomw),
	def:roomH(DefRoomH),
	getOpt(roomh, DefRoomH, Roomh),
	def:color(map, DefColor),
	getOpt(colormap, DefColor, Color),
	getOpt(colortheme, 0, Theme),

	edi:setAxes(Axes),
	edi:setGrid(Grid),
	edi:setSnap(Snap),
	edi:setRoomGrid(Roomgrid),
	edi:setBrushRect(Brushrect),
	edi:setRoomW(Roomw),
	edi:setRoomH(Roomh),
	edi:setColorMap(Color),
	def:setColorTheme(Theme).



create :-
	gui:createDlgTitleModal(0, 0, 340, 354, "Options"),
	def:dlg(options, ID),
	dlg:setID(ID),
	dlg:setCloseOut,
	dlg:setCloseCmd(dlgOptions:close),
	gui:addKey(escape > (dlgOptions:close, gui:dlgClose)),
	gui:dlgTitleH(TITLEH),
	X0 = 16,
	Y0 is TITLEH + 16,
	DX = 6,
	DY = 6,
	DDX is DX + 20,
	DDY is DY + 20,
	createBox1(X0, Y0, DDX, DDY),
	createBox2(180, Y0, DDX, DDY),
	Y4 is Y0 + 184,
	createBox4(X0, Y4, DDY),
	Y5 is Y0 + 184+64+8,
	createBox5(X0, Y5, DDX, DDY),
	gui:addKey(r > dlgOptions:reloadTiles),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.



createBox1(X0, Y0, DDX, DDY) :-
	X = X0,
	Y = Y0,

	% visibility
	XX0 is X - 8,
	YY0 is Y - 8,
	gui:createBar(XX0, YY0, 156, 176, gui1),

	% axes
	edi:getAxes(Axes),
	gui:createCheck(X, Y, Axes),
	def:dlg(item, ID0),
	gui:itemSetID(ID0),
	XX1 is X + DDX,
	gui:createText( XX1, Y, 100, "show axes"),
	Y1 is Y + DDY,

	% grid
	edi:getGrid(Grid),
	gui:createCheck(X, Y1, Grid),
	def:dlg(item(1), ID1),
	gui:itemSetID(ID1),
	gui:createText(XX1, Y1, 100, "show grid"),
	Y2 is Y1 + DDY,

	% snap
	edi:getSnap(Snap),
	gui:createCheck(X, Y2, Snap),
	def:dlg(item(2), ID2),
	gui:itemSetID(ID2),
	gui:createText(XX1, Y2, 100, "snap to grid" ),
	Y3 is Y2 + DDY,
	% room grid
	edi:getRoomGrid(RoomGrid),
	gui:createCheck(X, Y3, RoomGrid),
	def:dlg(item(3), ID3),
	gui:itemSetID(ID3),
	gui:createText(XX1, Y3, 100, "show room grid" ),
	Y4 is Y3 + DDY,
	% brush rect
	edi:getBrushRect(BrushRect),
	gui:createCheck(X, Y4, BrushRect ),

	def:dlg(item(4), ID4),
	gui:itemSetID(ID4),
	gui:createText(XX1, Y4, 100, "show brush rects" ),
	Y5 is Y4 + DDY,

	% room info
	mod:roomInfo(Info),
	mod:roomInfoName(Info, InfoName),
	gui:createButton(X, Y5, 140, InfoName, dlgOptions:roomInfo),
	def:dlg(item(5), ID5),
	gui:itemSetID(ID5),
	gui:itemSetToolTip("choose what kind of room info\nis displayed in the info bar.").


createBox2(X0, Y0, DDX, DDY) :-

% screensize
	X = X0,
	Y = Y0,
	XX0 is X - 8,
	YY0 is Y - 8,
	gui:createBar(XX0, YY0, 160, 176, gui1),
	(core:ini('editor.ini', 'editor', 'options_screensize', ScreenSize);
	ScreenSize = 1),

	gui:createText(X, Y, 100, "Screen Size"),
	Y1 is Y + DDY,
	createScreenSizeRadios(X, Y1, DDX, DDY, ScreenSize, ['small (640x480)', 'medium (800x600)', 'large (1024x768)', 'huge (1280x1024)'], YL),
	gui:createText(X, YL, 140, "(need restart to apply)").

createScreenSizeRadios(X, Y, DDX, DDY, Val, Names, YL) :-
	X1 is X + DDX,
	createScreenSizeRadios(Names, X, X1, Y, DDY, 10, 0, Val, YL).

createScreenSizeRadios([], _, _, Y, _, _, _, _, Y).
createScreenSizeRadios([Name|Names], X, X1, Y, DDY, ID, N, Val, YL) :-
	createScreenSizeRadio(Name, X, X1, Y, ID, N, Val),
	Y2 is Y + DDY,
	ID2 is ID + 1,
	N2 is N + 1,
	createScreenSizeRadios(Names, X, X1, Y2, DDY, ID2, N2, Val, YL).
createScreenSizeRadio(Name, X, X1, Y, ID, N, Val) :-
	(   N == Val
	->  gui:createRadio(X, Y, 1, 1 )
	;   gui:createRadio(X, Y, 0, 1 )),
	def:dlg(item(ID), IDN),
	gui:itemSetID(IDN),
	gui:createText(X1, Y, 100, Name).


createBox4(X, Y, DDY) :-
	XX is X - 8,
	YY is Y - 8,
	W is 172 + 160 - 8,

	gui:createBar(XX, YY, W, 64, gui1),
	(core:ini('editor.ini', 'editor', 'options_tiledir', TileFolder);
	 TileFolder = ''),
	gui:createText(X, Y, 100, "Tiles Folder"),
	XB is X + 244,
	gui:createButton( XB, Y, 64, "reload", dlgOptions:reloadTiles),
	gui:itemSetToolTip("reload tiles from\nthe selected folder [R]"),
	Y1 is Y + DDY,
	gui:createEdit(X, Y1, 282, TileFolder, dlgOptions:setTileFolder),
	def:dlg(item(20), ID20),
	gui:itemSetID(ID20),
	gui:itemSetUser(1, 255),
	gui:itemBuild, % resize editbox
	XB2 is X + 288,
	gui:createButton(XB2, Y1, 20, "...", dlgOptions:browseTileFolder),
	gui:itemSetToolTip("browse tiles folder").


createBox5(X, Y, DDX, DDY) :-
	XX is X - 8,
	YY is Y - 8,

	W is 172+160-8,
	H is 34+28,

	gui:createBar(XX, YY, W, H, gui1),
	gui:createText(X, Y, 100, "Map background color"),
	XB is X + 244,
	gui:createButton(XB, Y, 64, '', dlgOptions:browseColor),
	def:dlg(item(30), ID30),
	gui:itemSetID(ID30),
	gui:styleCode([backgr, border3d], StyleCode),
	gui:itemSetStyle(StyleCode),
	edi:getColorMap(ColorMap),
	ColorMapV is ColorMap \/ 0xff000000,
	gui:itemSetColor(0, ColorMapV),
	gui:itemSetToolTip("change map color"),
	Y1 is Y + DDY,

	(core:ini('editor.ini', 'editor', 'options_colortheme', ColorTheme);
	 ColorTheme = 0),
	gui:createText(X, Y1, 100, "Editor color theme"),
	XB2 is X + 244,
	gui:createButton(XB2, Y1, 64, '', dlgOptions:browseColorTheme),
	def:dlg(item(31), ID31),
	gui:itemSetID(ID31),
	gui:styleCode([backgr, border3d], StyleCode),
	gui:itemSetStyle(StyleCode),
	def:colorTheme(ColorTheme, sample, Color),
	gui:itemSetColor(0, Color),
	gui:itemSetToolTip("change editor color preferences").


roomInfo :-
	findall(item(Info-Name,	(gui:dlgClose, dlgOptions:roomInfoSet(Info)), []), mod:roomInfoName(Info, Name), Menu),
	mod:roomInfo(Info),
	gui:createPullDown(0, 0, Menu, Info),
	gui:dlgMoveToMouse.


roomInfoSet(Info):-
	mod:setRoomInfo(Info),
	def:dlg(options, MB),
	dlg:find(MB, IDX),
	dlg:select(IDX),
	gui:select(5),
	mod:roomInfoName(Info, Name),
	gui:itemSetTxt(Name).


reloadTiles :-
	edi:tileReload, !,
	edi:tileCount(Cnt),
	format(string(Msg), 'Tiles reloading successful.\nLoaded ~d tiles.', [Cnt]),
	gui:msgBoxOk("Message", Msg, icon_info).

reloadTiles :- !,
	gui:msgBoxOk("Error", "Tiles reloading failed.\nCheck the path to the tiles folder.", icon_error).

setTileFolder :-
	gui:itemGetTxt(TileFolder),
	core:ini('editor.ini', 'editor', 'options_tiledir', TileFolder).

browseTileFolder :-
	gui:select(20),
	gui:itemGetTxt(TileFolder),
	gui:winDlgOpenFolder( TileFolder, NewTileFolder), !,
	gui:itemSetTxt(NewTileFolder),
	setTileFolder.

browseTileFolder :- !.


browseColor :-
	edi:getColorMap(ColorMap),
	Color is ColorMap \/ 0xff000000,
	dlgColor:create(0, 0, dlgOptions:colorSet(_), Color),
	gui:dlgMoveToMouse.


browseColorTheme :-
	(core:ini('editor.ini', 'editor', 'options_colortheme', C);
	 C = 0),
	NC is C + 1,
	(   def:colorTheme(NC, _, _)
	->  NCC = NC
	;   NCC = 0),
	def:colorTheme(NCC, sample, Color),
	core:ini('editor.ini', 'editor', 'options_colortheme', NCC),

	def:dlg(options, ID),
	dlg:find(ID, IDX),
	dlg:select(IDX),
	gui:select(31),
	gui:itemSetColor(0, Color).


colorSet(Color) :-
	C is Color \/ 0xff000000,
	def:dlg(options, ID),
	dlg:find(ID, IDX),
	dlg:select(IDX),
	gui:select(30),
	gui:itemSetColor(0, C),
	edi:setColorMap(C),
	map:refresh.


getsetoption(Opt) :-
	(core:ini('editor.ini', 'editor', Opt, Var); Var = 0),
	core:ini('editor.ini', 'editor', Opt, Var).



getValue(Item, Val) :-
	gui:select(Item),
	gui:itemGetValue(Val).

close :-
	getsetoption('options_videoapi'),
	getsetoption('options_cool'),

	getValue(0, Axes),
	edi:setAxes(Axes),
	core:ini('editor.ini', 'editor', 'options_axes', Axes),

	getValue(1, Grid),
	edi:setGrid(Grid),
	core:ini('editor.ini', 'editor', 'options_grid', Grid),

	getValue(2, Snap),
	edi:setSnap(Snap),
	core:ini('editor.ini', 'editor', 'options_snap', Snap),

	getValue(3, RoomGrid),
	edi:setRoomGrid(RoomGrid),
	core:ini('editor.ini', 'editor', 'options_roomgrid', RoomGrid),

	getValue(4, BrushRect),
	edi:setBrushRect(BrushRect),
	core:ini('editor.ini', 'editor', 'options_brushrect', BrushRect),

	(   getValue(10, 1), ScreenSize = 0;
	getValue(11, 1), ScreenSize = 1;
	getValue(12, 1), ScreenSize = 2;
	getValue(13, 1), ScreenSize = 3),
	core:ini('editor.ini', 'editor', 'options_screensize', ScreenSize),

	gui:select(20),
	gui:itemGetTxt(TileFolder),
	core:ini('editor.ini', 'editor', 'options_tiledir', TileFolder),

	gui:select(30),
	gui:itemGetColor(Color),
	core:ini('editor.ini', 'editor', 'options_colormap', Color),

	map:refresh,
	(   edi:tileCount(0)
	->  gui:msgBoxOk('Warning', 'No tiles loaded.\nCheck the path and the tiles folder.', icon_warning);
	true).






