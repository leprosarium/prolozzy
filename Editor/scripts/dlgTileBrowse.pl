:- module(dlgTileBrowse, [create/3,
			 close/0]).


create(X, Y, Cmd) :-
	edi:tileCount(0);
	editor:param(tilebrowse_first, 0, First),
	editor:param(tilebrowse_cols, 4, Cols),
	editor:param(tilebrowse_rows, 4, Rows),
	editor:param(tilebrowse_size, 64, Size),
	recorda(params, params(First, Cols, Rows, Size)),
	gui:dlgTitleH(TITLEH),

	TilesX = Cols,
	TilesY = Rows,
	TileSize = Size,
	Space = 5,

	W is TilesX * TileSize + (TilesX + 1) * Space,
	H is TilesY * TileSize + (TilesY + 1) * Space + TITLEH + Space,
	gui:createDlgTitleModal( X, Y, W, H, "Tile Browse"),
	def:dlg(tileBrowse, DID),
	dlg:setID(DID),
	dlg:setCloseOut,
	dlg:setCloseCmd(dlgTileBrowse:close),
	gui:addKey(pgdn > dlgTileBrowse:key(pgdn, Cmd)),
	gui:addKey(pgup > dlgTileBrowse:key(pgup, Cmd)),
	gui:addKey(mouse_wheeldn > dlgTileBrowse:key(pgdn, Cmd)),
	gui:addKey(mouse_wheelup > dlgTileBrowse:key(pgup, Cmd)),
	gui:addKey(down > dlgTileBrowse:key(down, Cmd)),
	gui:addKey(up > dlgTileBrowse:key(up, Cmd)),
	gui:addKey(home > dlgTileBrowse:key(home, Cmd)),
	gui:addKey(end > dlgTileBrowse:key(end, Cmd)),
	gui:addKey(escape > gui:dlgClose),
	gui:alignCode([left, centery], Align),
	gui:itemSetTxtAlign(Align),

	CelX is	TileSize + Space,
	CelY is TileSize + Space,
	X0 = Space,
	Y0 is Space + TITLEH,
	forall(cells(I, J, TilesX, TilesY, IDX), createCell(IDX, I, J, X0, Y0, CelX, CelY, TileSize)),
	update(Cmd).
close :-
	recorded(params, params(First, Cols, Rows, Size), Ref),
	erase(Ref),
	editor:param(tilebrowse_first, First),
	editor:param(tilebrowse_cols, Cols),
	editor:param(tilebrowse_rows, Rows),
	editor:param(tilebrowse_size, Size).


cells(I, J, X, Y, IDX) :-
	Y0 is Y - 1,
	X0 is X - 1,
	between(0, Y0, J),
	between(0, X0, I),
	IDX is J * X + I.

createCell(IDX, I, J, X0, Y0, CelX, CelY, TileSize) :-
	X is I * CelX + X0,
	Y is J * CelY + Y0,
	XX is X - 1,
	YY is Y - 1,
	W is TileSize + 2,
	gui:createRect(XX, YY, W, W, gui, true, true),
	gui:createItem(cGUITile, X, Y, TileSize, TileSize),
	def:dlg(item(IDX), ID),
	gui:itemSetID(ID),
	gui:itemSetValue(-1),
	def:color(tilebkgr, Color),
	gui:itemSetColor(1, Color),
	gui:itemSetGuiTileScale(1),
	gui:itemSetGuiTileShrink(1).


key(K, Cmd) :-
	recorded(params, params(First, TilesX, TilesY, Size), Ref),
	next(K, TilesX, TilesY, First, NewFirst),
	fix(TilesX, TilesY, NewFirst, FixedFirst),
	erase(Ref),
	recorda(params, params(FixedFirst, TilesX, TilesY, Size)),
	update(Cmd).

next(pgdn, X, Y, F, NF) :-
	NF is F + X * Y.
next(pgup, X, Y, F, NF) :-
	NF is F - X * Y.
next(down, X, _Y, F, NF) :-
	NF is F + X.
next(up, X, _Y, F, NF) :-
	NF is F - X.
next(home, _X, _Y, _F, 0).
next(end, X, Y, _F, NF) :-
	edi:tileCount(TC),
	NF is TC - X * Y.

fix(X, Y, F, N) :-
	edi:tileCount(TC),
	N is max(min(TC - X * Y, F), 0).
update(Cmd) :-
	recorded(params, params(First, TilesX, TilesY, _Size)),
	forall(cells(_I, _J, TilesX, TilesY, IDX), updateCell(IDX, First, Cmd)),
	gui:select(title),
	edi:tileCount(TC),
	format(string(Txt), 'Browse  ~d / ~d', [First, TC]),
	gui:itemSetTxt(Txt).


filledCell(IDX, First) :-
	edi:tileCount(TC),
	Pos is First + IDX,
	Pos >= 0,
	Pos < TC.

updateCell(IDX, First, Cmd) :-
	gui:select(IDX),
	(   filledCell(IDX, First)
	->  updateFilledCell(First, IDX, Cmd)
	;   updateEmptyCell).


updateEmptyCell :-
	gui:itemSetValue(-1),
	gui:itemSetCmdAction(''),
	gui:itemSetToolTip('').

makeAction(Cmd, ID, Action) :-
	copy_term(Cmd, Action),
	term_variables(Action, [ID|_]).

updateFilledCell(First, IDX, Cmd)  :-
	Pos is First + IDX,
	edi:tileGetName(Pos, Name),
	edi:tileGetID(Pos, ID),
	edi:tileGetW(Pos, W),
	edi:tileGetH(Pos, H),
	edi:tileGetFrames(Pos, Frames),
	gui:itemSetValue(ID),
	makeAction(Cmd, ID, Action),
	gui:itemSetCmdAction((gui:dlgClose,Action)),
	format(string(ToolTip), 'id=~d w=~d h=~d frames=~d name=~a', [ID, W, H, Frames, Name]),
	gui:itemSetToolTip(ToolTip).









