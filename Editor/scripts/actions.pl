:-module(actions, [props/0,
		   menu/0,
		   exit/0,
		   options/0,
		   view/0,
		   viewSet/1,
		   shader/0,
		   shaderSet/1,
		   draw/0,
		   drawSet/1,
		   material/0,
		   materialSet/1,
		   tool/0,
		   flip/0,
		   flipSet/1,
		   justFlip/0,
		   justRotate/0,
		   layer/2,
		   fileNew/0,
		   fileNewDo/0,
		   fileOpenMap/0,
		   fileOpen/0,
		   fileSave/1,
		   fileInfo/0,
		   fileExport/0,
		   tile/0,
		   tileBrowseSet/1,
		   mapping/0,
		   color/0,
		   colorSet/1,
		   colorWin/0,
		   zoomSet/1,
		   search/0,
		   change/0,
		   script/0,
		   script2/0,
		   roomProps/0,
		   toolPickMenu/1,
		   toolCommandPickBrush/1]).


props :-
	edi:getTool(edit);
	dlgProps2:create.

menu :-
	Data = [
	item(file-"File", "", []),
	item(new-" new",       (gui:dlgClose, actions:fileNew), [key(n), tooltip("new map [N]")]),
	item(imap-" open map", (gui:dlgClose, actions:fileOpenMap), [key(o), tooltip("open map [O]")]),
	item(open-" open",   (gui:dlgClose, actions:fileOpen), [key(shift+o), tooltip("open map 2 [Shift+O]")]),
	item(save-" save as",  (gui:dlgClose, actions:fileSave(false)), [key(s), tooltip("save map [S]")]),
	item(export-" export", (gui:dlgClose, actions:fileExport), [key(e), tooltip("export map as image [E]")]),
	item(info-" info",     (gui:dlgClose, actions:fileInfo), [key(i), tooltip("info about current map [I]")]),
	item(editor-"Editor", "", []),
	item(options-" options", (gui:dlgClose, actions:options), [key(p), tooltip("change editor preferences [P]")]),
	item(help-" help",       (gui:dlgClose, actions:help), [key(h), tooltip("open editor help [H]")]),
	item(exit-" exit",	 (gui:dlgClose, actions:exit), [key(x), tooltip("exit editor [X]")])],

	gui:createPullDown( 0,0, Data),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


exit:-
	edi:close.

options :-
	dlgOptions:create.


view :-
	mod:view(Sel),
	findall(Mode-Name, mod:viewMode(Mode, Name), ViewNames),
	gui:createPullDownSelect(0, 0, act(Val, actions:viewSet(Val)), ViewNames, Sel),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


viewSet(View) :-
	mod:setView(View),
	map:refresh.

tool :-
	edi:getTool(Tool),
	nextTool(Tool, NTool),
	edi:setTool(NTool),
	dlgMenuBar:refresh.

nextTool(paint, edit).
nextTool(edit, paint).

flip :-
	edi:getTool(edit);
	edi:toolBrush(B),
	brush:getFlip(B, Code),
	def:flip(_Sel, Code, _),
	mod:brushProp(flip, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:flipSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

flipSet(Code) :-
	edi:getTool(edit);
	def:flip(_Flip, Code, _),
	edi:toolBrush(B),
	brush:setFlip(B, Code).

justFlip :-
	edi:getTool(edit);
	edi:toolBrush(B),
	brush:getFlip(B, CurFlip),
	def:flip(xy, FLIP_XY, _),
	def:flip(r, FLIP_R, _),
	Flip1 is CurFlip /\ FLIP_XY,
	Flip2 is Flip1 + 1,
	(   Flip2 > FLIP_XY
	->  Flip3 = 0
	;   Flip3 = Flip2),
	Flip is (CurFlip /\ FLIP_R) \/ Flip3,
	brush:setFlip(B, Flip).

justRotate :-
	edi:getTool(edit);
	edi:toolBrush(B),
	brush:getFlip(B, Flip),
	def:flip(r, FLIP_R, _),
	NewFlip is Flip xor FLIP_R,
	brush:setFlip(B, NewFlip).

shader :-
	edi:getTool(edit);
	edi:toolBrush(B),
	brush:getShader(B, Code),
	def:shader(_Shader, Code),
	mod:brushProp(shader, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:shaderSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

shaderSet(Code) :-
	edi:getTool(edit);
	def:shader(_Shader, Code),
	edi:toolBrush(B),
	brush:setShader(B, Code).

draw :-
	edi:getTool(edit);
	edi:toolBrush(B),
	brush:getDraw(B, Code),
	def:drawMode(_Draw, Code, _),
	mod:brushProp(draw, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:drawSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


drawSet(Code) :-
	edi:getTool(edit);
	def:drawMode(_Draw, Code, _),
	edi:toolBrush(B),
	brush:setDraw(B, Code).

material :-
	edi:getTool(edit);
	edi:toolBrush(B),
	brush:getMaterial(B, Code),
	def:material(_Mat, Code, _, _),
	mod:brushProp(material, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:materialSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


materialSet(Code) :-
	edi:getTool(edit);
	def:material(_Mat, Code, _, _),
	edi:toolBrush(B),
	brush:setMaterial(B, Code).

layer(Param, Layer) :-
	Layer >= 0,
	def:layerMax(LayerMax),
	Layer < LayerMax,
	layerActivate(Param, Layer),
	dlgMenuBar:layerUpdateButtons,
	map:refresh.
layer(_,_).


layerActivate(1, Layer) :-
	edi:toolBrush(B),
	brush:setLayer(B, Layer).

layerActivate(2, Layer) :-
	edi:toolBrush(B),
	brush:getLayer(B, Layer);

	edi:layerGet(Layer, State),
	(   State =\= 0
	->  NState = 0
	;   NState = 1),
	edi:layerSet(Layer, NState).
layerActivate(_, _).


fileNew :-
	gui:msgBox("Question", "Do you want to create a new map?\n( current map will be lost if not saved )", icon_question, [btn("YES", actions:fileNewDo), btn("NO" , true)]).

mapReset :-
	map:reset,
	roomNames:reset(false),
	brush:eraseAll.


fileNewDo :-
	mapReset,
	fileInfo.

fileOpenMap :-
	edi:toolReset,
	dlgInfo:mapFile(CurFile),
	fileio:fixExt(CurFile, MapFile, map),
	gui:winDlgOpenFile(MapFile, ActFile, map, 0),
	fileio:fixExt(ActFile, NamFile, nam),
	fileio:fixExt(ActFile, PMPFile, pmp),
	dlgInfo:setMapFile(PMPFile),
	mapReset,
	(   \+ gui:waitCall((fileio:mapLoad(ActFile), fileio:roomsLoadNames(NamFile)))
	->  gui:msgBoxOk('Error', 'File open failed.\nFile might be incorrect or damaged.', icon_error)).
fileOpenMap.


fileOpen :-
	edi:toolReset,
	dlgInfo:mapFile(CurFile),
	gui:winDlgOpenFile(CurFile, ActFile, pmp, 0),
	dlgInfo:setMapFile(ActFile),
	mapReset,
	(   \+ gui:waitCall(fileio:mapLoad2(ActFile))
	->  gui:msgBoxOk('Error', 'File open failed.\nFile might be incorrect or damaged.', icon_error)).
fileOpen.

fileSave(Silent) :-
	dlgInfo:defName(DefName),
	dlgInfo:mapFile(CurFile),
	(   (   (\+ Silent; CurFile==DefName), gui:winDlgOpenFile(CurFile, ActFile, pmp, 1))
	;   Silent, CurFile \= DefName, ActFile = CurFile),
	dlgInfo:setMapFile(ActFile),
	(   gui:waitCall(fileio:mapSave(ActFile))
	->  gui:msgBoxOk('Message', 'File save successful.', icon_info)
	;   gui:msgBoxOk('Error', 'File save failed.', icon_error)), !.
fileSave(_).

fileInfo :-
	dlgInfo:create.

tile :-
	(   edi:getTool(paint), \+ edi:tileCount(0)),
	dlgTileBrowse:create(0, 0, actions:tileBrowseSet(_TileID)),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


tileBrowseSet(TileID) :-
	edi:tileFind(TileID, Idx),
	edi:tileGetFrames(Idx, Fr),
	(   Fr =< 0
	->  Frames = 1
	;   Frames = Fr),
	edi:tileGetW(Idx, W),
	edi:tileGetH(Idx, H),
	edi:toolBrush(B),
	brush:setProps(B, [tile=TileID, frame=Frames, x1=0, y1=0, x2=W, y2=H]),
	mapping.

mapping :-
	(edi:getTool(paint), \+ edi:tileCount(0)),
	dlgTileMap:create.

color :-
	edi:getTool(edit);
	edi:toolBrush(B),
	brush:getColor(B, Color),
	dlgColor:create(0, 0, actions:colorSet(_), Color),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


colorSet(C) :-
	edi:toolBrush(B),
	brush:setColor(B, C),
	dlgColor:push(C).

colorWin :-
	edi:toolBrush(B),
	brush:getColor(B, C),
	gui:winDlgOpenColor(C, CN),
	colorSet(CN).


zoomSet(Step) :-
	map:getZoom(CurZoom),
	NewZoom is min(max(CurZoom + Step, 1), 4),
	map:setZoom(NewZoom),
	map:refresh.

search :-
	edi:getTool(paint);
	scripts:brushSearch.


change :-
	edi:getTool(paint);
	scripts:brushChange.

script:-
	edi:getTool(paint);
	Data = [
		item(sel-'Selection', "", []),
		item(inv-' invert', (gui:dlgClose, scripts:brushInvert), [key(i), tooltip('invert selection [I]')]),
		item(move-' move', (gui:dlgClose, scripts:brushMove), [key(m), tooltip('move brushes [M]')]),
		item(seli-' select by index', (gui:dlgClose, scripts:selectByIdx), [tooltip('select a brush by it\'s index')]),
		item(keept-' keep topmost', (gui:dlgClose, scripts:brushKeepTopmost), [tooltip('keep the topmost brush from the current selection')]),
		item(ids-' set group ids', (gui:dlgClose, scripts:brushGroupIds), [tooltip('set group ids for the brushes in the selection')])],
	gui:createPullDown(0, 0, Data),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


script2:-
	edi:getTool(paint);
	Data = [
		item(rc-'Release checks', "", []),
		item(cmt-' check missing tiles', (gui:dlgClose, scripts2:checkTile), [tooltip('select all brushes with missing tiles')]),
		item(cdi-' check duplicate ids', (gui:dlgClose, scripts2:checkId), [tooltip('select all brushes with duplicate ids')]),
		item(co-' check overlapping', (gui:dlgClose, scripts2:checkOverlapping), [tooltip('select overlapping brushes\n(position,size,tile and map)')]),
		item(cdni-' check dynamic ids', (gui:dlgClose, scripts2:checkDynamicBrushId), [tooltip('select all dynamic brushes without valid ids')]),
		item(csi-' check static ids', (gui:dlgClose, scripts2:checkStaticBrushId), [tooltip('select all static brushes with valid ids')]),
		item(cr-' count rooms', (gui:dlgClose, scripts2:countRooms), [tooltip('count and mark all the rooms with brush content')])],
	gui:createPullDown(0, 0, Data),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.




toolPickMenu(Brush) :-
	def:toolCmd(pickBrush, PickBrush),
	def:toolCmd(pickColor, PickColor),
	def:toolCmd(toFront, ToFront),
	def:toolCmd(toBack, ToBack),
	def:toolCmd(delete, Delete),
	Data = [
		item(bprop-bprop,	(gui:dlgClose, dlgBrushProps:create(Brush)), [tooltip("B properties")]),
		item(prop-prop,	(gui:dlgClose, dlgProps2:create(normal, Brush)), [key(p), tooltip("properties [P]")]),
		item(pb-'pick brush',	(gui:dlgClose, edi:toolCommand(PickBrush)), [tooltip("pick brush")]),
		item(pt-'pick tile',	(gui:dlgClose, actions:toolCommandPickBrush(Brush)), [key(t), tooltip("pick tile [T]")]),
		item(pc-'pick color',	(gui:dlgClose, edi:toolCommand(PickColor)), [key(c), tooltip("pick color [C]")]),
		item(tf-'to front',	(gui:dlgClose, edi:toolCommand(ToFront)), [key(f), tooltip("bring to front [F]")]),
		item(tb-'to back',	(gui:dlgClose, edi:toolCommand(ToBack)), [key(b), tooltip("send to back [B]")]),
		item(d-delete,	(gui:dlgClose, edi:toolCommand(Delete)), [key(delete), tooltip("delete [DEL]")])
	       ],
	gui:mouseX(MX),
	gui:mouseY(MY),
	XX is MX + 4,
	YY is MY + 4,
	gui:createPullDown(XX, YY, Data),
	dlg:getRect(X, Y, _, Y2),
	edi:getScrH(ScrH),
	Y3 is ScrH - (Y2 - Y),
	(Y > Y3->gui:dlgMove(X, Y3);true).

toolCommandPickBrush( Brush) :-
	brush:getTile(Brush, Tile),
	brush:getMapX1(Brush, X1),
	brush:getMapY1(Brush, Y1),
	brush:getMapX2(Brush, X2),
	brush:getMapY2(Brush, Y2),
	brush:getFlip(Brush, Flip),

	edi:toolBrush(B),
	brush:setProps(B, [tile=Tile, x1=X1, y1=Y1, x2=X2, y2=Y2, flip=Flip]).

roomProps :-
	edi:getTool(edit);
	edi:toolBrush(B),
	brush:getX(B, X),
	brush:getY(B, Y),
	map:getRoomW(W),
	map:getRoomH(H),
	RX is X // W,
	RY is Y // H,
	dlgRoomProps:create(RX, RY).


fileExport :-
	gui:msgBox('Question', 'Do you want to export a huge image with all the map ?\nIt may take a while. Make sure you have your map saved.', icon_question, [btn('YES', actions:fileExportDo), btn('NO', true)]).


fileExportDo :-
	edi:toolReset,
	dlgInfo:mapFile(CurFile),
	fileio:fixExt(CurFile, PngFile, png),
	gui:winDlgOpenFile(PngFile, ActFile, png, 1),
	(   gui:waitCall(map:saveImage(ActFile))
	->  gui:msgBoxOk('Message', 'Export map image successful.', icon_info)
	;   gui:msgBoxOk('Error', 'Export map image failed.', icon_error)).
fileExportDo.


