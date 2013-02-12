:-module(actions, [props/0,
		   menu/0,
		   exit/0,
		   options/0,
		   view/0,
		   viewSet/1,
		   shader/0,
		   shaderSet/1,
		   type/0,
		   typeSet/1,
		   draw/0,
		   drawSet/1,
		   material/0,
		   materialSet/1,
		   class/0,
		   classSet/1,
		   tool/0,
		   flip/0,
		   flipSet/1,
		   justFlip/0,
		   justRotate/0,
		   layer/1,
		   fileNew/0,
		   fileNewDo/0,
		   fileOpen/0,
		   fileOpen2/0,
		   fileSave/1,
		   fileInfo/0,
		   tile/0,
		   tileBrowseSet/1,
		   mapping/0,
		   color/0,
		   colorSet/1,
		   colorWin/0,
		   zoomSet/1,
		   search/0,
		   toolPickMenu/1,
		   toolCommandPickBrush/1]).


props :-
	edi:getTool(1);
	dlgProps:create.


menu :-
	Data = [
	item(file-"File", "", []),
	item(new-" new",       (gui:dlgClose, actions:fileNew), [key(n), tooltip("new map [N]")]),
	item(open-" open",     (gui:dlgClose, actions:fileOpen), [key(o), tooltip("open map [O]")]),
	item(open2-" open2",   (gui:dlgClose, actions:fileOpen2), [key(shift+o), tooltip("open map 2 [Shift+O]")]),
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
	NTool is 1 - Tool, % toggle tool
	edi:setTool(NTool),
	dlgMenuBar:refresh.

flip :-
	edi:getTool(1);
	edi:toolBrushGetFlip(Code),
	def:flip(_Sel, Code, _),
	mod:brushProp(flip, _, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:flipSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

flipSet(Code) :-
	edi:getTool(1);
	def:flip(_Flip, Code, _),
	edi:toolBrushSetFlip(Code).

justFlip :-
	edi:getTool(1);
	edi:toolBrushGetFlip(CurFlip),
	def:flip(xy, FLIP_XY, _),
	def:flip(r, FLIP_R, _),
	Flip1 is CurFlip /\ FLIP_XY,
	Flip2 is Flip1 + 1,
	(   Flip2 > FLIP_XY
	->  Flip3 = 0
	;   Flip3 = Flip2),
	Flip is (CurFlip /\ FLIP_R) \/ Flip3,
	edi:toolBrushSetFlip(Flip).

justRotate :-
	edi:getTool(1);
	edi:toolBrushGetFlip(Flip),
	def:flip(r, FLIP_R, _),
	NewFlip is Flip xor FLIP_R,
	edi:toolBrushSetFlip(NewFlip).

shader :-
	edi:getTool(1);
	edi:toolBrushGetShader(Code),
	def:shader(_Shader, Code),
	mod:brushProp(shader, _, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:shaderSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

shaderSet(Code) :-
	edi:getTool(1);
	def:shader(_Shader, Code),
	edi:toolBrushSetShader(Code).


type :-
	edi:getTool(1);
	edi:toolBrushGetType(Code),
	def:brushType(_Type, Code),
	mod:brushProp(type, _, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:typeSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


typeSet(Code):-
	edi:getTool(1);
	def:brushType(Type, Code),
	edi:toolBrushSetType(Code),
	mod:brushNew(Type).

draw :-
	edi:getTool(1);
	edi:toolBrushGetDraw(Code),
	def:drawMode(_Draw, Code, _),
	mod:brushProp(draw, _, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:drawSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


drawSet(Code) :-
	edi:getTool(1);
	def:drawMode(_Draw, Code, _),
	edi:toolBrushSetDraw(Code).

material :-
	edi:getTool(1);
	edi:toolBrushGetMaterial(Code),
	def:material(_Mat, Code, _, _),
	mod:brushProp(material, _, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:materialSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


materialSet(Code) :-
	edi:getTool(1);
	def:material(_Mat, Code, _, _),
	edi:toolBrushSetMaterial(Code).


class :-
	edi:getTool(1);
	edi:toolBrushGetClass(Code),
	def:class(_Class, Code),
	mod:brushProp(class, _, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:classSet(Val)), Select, Code),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

classSet(Code) :-
	edi:getTool(1);
	def:class(_Class, Code),
	edi:toolBrushSetClass(Code).

layer(Layer) :-
	gui:itemGetCmdActionParam(Param),
	gui:itemSetCmdActionParam(0),
	Layer >= 0,
	def:layerMax(LayerMax),
	Layer < LayerMax,
	layerActivate(Param, Layer),
	map:refresh.
layer(_).


layerActivate(0, _).
layerActivate(1, Layer) :-
	edi:layerActive(Act),
	(   Act == -1;
	dlgMenuBar:layerSetButton(Act, 1),
	dlgMenuBar:layerSetButton(Layer, 2)).

layerActivate(2, Layer) :-
	edi:layerActive(Layer)
	->  dlgMenuBar:layerSetButton(Layer, 2);
	edi:layerGet(Layer, State),
	(   State =\= 0
	->  NState = 0
	;   NState = 1),
	dlgMenuBar:layerSetButton(Layer, NState).



fileNew :-
	gui:msgBox("Question", "Do you want to create a new map?\n( current map will be lost if not saved )", icon_question, [btn("YES", actions:fileNewDo), btn("NO" , true)]).


fileNewDo :-
	map:reset,
%	RoomNamesReset(0);
%	RoomTextsReset(0);
%	RoomPropsReset(0);
	fileInfo.

fileOpen :-
	edi:toolReset,
	dlgInfo:mapFile(CurFile),
	gui:winDlgOpenFile(CurFile, ActFile, "map", 0),
	dlgInfo:setMapFile(ActFile),
	edi:waitCursor(1),
	(   fileio:mapLoad(ActFile)
	->  edi:waitCursor(0)
	;   edi:waitCursor(0),
	    gui:msgBoxOk("Error", "File open failed.\nFile might be incorrect or damaged.", icon_error)).
fileOpen.


fileOpen2 :-
	edi:toolReset,
	dlgInfo:mapFile(CurFile),
	gui:winDlgOpenFile(CurFile, ActFile, "pmp", 0),
	dlgInfo:setMapFile(ActFile),
	edi:waitCursor(1),
	(   fileio:mapLoad2(ActFile)
	->  edi:waitCursor(0)
	;   edi:waitCursor(0),
	    gui:msgBoxOk("Error", "File open failed.\nFile might be incorrect or damaged.", icon_error)).
fileOpen2.


fileSave(Silent) :-
	dlgInfo:defName(DefName),
	dlgInfo:mapFile(CurFile),
	(   (   (\+ Silent; CurFile==DefName), gui:winDlgOpenFile(CurFile, ActFile, "pmp", 1))
	;   Silent, CurFile \= DefName, ActFile = CurFile),
	dlgInfo:setMapFile(ActFile),
	edi:waitCursor(1),
	(   fileio:mapSave(ActFile)
	->  edi:waitCursor(0),
	    gui:msgBoxOk("Message", "File save successful.", icon_info)
	;   edi:waitCursor(0),
	    gui:msgBoxOk("Error", "File save failed.", icon_error)), !.
fileSave(_).

fileInfo :-
	dlgInfo:create.

tile :-
	(   edi:getTool(0), \+ edi:tileCount(0)),
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
	edi:toolBrushSetTile(TileID),
	edi:toolBrushSetFrame(Frames),
	edi:toolBrushSetMapX1(0),
	edi:toolBrushSetMapY1(0),
	edi:toolBrushSetMapX2(W),
	edi:toolBrushSetMapY2(H),
	mapping.

mapping :-
	(edi:getTool(0), \+ edi:tileCount(0)),
	dlgTileMap:create.

color :-
	edi:getTool(1);
	edi:toolBrushGetColor(Color),
	dlgColor:create(0, 0, actions:colorSet(_), Color),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


colorSet(C) :-
	edi:toolBrushSetColor(C),
	dlgColor:push(C).

colorWin :-
	edi:toolBrushGetColor(C),
	gui:winDlgOpenColor(C, CN),
	colorSet(CN).


zoomSet(Step) :-
	edi:getZoom(CurZoom),
	NewZoom is min(max(CurZoom + Step, 1), 4),
	edi:setZoom(NewZoom),
	map:refresh.

search :-
	edi:getTool(1);
	scrBrushSearch.


toolPickMenu(BrushIdx) :-
	Data = [
		item(prop-prop,	(gui:dlgClose, dlgProps:create(normal, BrushIdx)), [key(p), tooltip("properties [P]")]),
		item(pb-'pick brush',	(gui:dlgClose, def:toolCmd(pickBrush, C), edi:toolCommand(C)), [tooltip("pick brush")]),
		item(pt-'pick tile',	(gui:dlgClose, actions:toolCommandPickBrush(BrushIdx)), [key(t), tooltip("pick tile [T]")]),
		item(pc-'pick color',	(gui:dlgClose, def:toolCmd(pickColor, C), edi:toolCommand(C)), [key(c), tooltip("pick color [C]")]),
		item(tf-'to front',	(gui:dlgClose, def:toolCmd(toFront, C), edi:toolCommand(C)), [key(f), tooltip("bring to front [F]")]),
		item(tb-'to back',	(gui:dlgClose, def:toolCmd(toBack, C), edi:toolCommand(C)), [key(b), tooltip("send to back [B]")]),
		item(d-delete,	(gui:dlgClose, def:toolCmd(delete, C), edi:toolCommand(C)), [key(delete), tooltip("delete [DEL]")])
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

toolCommandPickBrush( BrushIdx ) :-
	map:brushGetTile(BrushIdx, Tile),
	map:brushGetMapX1(BrushIdx, X1),
	map:brushGetMapY1(BrushIdx, Y1),
	map:brushGetMapX2(BrushIdx, X2),
	map:brushGetMapY2(BrushIdx, Y2),
	map:brushGetFlip(BrushIdx, Flip),

	edi:toolBrushSetTile(Tile),
	edi:toolBrushSetMapX1(X1),
	edi:toolBrushSetMapY1(Y1),
	edi:toolBrushSetMapX2(X2),
	edi:toolBrushSetMapY2(Y2),
	edi:toolBrushSetFlip(Flip).
