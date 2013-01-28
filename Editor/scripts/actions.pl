:-module(actions, [menu/0,
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
		   layer/1,
		   fileNew/0,
		   fileNewDo/0,
		   fileOpen/0,
		   fileOpen2/0,
		   fileSave/1,
		   fileInfo/0]).

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

options.


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
	def:flip(Sel, Code, _),
	mod:brushProp(flip, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:flipSet(Val)), Select, Sel),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

flipSet(Flip) :-
	edi:getTool(1);
	def:flip(Flip, Code, _),
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


shader :-
	edi:getTool(1);
	edi:toolBrushGetShader(Code),
	def:shader(Shader, Code, _),
	mod:brushProp(shader, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:shaderSet(Val)), Select, Shader),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

shaderSet(Shader) :-
	edi:getTool(1);
	def:shader(Shader, Code, _),
	edi:toolBrushSetShader(Code).


type :-
	edi:getTool(1);
	edi:toolBrushGetType(Code),
	def:brushType(Type, Code, _),
	mod:brushProp(type, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:typeSet(Val)), Select, Type),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


typeSet(Type):-
	edi:getTool(1);
	def:brushType(Type, Code, _),
	edi:toolBrushSetType(Code),
	mod:brushNew(Type).

draw :-
	edi:getTool(1);
	edi:toolBrushGetDraw(Code),
	def:drawMode(Draw, Code, _),
	mod:brushProp(draw, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:drawSet(Val)), Select, Draw),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


drawSet(Draw) :-
	edi:getTool(1);
	def:drawMode(Draw, Code, _),
	edi:toolBrushSetDraw(Code).

material :-
	edi:getTool(1);
	edi:toolBrushGetMaterial(Code),
	def:material(Mat, Code, _, _),
	mod:brushProp(material, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:materialSet(Val)), Select, Mat),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.


materialSet(Mat) :-
	edi:getTool(1);
	def:material(Mat, Code, _, _),
	edi:toolBrushSetMaterial(Code).


class :-
	edi:getTool(1);
	edi:toolBrushGetClass(Code),
	def:class(Class, Code, _),
	mod:brushProp(class, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:classSet(Val)), Select, Class),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

classSet(Class) :-
	edi:getTool(1);
	def:class(Class, Code, _),
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









