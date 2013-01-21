:-module(actions, [options/0,
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
		   layer/1]).

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














