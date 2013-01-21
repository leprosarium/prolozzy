:-module(actions, [options/0,
		   view/0,
		   viewSet/1,
		   tool/0,
		   flip/0,
		   flipSet/1,
		   justFlip/0]).

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
	def:flip(Sel, Code),
	mod:brushProp(flip, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:flipSet(Val)), Select, Sel),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

flipSet(Flip) :-
	edi:getTool(1);
	def:flip(Flip, Code),
	edi:toolBrushSetFlip(Code).

justFlip :-
	edi:getTool(1);
	edi:toolBrushGetFlip(CurFlip),
	def:flip(xy, FLIP_XY),
	def:flip(r, FLIP_R),
	Flip1 is CurFlip /\ FLIP_XY,
	Flip2 is Flip1 + 1,
	(   Flip2 > FLIP_XY
	->  Flip3 = 0
	;   Flip3 = Flip2),
	Flip is (CurFlip /\ FLIP_R) \/ Flip3,
	edi:toolBrushSetFlip(Flip).

