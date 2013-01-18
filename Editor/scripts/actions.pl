:-module(actions, [options/0,
		  tool/0,
		  flip/0,
		  flipSet/1,
		  justFlip/0]).

options.

tool :-
	edi:getTool(Tool),
	NTool is 1 - Tool, % toggle tool
	edi:setTool(NTool),
	dlgMenuBar:refresh.

flip :-
	edi:getTool(1);
	edi:toolBrushGetFlip(Sel),
	mod:brushProp(flip, _, _, select(Select)),
	gui:createPullDownSelect(0, 0, act(Val, actions:flipSet(Val)), Select, Sel),
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

flipSet(Flip) :-
	edi:getTool(1);
	edi:toolBrushSetFlip(Flip).

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

