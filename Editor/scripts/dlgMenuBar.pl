:- module(dlgMenuBar, [create/0]).

create :-
	edi:getScrW(BarW),
	gui:createDlg(0, 0, BarW, 32, [backgr, border3d]),
	def:dlg(menuBar, ID),
	dlg:setID(ID),
	dlg:setTestKey,
	Keys = [
		grave > actMenu,
		alt+1+ctrl > actMenu,
		2 > actView,
		space > actions:tool,
		p > actProps,
		b > actTile,
		m > actMapping,
		f > actions:justFlip,
		r > actJustRotate,
		c > actColor,
		c+shift > actColorWin,
		equals > actZoomSet(1),
		minus > actZoomSet(-1),
		add > actZoomSet(1), % on numpad
		subtract > actZoomSet(-1), % on numpad
		o+ctrl > actFileOpen,
		s+ctrl > actFileSave(1),
		f1 > actHelp,
		f2 > markerGoto(1),
		f2+shift > markerGoto(-1),
		f2+ctrl > markerToggle( 'EdiGet(EDI_CAMX), EdiGet(EDI_CAMY)' ),
		f3 > selectionGoto(1),
		f3+shift > selectionGoto(-1),
		f+ctrl > actSearch,
		r+ctrl > actRoomProps,
		u > actScript3
	       ],
	gui:dlgAddKeys(Keys),
	Btns = [
		btn(menu,	"system menu [`]/[1]", actMenu),
		btn(view,	"view mode [2]", actView),
		btn(tool,	"switch tool [SPACE]", actions:tool),
		%
		btn(props,	"brush properties [P]", actProps),
		btn(tile,	"brush tile [B]", actTile),
		btn(mapping,"brush mapping [M]", actMapping),
		btn(flip,	"brush flip [F]", actions:flip),
		btn(color,	"brush color [C]", actColor),
		btn(shader,	"brush shader", actShader),
		btn(type,	"brush type", actType),
		btn(draw,	"brush draw mode", actDraw),
		btn(material,"brush material", actMaterial),
		btn(class,	"brush class", actClass),
		%
		btn(search,	"search brushes and select", actSearch),
		btn(change,	"change selected brushes", actChange),
		btn(script,	"select scripts", actScript),
		btn(script2,"debug scripts", actScript2),
		btn(script3,"user scripts", actScript3)],
	foreach(member(B, Btns), addButton(B)),
	refresh,
	Size = 16,
	def:layerMax(LAYER_MAX),
	X is BarW - 2 - Size * LAYER_MAX,
	layerCreateButtons(0, LAYER_MAX, X, Size).

layerCreateButtons(LAYER_MAX, LAYER_MAX, _, _).
layerCreateButtons(I, LAYER_MAX, X, Size) :-
	layerCreateButton(I, X, 2),
	X2 is X + Size,
	I2 is I + 1,
	layerCreateButtons(I2, LAYER_MAX, X2, Size).





% utils

addButton(btn(ID, Tooltip, Cmd)) :-
	def:dlg(menuBar, MB),
	dlg:find(MB, IDX),
	dlg:select(IDX),
	BarH = 32,
	format(string(ImgPath1), 'Editor\\Graphics\\mb1_~a.tga', [ID]),
	format(string(ImgPath2), 'Editor\\Graphics\\mb2_~a.tga', [ID]),
	gui:imgLoad(ImgPath1, Img1),
	gui:imgLoad(ImgPath2, Img2),
	gui:createButtonImg(0, 0, BarH, BarH, Img1, Img2, Cmd),
	def:mb(_, ID, ItemID),
	gui:itemSetID(ItemID),
	gui:itemSetStyle(0),
	gui:itemSetToolTip(Tooltip).

% visibility dependences and repositioning
refresh :-
	edi:getTool(Tool),
	foreach(def:mb(t1, _, ID1), showButton(ID1, Tool)),
	NTool is 1 - Tool,
	foreach(def:mb(t2, _, ID2), showButton(ID2, NTool)).


showButton(Id, Show) :-
	def:dlg(menuBar, MB),
	dlg:find(MB, IDX),
	dlg:select(IDX),
	gui:itemFind(Id, Idx),
	gui:itemSelect(Idx),
	gui:itemSetHidden(Show),
	gui:itemSetDisable(Show),
	reposition.


% reposition the visible buttons
reposition :-
	findall(ID, def:mb(_, _, ID), IDs),
	reposition(IDs, 0, 32).

reposition([], _, _).
reposition([ID|IDs], X, BarH) :-
	gui:itemFind(ID, IDX),
	gui:itemSelect(IDX),
	(   gui:itemGetHidden
	->  reposition(IDs, X, BarH)
	;   (X1 is X + BarH,
	    gui:itemSetRect(X, 0, X1, BarH),
	     reposition(IDs, X1, BarH))
	).

layerCreateButton(Layer, X, Y) :-
	format(string(LayerText), '~d', [Layer]),
	gui:createItem("cGUICheck", X, Y, 16, 28, [backgr, border3d], [text(LayerText), color(0, layer1), color(1, layer1), color(2, gui)]),
	def:mb(layer, ID_MB_LAYER),
	ID is ID_MB_LAYER + Layer,
	gui:itemSetID(ID),
	gui:itemSetValue(1),
	format(string(Action), 'actLayer(~d)', [Layer]),
	gui:itemSetCmdAction(Action).

