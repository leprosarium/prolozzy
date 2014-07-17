:- module(dlgMenuBar, [create/0,
		      layerUpdateButtons/0]).

create :-
	edi:getScrW(BarW),
	gui:createDlg(0, 0, BarW, 32, [backgr, border3d]),
	def:dlg(menuBar, ID),
	dlg:setID(ID),
	dlg:setTestKey,
	Keys = [
		grave > actions:menu,
		1 > actions:menu,
		2 > actions:view,
		space > actions:tool,
		p > actions:props,
		b > actions:tile,
		m > actions:mapping,
		f > actions:justFlip,
		r > actions:justRotate,
		c > actions:color,
		c+shift > actions:colorWin,
		equals > actions:zoomSet(1),
		minus > actions:zoomSet(-1),
		add > actions:zoomSet(1), % on numpad
		subtract > actions:zoomSet(-1), % on numpad
		o+ctrl > actions:fileOpen,
		s+ctrl > actions:fileSave(true),
		f1 > actHelp,
		f3 > selection:goto(1),
		f3+shift > selection:goto(-1),
		f+ctrl > actions:search,
		r+ctrl > actions:roomProps,
		u > actScript3
	       ],
	gui:dlgAddKeys(Keys),
	Btns = [
		btn(menu,	"system menu [`]/[1]", actions:menu),
		btn(view,	"view mode [2]", actions:view),
		btn(tool,	"switch tool [SPACE]", actions:tool),
		%
		btn(props,	"brush properties [P]", actions:props),
		btn(tile,	"brush tile [B]", actions:tile),
		btn(mapping,    "brush mapping [M]", actions:mapping),
		btn(flip,	"brush flip [F]", actions:flip),
		btn(color,	"brush color [C]", actions:color),
		btn(shader,	"brush shader", actions:shader),
%		btn(type,	"brush type", actions:type),
		btn(draw,	"brush draw mode", actions:draw),
		btn(material,   "brush material", actions:material),
%		btn(class,	"brush class", actions:class),
		%
		btn(search,	"search brushes and select", actions:search),
		btn(change,	"change selected brushes", actions:change),
		btn(script,	"select scripts", actions:script),
		btn(script2,    "debug scripts", actions:script2),
		btn(script3,	"user scripts", actScript3)],
	foreach(member(B, Btns), addButton(B)),
	Size = 16,
	def:layerMax(LAYER_MAX),
	X is BarW - 2 - Size * LAYER_MAX,
	layerCreateButtons(0, LAYER_MAX, X, Size),
	refresh.

layerCreateButtons(LAYER_MAX, LAYER_MAX, _, _).
layerCreateButtons(I, LAYER_MAX, X, Size) :-
	layerCreateButton(I, X, 2),
	X2 is X + Size,
	I2 is I + 1,
	layerCreateButtons(I2, LAYER_MAX, X2, Size).





% utils

addButton(btn(ID, Tooltip, Cmd)) :-
	def:dlg(menuBar, MB),
	dlg:select(MB),
	BarH = 32,
	format(string(ImgPath1), 'editor\\graphics\\mb1_~a.tga', [ID]),
	format(string(ImgPath2), 'editor\\graphics\\mb2_~a.tga', [ID]),
	gui:imgLoad(ImgPath1, Img1),
	gui:imgLoad(ImgPath2, Img2),
	gui:createButtonImg(0, 0, BarH, BarH, Img1, Img2, Cmd),
	def:mb(_, ID, ItemID),
	gui:itemSetID(ItemID),
	gui:itemSetStyle(0),
	gui:itemSetToolTip(Tooltip).

% visibility dependences and repositioning
refresh :-
	layerUpdateButtons,
	edi:getTool(Tool),
	def:dlg(menuBar, MB),
	dlg:select(MB),
	hideToolButtons(Tool, 0),
	actions:nextTool(Tool, NTool),
	hideToolButtons(NTool, 1),
	reposition.

hideToolButtons(Tool, Hide) :-
	foreach(def:mb(Tool, _, Id), hideButton(Id, Hide)).

hideButton(Id, Hide) :-
	gui:itemSelect(Id),
	gui:itemSetHidden(Hide),
	gui:itemSetDisable(Hide).

% reposition the visible buttons
reposition :-
	findall(ID, def:mb(_, _, ID), IDs),
	reposition(IDs, 0, 32).

reposition([], _, _).
reposition([ID|IDs], X, BarH) :-
	gui:itemSelect(ID),
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
	gui:itemSetCmdAction(actions:layer(_Params, Layer)).

layerUpdateButtons :-
	def:dlg(menuBar, MB),
	dlg:select(MB),
	def:layerMax(Max),
	edi:toolBrush(Br),
	brush:getLayer(Br, Active),
	def:mb(layer, ID),
	layerUpdateButtons(0, Max, ID, Active),
	edi:layerSet(Active, 1).

layerUpdateButtons(L, L, _, _).
layerUpdateButtons(L, LM, ID, Act) :-
	layerUpdateButton(L, ID, Act),
	L2 is L + 1,
	layerUpdateButtons(L2, LM, ID, Act).


layerUpdateButton(L, ID_MB_LAYER, Act) :-
	ID is ID_MB_LAYER + L,
	gui:itemSelect(ID),
	edi:layerGet(L, Status),
	gui:itemSetValue(Status),
	layerButtonColor(L, Act, Status, Color),
	def:color(Color, COLOR),
	gui:itemSetColor(0, COLOR).

layerButtonColor(L, L, _, layer2) :- !.
layerButtonColor(_, _, 0, layer0).
layerButtonColor(_, _, 1, layer1).

















