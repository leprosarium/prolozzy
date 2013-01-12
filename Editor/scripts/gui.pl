:- module(gui, [loadResources/0,
	       styleCode/2,
	       alignCode/2,
	       createDlg/5,
	       createItem/6]).

styleCode(none, 0).
styleCode(backgr, 1). % color bar
styleCode(gradient, 2). % color gradient
styleCode(border, 4). % simple border
styleCode(border3d, 8). % 3d border
styleCode(pressed, 16). % 3d border status (pressed or released)

styleCode([], 0).
styleCode([Style|Styles], S) :-
	styleCode(Styles, Ss),
	styleCode(Style, Code),
	S is Ss \/ Code.


% align bits 0,1=horizontal, 2,3=vertical; value 0=center, 1=top/left, 2=right/bottom


alignCode(none, 0). %default (top-left 0,0)
alignCode(left, 1).
alignCode(right, 2).
alignCode(centerx, 3).
alignCode(top, 4).
alignCode(bottom, 8).
alignCode(centery, 12).
alignCode(centerxy, 15).

alignCode([], 0).
alignCode([Align|Aligns], A) :-
	alignCode(Aligns, Aa),
	alignCode(Align, Code),
	A is Aa \/ Code.




loadResources :-
	gui:imgLoad("Editor\\Graphics\\check1.tga", Img_check1),
	core:dl(img(Img_check1)),
	gui:imgLoad("Editor\\Graphics\\check2.tga", Img_check2),
	core:dl(img(Img_check2)),
	gui:imgLoad("Editor\\Graphics\\check3.tga", Img_check3),
	core:dl(img(Img_check3)),
	gui:imgLoad("Editor\\Graphics\\check4.tga", Img_check4),
	core:dl(img(Img_check4)),
	gui:imgLoad("Editor\\Graphics\\radio1.tga", Img_radio1),
	core:dl(img(Img_radio1)),
	gui:imgLoad("Editor\\Graphics\\radio2.tga", Img_radio2),
	core:dl(img(Img_radio2)),

	gui:imgLoad("Editor\\Graphics\\icon_info.tga", Img_Icon_Info),
	core:dl(img(Img_Icon_Info)),
	gui:imgLoad("Editor\\Graphics\\icon_question.tga", Img_icon_Question),
	core:dl(img(Img_icon_Question)),
	gui:imgLoad("Editor\\Graphics\\icon_warning.tga", Img_Icon_Warning),
	core:dl(img(Img_Icon_Warning)),
	gui:imgLoad("Editor\\Graphics\\icon_error.tga", Img_Icon_Error),
	core:dl(img(Img_Icon_Error)).

% Dialogs

createDlg(X, Y, W, H, Style) :-
	gui:dlgNew(_IDX),
	X2 is X + W,
	Y2 is Y + H,
	gui:dlgSetRect(X, Y, X2, Y2),
	def:color(gui, COLOR_GUI),
	createItem(0, 0, W, H, Style, [color(0, COLOR_GUI), color(1, COLOR_GUI), color(2, COLOR_GUI)]),
	def:id(dlgBack, ID),
	gui:itemSetID(ID).

% Controls

createItem(X, Y, W, H, Style, Props) :-
	gui:itemNew(_ID, "cGUIItem"),
	X2 is X + W,
	Y2 is Y + H,
	gui:itemSetRect(X, Y, X2, Y2),
	itemSetProps(Props),
	styleCode(Style, StyleCode),
	gui:itemSetStyle(StyleCode),
	alignCode(centerxy, Align),
	gui:itemSetTxtAlign(Align).

itemSetProps([]).
itemSetProps([Prop|Props]) :-
	itemSetProp(Prop),
	itemSetProps(Props).

itemSetProp(text(Text)):- gui:itemSetTxt(Text).
itemSetProp(color(N, Text)):- gui:itemSetColor(N, Text).



