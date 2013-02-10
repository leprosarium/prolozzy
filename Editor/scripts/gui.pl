:- module(gui, [loadResources/0,
		select/1,
		styleCode/2,
		alignCode/2,
		createEdit/4,
		createEdit/5,
		createDlg/5,
		createDlgTitle/5,
		createDlgTitleModal/5,
		createItem/5,
		createItem/6,
		createItem/7,
		createRect/7,
		createBar/5,
		createBar/6,
		createCheck/3,
		createCheck/4,
		createRadio/4,
		createRadio/5,
		dlgAddKeys/1,
		createButtonImg/7,
		createText/5,
		createText/4,
		createImage/5,
		createImage/4,
		dlgTitleH/1,
		msgBox/4,
		msgBoxOk/3,
		createPullDownSelect/5,
		createPullDown/4,
		createPullDown/3,
		dlgResize/4]).

dlgTitleH(20).

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
	forall(member(Img,
		      [check1, check2, check3, check4,
		       radio1, radio2,
		       icon_info, icon_question, icon_warning, icon_error]),
	       loadImg(Img)).


loadImg(Img) :-
	format(string(Path), 'Editor\\Graphics\\~a.tga', [Img]),
	gui:imgLoad(Path, Idx),
	recordz(img, img(Img, Idx)).

getImg(Img, Idx) :-
	recorded(img, img(Img, Idx)).

select(Item) :-
	(def:dlg(Item, IID); def:dlg(item(Item), IID)),
	gui:itemFind(IID, IIDX),
	gui:itemSelect(IIDX).


% Dialogs

createDlg(X, Y, W, H, Style) :-
	dlg:new(_IDX),
	X2 is X + W,
	Y2 is Y + H,
	dlg:setRect(X, Y, X2, Y2),
	createItem(0, 0, W, H, Style, [color(0, gui), color(1, gui), color(2, gui)]),
	def:dlg(back, ID),
	gui:itemSetID(ID).

createDlgTitle(X, Y, W, H, Text, Color0, Color1) :-
	createDlg(X, Y, W, H, [backgr, border3d]),
	WW is W - 4,
	dlgTitleH(HH),
	createItem("cGUITitle", 2, 2, WW, HH, [gradient, border3d, pressed], [text(Text), color(0, Color0), color(1, Color1), color(2, gui)]),
	def:dlg(title, DLGID),
	gui:itemSetID(DLGID).

createDlgTitle(X, Y, W, H, Text) :-
	createDlgTitle(X, Y, W, H, Text, title1, title2).


createDlgTitleModal(X, Y, W, H, Text) :-
	createDlgTitle(X, Y, W, H, Text, modal1, modal2),
	dlg:setModal.


createBar(X, Y, W, H, Color0) :-
	createItem(X, Y, W, H),
	itemSetProp(color(0, Color0)),
	styleCode(backgr, StyleCode),
	gui:itemSetStyle(StyleCode).

createBar(X, Y, W, H, Color0, Color1) :-
	createItem(X, Y, W, H),
	itemSetProp(color(0, Color0)),
	itemSetProp(color(1, Color1)),
	styleCode(gradient, StyleCode),
	gui:itemSetStyle(StyleCode).



% Controls

createItem(X, Y, W, H) :-
	createItem("cGUIItem", X, Y, W, H).

createItem(Class, X, Y, W, H) :-
	gui:itemNew(_ID, Class),
	X2 is X + W,
	Y2 is Y + H,
	gui:itemSetRect(X, Y, X2, Y2).


createItem(Class, X, Y, W, H, Style, Props) :-
	createItem(Class, X, Y, W, H),
	itemSetProps(Props),
	styleCode(Style, StyleCode),
	gui:itemSetStyle(StyleCode),
	alignCode(centerxy, Align),
	gui:itemSetTxtAlign(Align).

createItem(X, Y, W, H, Style, Props) :-
	createItem("cGUIItem", X, Y, W, H, Style, Props).

createRect(X, Y, W, H, Color, Is3d, IsPressed) :-
	(   Is3d == true
	->  Style0 = [border3d]
	;   Style0 = [border]),
	(   IsPressed == true
	->  Style = [pressed | Style0]
	;   Style = Style0),
	createItem(X, Y, W, H, Style, [color(2, Color)]).

createButton(X, Y, W, Text, Cmd) :-
	createItem("cGUIButton", X, Y, W, 20, [gradient, border3d], [text(Text), color(0, gui1), color(1, gui2), color(2, gui)]),
	gui:itemSetCmdAction(Cmd).

createButtonImg(X, Y, W, H, Img0, Img1, Cmd ) :-
	createItem("cGUIButton", X, Y, W, H, [gradient, border3d], [color(0, gui1), color(1, gui2), color(2, gui)]),
	gui:itemSetImg0(Img0),
	gui:itemSetImg1(Img1),
	gui:itemSetCmdAction(Cmd).

createCheck(X, Y, Value) :-
	createItem("cGUICheck", X, Y, 20, 20),
	getImg(check1, Img0),
	getImg(check2, Img1),
	gui:itemSetImg0(Img0),
	gui:itemSetImg1(Img1),
	def:color(edit, Color),
	gui:itemSetImgColor(Color),
	gui:itemSetValue(Value).

createCheck(X, Y, Value, Cmd) :-
	createCheck(X, Y, Value),
	gui:itemSetCmdAction(Cmd).



createRadio(X, Y, Value, Group, Cmd) :-
	createRadio(X, Y, Value, Group),
	gui:itemSetCmdAction(Cmd).


createRadio(X, Y, Value, Group) :-
	createItem("cGUIRadio", X, Y, 20, 20),
	getImg(radio1, Img0),
	getImg(radio2, Img1),
	gui:itemSetImg0(Img0),
	gui:itemSetImg1(Img1),
	def:color(edit, Color),
	gui:itemSetImgColor(Color),
	gui:itemSetValue(Value),
	gui:itemSetGroup(Group).

createText(X, Y, W, Text) :-
	createText(X, Y, W, Text, [left, centery]).

createText(X, Y, W, Text, Align) :-
	createItem(X, Y, W, 20),
	gui:itemSetTxt(Text),
	alignCode(Align, AlignCode),
	gui:itemSetTxtAlign(AlignCode).


createImage(X, Y, W, H, Img) :-
	getImg(Img, ImgIdx),
	createImage(X, Y, W, H),
	gui:itemSetImg0(ImgIdx).

createImage(X, Y, W, H) :-
	createItem(X, Y, W, H),
	styleCode(none, Style),
	gui:itemSetStyle(Style),
	alignCode([left, top], Align),
	gui:itemSetImgAlign(Align).



createEdit(X, Y, W, Text) :-
	createItem("cGUIEdit", X, Y, W, 20, [backgr, border], [color(0, edit), color(1, gui1), color(2, editsel)]),
	gui:itemSetTxt(Text).

createEdit(X, Y, W, Text, Cmd) :-
	createEdit(X, Y, W, Text),
	gui:itemSetCmdAction(Cmd).

itemSetProps([]).
itemSetProps([Prop|Props]) :-
	itemSetProp(Prop),
	itemSetProps(Props).

itemSetProp(text(Text)):- gui:itemSetTxt(Text).
itemSetProp(color(N, Color)):- (def:color(Color, Code);Code=Color), gui:itemSetColor(N, Code).

dlgAddKeys([]).
dlgAddKeys([Key|Keys]) :-
	addKey(Key),
	dlgAddKeys(Keys).


addKey(KeyOp > Cmd) :-
	parceKeyOp(KeyOp, Key, Flags),
	dlg:addKey(Key, Flags, Cmd).


parceKeyOp(Key, K, 0) :-
	keys:key(Key, K).

parceKeyOp(Flag, 0, F) :-
	keys:flag(Flag, F).


parceKeyOp(A+B, K, F):-
	(   parceKeyOp(A, 0, F1), parceKeyOp(B, K, F2)
	;   parceKeyOp(A, K, F1), parceKeyOp(B, 0, F2)),
	F is F1 + F2.



msgBoxOk(Title, Text, Icon) :-
	msgBox(Title, Text, Icon, [btn("OK", true)]),
	addKey(return > gui:dlgClose),
	addKey(escape > gui:dlgClose).




% Message Boxes
% buttoninfo = button tab info (if missing or empty then no buttons) {
% {b1_name, b1_cmd}, ... }

calcButtonW(Text, Min, Width) :-
	gui:textW(Text, W),
	W2 is W + 32,
	(   W2 > Min
	->  Width = W2
	;   Width = Min).



calcButtonsW([], Min, Min).
calcButtonsW([btn(Text, _) |Btns], Min, Width) :-
	calcButtonW(Text, Min, BtnWidth),
	calcButtonsW(Btns, BtnWidth, Width).

msgBox(Title, Text, Icon, ButtonInfos):-
	length(ButtonInfos, Buttons),
	calcButtonsW(ButtonInfos, 64, ButtonW0),
	MaxWidth = 200,
	(   ButtonW0 > MaxWidth
	->  ButtonW = MaxWidth
	;   ButtonW = ButtonW0),
	gui:textW(Title, TitleW),
	gui:textW(Text, TextW),
	gui:textH(Text, TextH),
	dlgTitleH(DLGTITLEH),
	DlgW = TextW + 8 + 32,  %Icon
	(   TextH < 32
	->  TextH2 = 32
	;   TextH2 = TextH),
	DlgH is TextH2 + DLGTITLEH + 16,
	(DlgW < TitleW ->  DlgW2 = TitleW; DlgW2 = DlgW),
	(   Buttons > 0	->  DlgH2 is DlgH + 8 + 20; DlgH2 = DlgH),
	ButtonsW is Buttons * (ButtonW + 8),
	(DlgW2 < ButtonsW -> DlgW3 = ButtonsW; DlgW3 = DlgW2),
	DlgW4 is DlgW3 + 16,
	createDlgTitleModal(0, 0, DlgW4, DlgH2, Title),
	dlg:setCloseOut,

	% text box
	TextX = 40,    %Icon
	TextY is DLGTITLEH + 8,
	createText(TextX, TextY, TextW, Text, [left, top]),
	createImage(8, TextY, 32, 32, Icon),  %Icon
	createButtons(ButtonInfos, Buttons, DlgW4, TextY, TextH2, ButtonW).

createButtons(_, 0, _, _, _, _).
createButtons(ButtonInfos, Buttons, DlgW, TextY, TextH, ButtonW) :-
	ButtonW2 is ButtonW // 2,
	Buts is (DlgW - 16) // Buttons,
	Buts2 is Buts // 2,
	ButX = 8,
	ButY is TextY + TextH + 8,
	createButtons1(ButtonInfos, ButX, ButY, Buts, Buts2, ButtonW, ButtonW2).

createButtons1([], _, _, _, _, _, _).
createButtons1([btn(Text, Cmd)|Bo], ButX, ButY, Buts, Buts2, ButtonW, ButtonW2) :-
	X is ButX + Buts2 - ButtonW2,
	createButton(X, ButY, ButtonW, Text, (gui:dlgClose, Cmd)),
	XX is ButX + Buts,
	createButtons1(Bo, XX, ButY, Buts, Buts2, ButtonW, ButtonW2),
	dlgMoveToMouse,
	dlgDockUp.


% move dialog to mouse position and also prevent it to get ouside the screen
dlgMoveToMouse :-
	dlg:getRect(X, Y, X2, Y2),
	W is X2 - X,
	H is Y2 - Y,
	gui:mouseX(MX),
	gui:mouseY(MY),
	XX is MX - W // 2,
	YY is MY - H // 2,
	(   XX < 0
	->  XX2 = 0
	;   XX2 = XX),
	(   YY < 0
	->  YY2 = 0
	;   YY2 = YY),
	edi:getScrW(ScrW),
	edi:getScrH(ScrH),
	XS is ScrW - W,
	YS is ScrH - H,
	(   XX2 > XS
	->  XX3 = XS
	;   XX3 = XX2),
	(   YY2 > YS
	->  YY3 = YS
	;   YY3 = YY2),
	XX4 is XX3 + W,
	YY4 is YY3 + H,
	dlg:setRect(XX3, YY3, XX4, YY4).

% move dialog to the top menubar
dlgDockUp :-
	dlg:getPos(X, Y),
	(   Y < 32
	->  dlgMove(X, 32)
	;   true).

% just move dialog
dlgMove(X, Y) :-
	dlg:getRect(X1, Y1, X2, Y2),
	XX is X + X2 - X1,
	YY is Y + Y2 - Y1,
	dlg:setRect(X, Y, XX, YY).

createPullDownSelect(X, Y, Callbackname, List, Sel):-
	length(List, Count),
	Count == 0;
	makeMenuList(List, Callbackname, Menu),
	createPullDown(X, Y, Menu, Sel).

makeMenuList([], _, []).
makeMenuList([Id-A|As], Act, [item(Id-A, (gui:dlgClose, Action), [])|Ms]) :-
	copy_term(Act, act(Id, Action)),
	makeMenuList(As, Act, Ms).

calcPullDowmSizes(ItemH, MenuH, MenuW, Menu) :-
	length(Menu, Size),
	MenuH is Size * ItemH,
	calcPullDowmSizes(Menu, MenuW0),
	MenuW is MenuW0 + 20.
calcPullDowmSizes([], 0).
calcPullDowmSizes([item(_-Text, _, _)|Ms], W) :-
	gui:textW(Text, Wt),
	calcPullDowmSizes(Ms, MsW),
	(   Wt > MsW
	->  W = Wt
	;   W = MsW).

createPullDown(_, _, []).
createPullDown(X, Y, Menu) :-
	createPullDown(X, Y, Menu, -1).

createPullDown(_, _, [], _).
createPullDown(X, Y, Menu, Sel) :-
	ItemH = 20,
	calcPullDowmSizes(ItemH, MenuH, MenuW, Menu),
	createDlg(X, Y, MenuW, MenuH, [none]),
	dlg:setModal,
	dlg:setCloseOut,
	addKey(escape > gui:dlgClose),
	addPullDownItems(Menu, 0, ItemH, MenuW, Sel).

addPullDownItems([], _, _, _, _).
addPullDownItems([M|Ms], Y, Step, W, Sel) :-
	addPullDownItem(M, Y, W, Sel),
	Y2 is Y + Step,
	addPullDownItems(Ms, Y2, Step, W, Sel).



addPullDownItem(item(Id-Name, Cmd, Props), Y, MenuW, Sel) :-
	createButton(0, Y, MenuW, Name, Cmd),
	styleCode([backgr, border3d], Style),
	gui:itemSetStyle(Style),
	alignCode([left, centery], AlignCode),
	gui:itemSetTxtAlign(AlignCode),
	(   Cmd = ""
	->  (Color = gui1, gui:itemSetDisable(1))
	;   (Id=Sel->Color=layer1;Color = gui)),
	def:color(Color, Code),
	gui:itemSetColor(0, Code),
	gui:itemSetColor(1, Code),
	gui:itemSetColor(2, Code),
	(   member(key(Key), Props)
	->  addKey(Key > Cmd)
	;   true),
	(   member(tooltip(Tooltip), Props)
	->  gui:itemSetToolTip(Tooltip)
	;   true).

% reposition and resize dialog and it's background and title bar
dlgResize(X, Y, W, H) :-
	X2 is X + W,
	Y2 is Y + H,
	dlg:setRect(X, Y, X2, Y2),
	(   select(back)
	->  gui:itemSetRect(0, 0, W, H)
	;   true),
	(   select(title)
	->  WW is W - 2,
	    dlgTitleH(TH),
	    HH is TH + 2,
	    gui:itemSetRect(2, 2, WW, HH)
	;   true).















