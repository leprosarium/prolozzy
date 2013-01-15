:-module(dlgStatusBar, [create/0]).

% Those are some text controls that are updated per frame from
% the MOD_UserUpdate callback to show status info
% Users use SetStatusBar1, SetStatusBar2, SetStatusBar3 and
% SetStatusBar4 to set their texts

create :-
	edi:getScrH(ScrH),
	edi:getScrW(BarW),
	dlg:new(_),
	dlg:setRect(0, 30, BarW, 46),
	def:dlg(statusBar1, ID),
	dlg:setID(ID),
	gui:createText(0, 0, BarW, ""),
	def:color(backtext, COLOR_BACKTEXT),
	gui:itemSetTxtColor(COLOR_BACKTEXT),
	def:dlg(item, IID),
	gui:itemSetID(IID),
	gui:createText(0, 0, BarW, "", [right, centery]),
	gui:itemSetTxtColor(COLOR_BACKTEXT),
	IID2 is IID + 1,
	gui:itemSetID(IID2),

	% dialog bottom
	dlg:new(_),
	Y is ScrH - 17,
	dlg:setRect(0, Y, BarW, BarW),
	def:dlg(statusBar2, ID2),
	dlg:setID(ID2),
	gui:createItem(0, 0, BarW, 17, [backgr], [color(0, gui), color(1, gui), color(2, gui)]),
	def:dlg(back, DLGBACK),
	gui:itemSetID(DLGBACK),

	gui:createText(0, -1, BarW, ""),
	def:color(black, COLOR_BLACK),
	gui:itemSetTxtColor(COLOR_BLACK),
	gui:itemSetID(IID),
	gui:createText(0, -1, BarW, "", [right, centery]),
	gui:itemSetTxtColor(COLOR_BLACK),
	gui:itemSetID(IID2).












