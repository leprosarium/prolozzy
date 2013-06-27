:-module(dlgInfo, [defName/1,
		   mapFile/1,
		   setMapFile/1,
		   create/0]).

defName('noname.pmp').

:-defName(Def), recorda(mapFile, Def).

mapFile(X) :-
	recorded(mapFile, X).
setMapFile(F) :-
	recorded(mapFile, _, Ref),
	erase(Ref),
	recorda(mapFile, F).


create :-
	DlgW = 340,
	DlgH is 136 + 26,
	gui:createDlgTitleModal(0, 0, DlgW, DlgH, "Map Info"),
	def:dlg(info, ID),
	dlg:setID(ID),
	dlg:setCloseOut,
	gui:addKey(escape > gui:dlgClose),
	gui:dlgTitleH(DLGTITLEH),
	X0 = 8,
	Y0 is DLGTITLEH + 8,
	DX = 6,
	DY = 6,
	DDY is 20 + DY,
	DDX is 20 + DX,
	X = X0,
	Y = Y0,
	WW is DlgW - 16,
	gui:createText(X, Y, WW, "Map width and height" ),
	X1 is X +148, % += DDY;
	gui:createEdit(X1, Y, 48, "0"),
	def:dlg(item, IID),
	gui:itemSetID(IID),
	gui:itemSetToolTip("world's widht in pixels."),
	X2 is X1 + 64,
	gui:createEdit(X2, Y, 48, "0"),
	def:dlg(item(1), IID1),
	gui:itemSetID(IID1),
	gui:itemSetToolTip("world's height  in pixels."),
	X3 is X1 + 128,
	gui:createButton(X3, Y, 48, "Resize", dlgInfo:mapResize),
	Y2 is Y + 46,
	gui:itemSetY2(Y2),
	gui:itemSetToolTip("Apply new values for map and room sizes.\nChanging map and room sizes may affect\ncurrent map content." ),

	XX = X0,
	YY is Y + DDY,

	gui:createText(XX, YY, WW, "Room width and height"),
	XX1 is XX + 148, %+=ddy;
	gui:createEdit(XX1, YY, 48, "0" ),
	def:dlg(item(2), IID2),
	gui:itemSetID(IID2),
	gui:itemSetToolTip("room's widht in pixels\ndefault=240, max=256" ),
	XX2 is XX1 + 64,
	gui:createEdit(XX2, YY, 48, "0" ),
	def:dlg(item(3), IID3),
	gui:itemSetID(IID3),
	gui:itemSetToolTip("room's height in pixels\ndefault=136, max=192" ),


	XX3  is X0,
	YY3 is YY + DDY,
	gui:createText(XX3, YY3, WW, "..." ),
	def:dlg(item(10), IID10),
	gui:itemSetID(IID10),
	gui:styleCode([backgr, border], Style),
	gui:itemSetStyle(Style),
	gui:itemSetProps([color(0, gui1), color(1, gui1), color(2, black)]),

	update,
	gui:dlgMoveToMouse,
	gui:dlgDockUp.

setVal(Item, Text) :-
	gui:select(Item),
	gui:itemSetTxt(Text).

getVal(Item, Text) :-
	gui:select(Item),
	gui:itemGetTxt(Text).


cropString("", "", _).
cropString(Nm, Nm, Max) :-
	gui:textW(Nm, Sz),
	Sz =< Max.
cropString([_|No], Nr, Max) :-
	cropString(No, Nr, Max).


update :-
	map:getMapW(MapW), setVal(item, MapW),
	map:getMapH(MapH), setVal(item(1), MapH),
	map:getRoomW(RoomW), setVal(item(2), RoomW),
	map:getRoomH(RoomH), setVal(item(3), RoomH),
	mapFile(Name),
	atom_chars(Name, Chars),
	cropString(Chars, NM, 280),

	RoomX = MapW // RoomW,
	RoomY = MapH // RoomH,
	map:brushCount(BC),
	edi:tileCount(TC),
	RC = RoomX * RoomY,
	format(string(Info), 'Map: ~s\nBrush count: ~d\nTiles count: ~d\nRooms count: ~d ( ~d x ~d )', [NM, BC, TC, RC, RoomX, RoomY]),
	setVal(item(10), Info),
	gui:textH(Info, TextH),
	gui:itemGetY(Y),
	Y2 is Y + TextH,
	gui:itemSetY2(Y2).


getVals(MapW, MapH, RoomW, RoomH) :-
	getVal(item, MW), atom_number(MW, MapW),
	getVal(item(1), MH), atom_number(MH, MapH),
	getVal(item(2), RW), atom_number(RW, RoomW),
	getVal(item(3), RH),  atom_number(RH, RoomH).

roomsToSmall(RoomW, RoomH) :-
	(RoomW < 64;  RoomH < 64)
	-> gui:msgBoxOk("Error", "The rooms are too small.\nEnter bigger values for room sizes.", icon_error).

roomsToBig(RoomW, RoomH) :-
	(RoomW > 256; RoomH > 192)
	-> gui:msgBoxOk("Error", "The rooms are too big.\nEnter smaller values for room sizes.", icon_error).

mapToSmall(MapW, MapH) :-
	(MapW < 128; MapH < 128)
	-> gui:msgBoxOk("Error", "The map is too small.\nEnter bigger values for map sizes.", icon_error).
oneRoomMap(MapW, MapH, RoomW, RoomH) :-
	(MapW < RoomW; MapH < RoomH)
	-> gui:msgBoxOk("Error", "The map must contain at least one room.\nEnter bigger values for map sizes.", icon_error).
mapResize :-
	getVals(MapW, MapH, RoomW, RoomH),
	(   roomsToSmall(RoomW, RoomH)
	;   roomsToBig(RoomW, RoomH)
	;   mapToSmall(MapW, MapH)
	;   oneRoomMap(MapW, MapH, RoomW, RoomH)
	;   mapResize(MapW, MapH, RoomW, RoomH)).


mapResize(MapW, MapH, RoomW, RoomH) :-
	(   \+ map:resize(MapW, MapH)
	->  gui:msgBoxOk("Warning", "Some brushes have been cropped \nand left out of the current map area.", icon_warning)
	;   true),
	map:setRoomW(RoomW),
	map:setRoomH(RoomH),
	core:ini('editor.ini', 'editor', 'options_roomw', RoomW),
	core:ini('editor.ini', 'editor', 'options_roomh', RoomH),
	update,
	roomNames:reset(true).
















