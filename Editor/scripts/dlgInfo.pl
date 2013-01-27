:-module(dlgInfo, [mapFile/1,
		   setMapFile/1,
		   create/0]).

:-recorda(mapFile, "noname.pmp").

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

update.

%func DlgInfo_Update()
%{
%	ItemSelect(ItemFind(ID_DLGITEM+0));	ItemSetTxt(IV_TXT,(str)EdiGet(EDI_MAPW));
%	ItemSelect(ItemFind(ID_DLGITEM+1));	ItemSetTxt(IV_TXT,(str)EdiGet(EDI_MAPH));
%
%	ItemSelect(ItemFind(ID_DLGITEM+2));	ItemSetTxt(IV_TXT,(str)EdiGet(EDI_ROOMW));
%	ItemSelect(ItemFind(ID_DLGITEM+3));	ItemSetTxt(IV_TXT,(str)EdiGet(EDI_ROOMH));
%
%	// crop too long file names
%	i=0; len = strlen(g_mapfile);
%	while(TextW(strsub(g_mapfile,i))>280 && i<len) i++;
%	mapfile = strsub(g_mapfile,i);
%
%	// infos
%	ItemSelect(ItemFind(ID_DLGITEM+10));
%	roomw = EdiGet(EDI_ROOMW);
%	roomh = EdiGet(EDI_ROOMH);
%	roomx = EdiGet(EDI_MAPW) / roomw;
%	roomy = EdiGet(EDI_MAPH) / roomh;
%	text =	"Map: "+mapfile+"\n"+
%			"Brushes count: "+(str)MapBrushCount()+"\n"+
%			"Tiles count: "+(str)TileCount()+"\n"+
%			"Rooms count: "+(str)(roomx*roomy)+"  ( "+(str)roomx+" x "+(str)roomy+" )";
%	ItemSetTxt(IV_TXT, text);
%	texth = TextH(text)+8;
%	ItemSetInt(IV_Y2, ItemGetInt(IV_Y)+texth);
%	//...
%}
%


mapResize.

















