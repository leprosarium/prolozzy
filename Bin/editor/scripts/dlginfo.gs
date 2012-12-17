//////////////////////////////////////////////////////////////////////////////////
// Info Dialog
// Show informations about the current loaded map with resize option
//////////////////////////////////////////////////////////////////////////////////
str g_mapfile; // current loaded map file name

func DlgInfo_Create()
{
	dlgw = 340;
	dlgh = 136+26;
	CreateDlgTitle( 0, 0, dlgw, dlgh, "Map Info",1);
	DlgSetInt( DV_ID, ID_DLGINFO );
	DlgSetInt( DV_CLOSEOUT, 1 );
	DlgAddKey( KEY_ESCAPE, 	0, "DlgClose(0);");
	
	x0=8;
	y0=DLGTITLEH+8;
	dx=6; dy=6;
	ddy=20+dy; ddx=20+dx;

	x=x0;y=y0;
		
	// map sizes
	CreateText( x, y, dlgw-16, "Map width and height" ); x+=148;//+=ddy;
	CreateEdit( x, 	  y, 48, "0" ); ItemSetInt(IV_ID,ID_DLGITEM+0);
	ItemSetTxt(IV_TOOLTIP, "world's widht in pixels." );
	CreateEdit( x+64, y, 48, "0" ); ItemSetInt(IV_ID,ID_DLGITEM+1);
	ItemSetTxt(IV_TOOLTIP, "world's height in pixels." );

	CreateButton( x+128, y, 48, "Resize", "DlgInfo_MapResize();" );
	ItemSetInt(IV_Y2,y+46);
	ItemSetTxt(IV_TOOLTIP, "Apply new values for map and room sizes.\nChanging map and room sizes may affect\ncurrent map content." );

	x=x0;y+=ddy;
	
	// room sizes
	CreateText( x, y, dlgw-16, "Room width and height" ); x+=148;//+=ddy;
	CreateEdit( x, 	  y, 48, "0" ); ItemSetInt(IV_ID,ID_DLGITEM+2);
	ItemSetTxt(IV_TOOLTIP, "room's widht in pixels\ndefault=240, max=256" );
	CreateEdit( x+64, y, 48, "0" ); ItemSetInt(IV_ID,ID_DLGITEM+3);
	ItemSetTxt(IV_TOOLTIP, "room's height in pixels\ndefault=136, max=192" );
	x=x0;y+=ddy;

	// infos
	CreateText( x, y, dlgw-16, "..." ); ItemSetInt(IV_ID,ID_DLGITEM+10);
	ItemSetInt( IV_STYLE, GUISTYLE_BACKGR|GUISTYLE_BORDER );
	ItemSetInt( IV_COLOR, COLOR_GUI1,COLOR_GUI1,COLOR_BLACK );

	DlgInfo_Update();
	DlgMoveToMouse(); DlgDockUp();
}

func DlgInfo_Update()
{
	ItemSelect(ItemFind(ID_DLGITEM+0));	ItemSetTxt(IV_TXT,(str)EdiGet(EDI_MAPW));
	ItemSelect(ItemFind(ID_DLGITEM+1));	ItemSetTxt(IV_TXT,(str)EdiGet(EDI_MAPH));

	ItemSelect(ItemFind(ID_DLGITEM+2));	ItemSetTxt(IV_TXT,(str)EdiGet(EDI_ROOMW));
	ItemSelect(ItemFind(ID_DLGITEM+3));	ItemSetTxt(IV_TXT,(str)EdiGet(EDI_ROOMH));
	
	// crop too long file names
	i=0; len = strlen(g_mapfile);
	while(TextW(strsub(g_mapfile,i))>280 && i<len) i++;
	mapfile = strsub(g_mapfile,i);
	
	// infos
	ItemSelect(ItemFind(ID_DLGITEM+10));
	roomw = EdiGet(EDI_ROOMW);
	roomh = EdiGet(EDI_ROOMH);
	roomx = EdiGet(EDI_MAPW) / roomw;
	roomy = EdiGet(EDI_MAPH) / roomh;
	text = 	"Map: "+mapfile+"\n"+
			"Brushes count: "+(str)MapBrushCount()+"\n"+
			"Tiles count: "+(str)TileCount()+"\n"+
			"Rooms count: "+(str)(roomx*roomy)+"  ( "+(str)roomx+" x "+(str)roomy+" )";
	ItemSetTxt(IV_TXT, text); 
	texth = TextH(text)+8;
	ItemSetInt(IV_Y2, ItemGetInt(IV_Y)+texth);
	//...
}

func DlgInfo_MapResize()
{
	ItemSelect(ItemFind(ID_DLGITEM+0));	mapw = (int)ItemGetTxt(IV_TXT);
	ItemSelect(ItemFind(ID_DLGITEM+1));	maph = (int)ItemGetTxt(IV_TXT);
	ItemSelect(ItemFind(ID_DLGITEM+2));	roomw = (int)ItemGetTxt(IV_TXT);
	ItemSelect(ItemFind(ID_DLGITEM+3));	roomh = (int)ItemGetTxt(IV_TXT);
	if(roomw<64 || roomh<64) 	{ MsgBoxOk("Error", "The rooms are too small.\nEnter bigger values for room sizes.", ICON_ERROR ); return; }
	if(roomw>256 || roomh>192) 	{ MsgBoxOk("Error", "The rooms are too big.\nEnter smaller values for room sizes.", ICON_ERROR ); return; }
	if(mapw<128 || maph<128) 	{ MsgBoxOk("Error", "The map is too small.\nEnter bigger values for map sizes.", ICON_ERROR ); return; }
	if(mapw<roomw || maph<roomh){ MsgBoxOk("Error", "The map must contain at least one room.\nEnter bigger values for map sizes.", ICON_ERROR ); return; }
	ret = MapResize(mapw,maph);
	EdiSet(EDI_ROOMW,roomw);
	gs_inisetint( INIFILE, "editor", "options_roomw", roomw );
	EdiSet(EDI_ROOMH,roomh);
	gs_inisetint( INIFILE, "editor", "options_roomh", roomh );
	DlgInfo_Update();
	RoomNamesReset(1);
	RoomTextsReset(1);
	RoomPropsReset(1);
	if(!ret) MsgBoxOk("Warning", "Some brushes have been cropped \nand left out of the current map area.", ICON_WARNING );
}

//////////////////////////////////////////////////////////////////////////////////
