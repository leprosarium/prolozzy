//////////////////////////////////////////////////////////////////////////////////
// Options Dialog
// Change editor options
//////////////////////////////////////////////////////////////////////////////////
#def ROOMW 240	// default game room width
#def ROOMH 136	// default game room height

#def MAX_COLORTHEME 3	// max color themes (may add more)
tab  g_colorthemesample;// color theme samples table

func DlgOptions_Create()
{
	g_colorthemesample = {0xff4d4833,0xff006B46,0xff7A848B};
	
	CreateDlgTitle( 0, 0, 340, 354, "Options",1);
	DlgSetInt( DV_ID, ID_DLGOPTIONS );
	DlgSetInt( DV_CLOSEOUT, 1 );
	DlgSetTxt( DV_CLOSECMD, "DlgOptions_Close();" );
	DlgAddKey( KEY_ESCAPE, 	0, "DlgOptions_Close();DlgClose();");
	
	x0=16;
	y0=DLGTITLEH+16;
	dx=6; dy=6;
	ddy=20+dy; ddx=20+dx;

	// BOX1
	// visibility
	x=x0;y=y0;
	CreateBar(x-8,y-8,156,176, COLOR_GUI1);
	// axes
	CreateCheck( x, y, EdiGet(EDI_AXES) ); ItemSetInt(IV_ID,ID_DLGITEM+0);
	CreateText( x+ddx, y, 100, "show axes" );
	y+=ddy;
	// grid
	CreateCheck( x, y, EdiGet(EDI_GRID) ); ItemSetInt(IV_ID,ID_DLGITEM+1);
	CreateText( x+ddx, y, 100, "show grid" );
	y+=ddy;
	// snap
	CreateCheck( x, y, EdiGet(EDI_SNAP) ); ItemSetInt(IV_ID,ID_DLGITEM+2);	
	CreateText( x+ddx, y, 100, "snap to grid" );
	y+=ddy;
	// room grid
	CreateCheck( x, y, EdiGet(EDI_ROOMGRID) ); ItemSetInt(IV_ID,ID_DLGITEM+3);	
	CreateText( x+ddx, y, 100, "show room grid" );
	y+=ddy;
	// brush rect
	CreateCheck( x, y, EdiGet(EDI_BRUSHRECT) ); ItemSetInt(IV_ID,ID_DLGITEM+4);	
	CreateText( x+ddx, y, 100, "show brush rects" );
	y+=ddy;
	// room info
	CreateButton( x,y,140,MOD_GetRoomInfoName(g_roominfo),"DlgOptions_RoomInfo();" ); ItemSetInt(IV_ID,ID_DLGITEM+5);
	ItemSetTxt(IV_TOOLTIP,"choose what kind of room info\nis displayed in the info bar.");
	
	// BOX2
	// screensize
	x = 180; y = y0;
	CreateBar(x-8,y-8,160,176, COLOR_GUI1);	
	screensize = 1;
	gs_inigetint( INIFILE, "editor", "options_screensize", &screensize );
	CreateText( x, y, 100, "Screen Size" );
	y+=ddy;
	CreateRadio( x, y, screensize==0, 1 ); ItemSetInt(IV_ID,ID_DLGITEM+10);
	CreateText( x+ddx, y, 100, "small (640x480)" );
	y+=ddy;
	CreateRadio( x, y, screensize==1, 1 ); ItemSetInt(IV_ID,ID_DLGITEM+11);
	CreateText( x+ddx, y, 100, "medium (800x600)" );
	y+=ddy;
	CreateRadio( x, y, screensize==2, 1 ); ItemSetInt(IV_ID,ID_DLGITEM+12);
	CreateText( x+ddx, y, 100, "large (1024x768)" );
	y+=ddy;
	CreateRadio( x, y, screensize==3, 1 ); ItemSetInt(IV_ID,ID_DLGITEM+13);
	CreateText( x+ddx, y, 100, "huge (1280x1024)" );
	y+=ddy;
	CreateText( x, y, 140, "(need restart to apply)" );
	
	// BOX4
	// tiles folder
	x = x0; y = y0+184;
	CreateBar(x-8,y-8,172+160-8,64, COLOR_GUI1);	
	tilefolder = "";
	gs_inigetstr( INIFILE, "editor", "options_tiledir", &tilefolder );
	CreateText( x, y, 100, "Tiles Folder" );
	CreateButton( x+244,y,64,"reload","DlgOptions_ReloadTiles();" );
	ItemSetTxt(IV_TOOLTIP,"reload tiles from\nthe selected folder [R]");
	y+=ddy;
	CreateEdit( x, y, 172+110, tilefolder, "DlgOptions_SetTileFolder();" ); ItemSetInt(IV_ID,ID_DLGITEM+20);	
	ItemSetInt(IV_USER+1,255); ItemBuild(); // resize editbox
	CreateButton( x+288,y,20,"...","DlgOptions_BrowseTileFolder();" );
	ItemSetTxt(IV_TOOLTIP,"browse tiles folder");

	// BOX5
	// colors
	x = x0; y = y0+184+64+8;
	CreateBar(x-8,y-8,172+160-8,34+28, COLOR_GUI1);	
	CreateText( x, y, 100, "Map background color" );
	CreateButton( x+244,y,64,"", "DlgOptions_BrowseColor();" );
	ItemSetInt(IV_ID,ID_DLGITEM+30);
	ItemSetInt(IV_STYLE, GUISTYLE_BACKGR|GUISTYLE_BORDER3D);
	ItemSetInt(IV_COLOR, EdiGet(EDI_COLORMAP) | 0xff000000);
	ItemSetTxt(IV_TOOLTIP,"change map color");
	y+=ddy;
	colortheme=0;
	gs_inigetint( INIFILE, "editor", "options_colortheme", &colortheme );
	CreateText( x, y, 100, "Editor color theme" );
	CreateButton( x+244,y,64,"", "DlgOptions_BrowseColorTheme();" );
	ItemSetInt(IV_ID,ID_DLGITEM+31);
	ItemSetInt(IV_STYLE, GUISTYLE_BACKGR|GUISTYLE_BORDER3D);
	ItemSetInt(IV_COLOR, g_colorthemesample[colortheme]);
	ItemSetTxt(IV_TOOLTIP,"change editor color preferences");
	y+=ddy;	
		
	DlgAddKey( KEY_R, 0, "DlgOptions_ReloadTiles();" );

	DlgMoveToMouse(); DlgDockUp();
}

func DlgOptions_Close()
{
	// write uneditable options, in case they are lost
	api = 0;
	gs_inigetint( INIFILE, "editor", "options_videoapi", &api );
	gs_inisetint( INIFILE, "editor", "options_videoapi", api );
	
	cool = 0;
	gs_inigetint( INIFILE, "editor", "options_cool", &cool );
	gs_inisetint( INIFILE, "editor", "options_cool", cool );
	
	ItemSelect(ItemFind(ID_DLGITEM+0));
	axes = ItemGetInt(IV_VALUE);
	EdiSet(EDI_AXES,axes);
	gs_inisetint( INIFILE, "editor", "options_axes", axes );

	ItemSelect(ItemFind(ID_DLGITEM+1));
	grid = ItemGetInt(IV_VALUE);
	EdiSet(EDI_GRID,grid);
	gs_inisetint( INIFILE, "editor", "options_grid", grid );

	ItemSelect(ItemFind(ID_DLGITEM+2));
	snap = ItemGetInt(IV_VALUE);
	EdiSet(EDI_SNAP,snap);
	gs_inisetint( INIFILE, "editor", "options_snap", snap );

	ItemSelect(ItemFind(ID_DLGITEM+3));
	roomgrid = ItemGetInt(IV_VALUE);
	EdiSet(EDI_ROOMGRID,roomgrid);
	gs_inisetint( INIFILE, "editor", "options_roomgrid", roomgrid );
	
	ItemSelect(ItemFind(ID_DLGITEM+4));
	brushrect = ItemGetInt(IV_VALUE);
	EdiSet(EDI_BRUSHRECT,brushrect);
	gs_inisetint( INIFILE, "editor", "options_brushrect", brushrect );
	
	screensize=1;	
	ItemSelect(ItemFind(ID_DLGITEM+10)); if(ItemGetInt(IV_VALUE)) screensize=0;
	ItemSelect(ItemFind(ID_DLGITEM+11)); if(ItemGetInt(IV_VALUE)) screensize=1;
	ItemSelect(ItemFind(ID_DLGITEM+12)); if(ItemGetInt(IV_VALUE)) screensize=2;
	ItemSelect(ItemFind(ID_DLGITEM+13)); if(ItemGetInt(IV_VALUE)) screensize=3;
	gs_inisetint( INIFILE, "editor", "options_screensize", screensize );
	
	ItemSelect(ItemFind(ID_DLGITEM+20)); tilefolder=ItemGetTxt(IV_TXT);
	gs_inisetstr( INIFILE, "editor", "options_tiledir", tilefolder );
	
	ItemSelect(ItemFind(ID_DLGITEM+30)); color=ItemGetInt(IV_COLOR);
	gs_inisetint( INIFILE, "editor", "options_colormap", color );

	MapRefresh();
	
	if(TileCount()==0)
		MsgBoxOk("Warning", "No tiles loaded.\nCheck the path and the tiles folder.", ICON_WARNING );
}

func DlgOptions_SetTileFolder()
{
	tilefolder = ItemGetTxt(IV_TXT);
	gs_inisetstr( INIFILE, "editor", "options_tiledir", tilefolder );
}

func DlgOptions_ReloadTiles()
{
	ok = TileReload();
	if(!ok) 
		MsgBoxOk("Error", "Tiles reloading failed.\nCheck the path to the tiles folder.", ICON_ERROR );
	else
		MsgBoxOk("Message", "Tiles reloading successful.\nLoaded "+(str)TileCount()+" tiles.", ICON_INFO );
}

func DlgOptions_BrowseTileFolder()
{
	ItemSelect(ItemFind(ID_DLGITEM+20)); 
	tilefolder=ItemGetTxt(IV_TXT);
	ret = WinDlgOpenFolder( &tilefolder );
	if(ret)
	{
		ItemSetTxt(IV_TXT,tilefolder);
		DlgOptions_SetTileFolder();
	}
}

func DlgOptions_BrowseColor()
{
	color = EdiGet(EDI_COLORMAP) | 0xff000000;
	DlgColor_Create( 0, 0, "DlgOptions_ColorSet", color );
	DlgMoveToMouse();
}

func DlgOptions_ColorSet(color)
{
	color |= 0xff000000;
	DlgSelect(DlgFind(ID_DLGOPTIONS));
	ItemSelect(ItemFind(ID_DLGITEM+30)); 
	ItemSetInt(IV_COLOR,color);
	EdiSet(EDI_COLORMAP,color);
	MapRefresh();
}

func DlgOptions_BrowseColorTheme()
{
	colortheme = 0;
	gs_inigetint( INIFILE, "editor", "options_colortheme", &colortheme );
	colortheme++;
	if(colortheme==MAX_COLORTHEME) colortheme=0;
	gs_inisetint( INIFILE, "editor", "options_colortheme", colortheme );
	DlgSelect(DlgFind(ID_DLGOPTIONS));
	ItemSelect(ItemFind(ID_DLGITEM+31)); 
	ItemSetInt(IV_COLOR,g_colorthemesample[colortheme]);
}

// called from EDI_Init to load values from ini file
func DlgOptions_Load()
{
	axes = 0;
	gs_inigetint( INIFILE, "editor", "options_axes", &axes );
	EdiSet(EDI_AXES,axes);
	
	grid = 1;
	gs_inigetint( INIFILE, "editor", "options_grid", &grid );
	EdiSet(EDI_GRID,grid);

	snap = 1;
	gs_inigetint( INIFILE, "editor", "options_snap", &snap );
	EdiSet(EDI_SNAP,snap);

	roomgrid = 0;
	gs_inigetint( INIFILE, "editor", "options_roomgrid", &roomgrid );
	EdiSet(EDI_ROOMGRID,roomgrid);
	
	brushrect = 0;
	gs_inigetint( INIFILE, "editor", "options_brushrect", &brushrect );
	EdiSet(EDI_BRUSHRECT,brushrect);
	
	roomw = ROOMW;
	gs_inigetint( INIFILE, "editor", "options_roomw", &roomw );
	EdiSet(EDI_ROOMW,roomw);

	roomh = ROOMH;
	gs_inigetint( INIFILE, "editor", "options_roomh", &roomh );
	EdiSet(EDI_ROOMH,roomh);
	
	color = COLOR_MAP;
	gs_inigetint( INIFILE, "editor", "options_colormap", &color );
	EdiSet(EDI_COLORMAP,color);
}

// called from EDI_Done to save some values in ini file
func DlgOptions_Save()
{
	axes = EdiGet(EDI_AXES);
	gs_inisetint( INIFILE, "editor", "options_axes", axes );
	
	grid = EdiGet(EDI_GRID);
	gs_inisetint( INIFILE, "editor", "options_grid", grid );

	snap = EdiGet(EDI_SNAP);
	gs_inisetint( INIFILE, "editor", "options_snap", snap );

	roomgrid = EdiGet(EDI_ROOMGRID);
	gs_inisetint( INIFILE, "editor", "options_roomgrid", roomgrid );
	
	brushrect = EdiGet(EDI_BRUSHRECT);
	gs_inisetint( INIFILE, "editor", "options_brushrect", brushrect );
	
	roomw = EdiGet(EDI_ROOMW);
	gs_inisetint( INIFILE, "editor", "options_roomw", roomw );

	roomh = EdiGet(EDI_ROOMH);
	gs_inisetint( INIFILE, "editor", "options_roomh", roomh );
	
	color = EdiGet(EDI_COLORMAP);
	gs_inisetint( INIFILE, "editor", "options_colormap", color );
}

// change editor color theme (changing defines values - not very elegant:)
func DlgOptions_ColorTheme()
{
	colortheme=0;
	gs_inigetint( INIFILE, "editor", "options_colortheme", &colortheme );
	if(colortheme==0)
	{
		COLOR_GUI			= 0xff4d4833;
		COLOR_GUI1			= 0xff605a42;
		COLOR_GUI2			= 0xff3c3a29;
		COLOR_TITLE			= 0xffefa845;
		COLOR_TITLE1		= 0xfffbbe55;
		COLOR_TITLE2		= 0xffe39237;
		COLOR_MODAL			= 0xffff3000;
		COLOR_MODAL1		= 0xffff6000;
		COLOR_MODAL2		= 0xffff0000;
		COLOR_EDIT			= 0xff5e6d8b;
		COLOR_EDITSEL		= 0xfffbbe55;
		COLOR_LAYER0		= 0xff4d4833;
		COLOR_LAYER1		= 0xffbd540e;
		COLOR_LAYER2		= 0xffffff00;
	}
	else
	if(colortheme==1)
	{
		COLOR_GUI			= 0xff006B46;
		COLOR_GUI1			= 0xff007751;
		COLOR_GUI2			= 0xff00643F;
		COLOR_TITLE			= 0xff009445;
		COLOR_TITLE1		= 0xff009F50;
		COLOR_TITLE2		= 0xff006A3B;
		COLOR_MODAL			= 0xff00C040;
		COLOR_MODAL1		= 0xff00E05D;
		COLOR_MODAL2		= 0xff00AB2C;
		COLOR_EDIT			= 0xff31B582;
		COLOR_EDITSEL		= 0xfffbbe55;//0xff009F50;
		COLOR_LAYER0		= 0xff006B46;
		COLOR_LAYER1		= 0xff00BD73;
		COLOR_LAYER2		= 0xff00FF9C;
	}
	else
	if(colortheme==2)
	{
		COLOR_GUI			= 0xff7A848B;
		COLOR_GUI1			= 0xff8A969E;
		COLOR_GUI2			= 0xff6E777D;
		COLOR_TITLE			= 0xff5D83bC;
		COLOR_TITLE1		= 0xff638Ac6;
		COLOR_TITLE2		= 0xff5172a4;
		COLOR_MODAL			= 0xff7DA3DC;
		COLOR_MODAL1		= 0xff83AAE6;
		COLOR_MODAL2		= 0xff7192C4;
		COLOR_EDIT			= 0xff90C0DF;
		COLOR_EDITSEL		= 0xffffffff;
		COLOR_LAYER0		= 0xff7A848B;
		COLOR_LAYER1		= 0xff90C0DF;
		COLOR_LAYER2		= 0xffffffff;
	}
}

// choose room info
func DlgOptions_RoomInfo()
{
	menu={};
	i=0;
	while(true)
	{
		sz = MOD_GetRoomInfoName(i);
		if(sz=="") break;
		menu += { { sz, "DlgClose();DlgOptions_RoomInfoSet("+(str)i+");" } };
		i++;
		//if(i==1 || i==5)	menu += { { " ", "" } }; // separators
		
	}
	CreatePullDown( 0, 0, &menu, g_roominfo );
	DlgMoveToMouse();
}

func DlgOptions_RoomInfoSet( roominfo )
{
	g_roominfo = roominfo;
	DlgSelect(DlgFind(ID_DLGOPTIONS));
	ItemSelect(ItemFind(ID_DLGITEM+5));
	ItemSetTxt(IV_TXT,MOD_GetRoomInfoName(g_roominfo));
}

//////////////////////////////////////////////////////////////////////////////////
