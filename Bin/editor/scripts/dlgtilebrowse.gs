//////////////////////////////////////////////////////////////////////////////////
// Tile Browse Dialog
// DV_USER selected tile id (returned)
// DV_CLOSERET 0=cancel 1=select
//////////////////////////////////////////////////////////////////////////////////
int g_tilebrowse_first;		// first in page
int g_tilebrowse_cols;		// tiles columns (x)
int g_tilebrowse_rows;		// tiles rows (y)
int g_tilebrowse_size;		// tile size

func DlgTileBrowse_Create( x, y, cmdclose )
{
	if(TileCount()==0) return;
	g_tilebrowse_first	= 0;
	g_tilebrowse_cols 	= 4;	// customizable
	g_tilebrowse_rows 	= 4;	// customizable
	g_tilebrowse_size 	= 64;	// customizable
	gs_inigetint( INIFILE, "editor", "tilebrowse_first", 	&g_tilebrowse_first );
	gs_inigetint( INIFILE, "editor", "tilebrowse_cols", 	&g_tilebrowse_cols );
	gs_inigetint( INIFILE, "editor", "tilebrowse_rows", 	&g_tilebrowse_rows );
	gs_inigetint( INIFILE, "editor", "tilebrowse_size", 	&g_tilebrowse_size );
	
	tilesx = g_tilebrowse_cols;
	tilesy = g_tilebrowse_rows;
	tilesize = g_tilebrowse_size;
	
	space = 5; // space on x and y
	if(!?cmdclose) cmdclose="";

	w = tilesx*tilesize+(tilesx+1)*space;
	h = tilesy*tilesize+(tilesy+1)*space + DLGTITLEH + space;
	CreateDlgTitle( x, y, w, h, "Tile Browse", 1);
	DlgSetInt( DV_ID, ID_DLGTILEBROWSE );
	DlgSetInt( DV_CLOSEOUT, 1 );
	DlgSetTxt( DV_CLOSECMD, "DlgTileBrowse_Close();"+cmdclose );
	DlgSetInt( DV_USER, -1);
	DlgAddKey( KEY_PGDN,   		0, "DlgTileBrowse_Key(KEY_PGDN);" );
	DlgAddKey( KEY_PGUP,   		0, "DlgTileBrowse_Key(KEY_PGUP);" );
	DlgAddKey( MOUSE_WHEELDN,   0, "DlgTileBrowse_Key(KEY_PGDN);" );
	DlgAddKey( MOUSE_WHEELUP,   0, "DlgTileBrowse_Key(KEY_PGUP);" );
	DlgAddKey( KEY_DOWN,   		0, "DlgTileBrowse_Key(KEY_DOWN);" );
	DlgAddKey( KEY_UP,     		0, "DlgTileBrowse_Key(KEY_UP);"   );
	DlgAddKey( KEY_HOME,  		0, "DlgTileBrowse_Key(KEY_HOME);" );
	DlgAddKey( KEY_END,    		0, "DlgTileBrowse_Key(KEY_END);"  );
	DlgAddKey( KEY_ESCAPE, 		0, "DlgClose();" );
	ItemSetInt(IV_TXTALIGN, GUIALIGN_LEFT|GUIALIGN_CENTERY);

	celx = tilesize+space;
	cely = tilesize+space;

	for(j=0;j<tilesy;j++)
	{
		for(i=0;i<tilesx;i++)	
		{
			x = space + i*celx;
			y = space + j*cely + DLGTITLEH;
			idx = j*tilesx + i;

			CreateRect(x-1,y-1,tilesize+2,tilesize+2,COLOR_GUI,1,1);

			ItemNew("cGUITile");
			ItemSetInt(IV_ID, ID_DLGITEM+idx);
			ItemSetInt(IV_RECT, x, y, x+tilesize, y+tilesize);
			ItemSetInt(IV_VALUE,-1);	
			ItemSetInt(IV_COLOR+1,COLOR_TILEBKGR);
			ItemSetInt(IV_GUITILE_SCALE,1);
			ItemSetInt(IV_GUITILE_SHRINK,1);
		}
	}

	DlgTileBrowse_Update();
}

func DlgTileBrowse_Close()
{
	gs_inisetint( INIFILE, "editor", "tilebrowse_first", 	g_tilebrowse_first );
	gs_inisetint( INIFILE, "editor", "tilebrowse_cols", 	g_tilebrowse_cols );
	gs_inisetint( INIFILE, "editor", "tilebrowse_rows", 	g_tilebrowse_rows );
	gs_inisetint( INIFILE, "editor", "tilebrowse_size", 	g_tilebrowse_size );
}

func DlgTileBrowse_Update()
{
	tilesx = g_tilebrowse_cols;
	tilesy = g_tilebrowse_rows;
	if(g_tilebrowse_first>TileCount()-tilesx*tilesy) g_tilebrowse_first = TileCount()-tilesx*tilesy;
	if(g_tilebrowse_first<0) g_tilebrowse_first = 0;
	
	for(j=0;j<tilesy;j++)
	{
		for(i=0;i<tilesx;i++)	
		{
			idx = j*tilesx + i;
			ItemSelect(ItemFind(ID_DLGITEM+idx));
			if( (g_tilebrowse_first + idx < 0) || (g_tilebrowse_first + idx >= TileCount()) ) 
			{
				ItemSetInt(IV_VALUE, -1);
				ItemSetTxt(IV_CMDACTION, "");
				ItemSetTxt(IV_TOOLTIP, "");
			}
			else
			{
				name = TileGet(g_tilebrowse_first + idx, TILE_NAME); if(name!="") name="\n"+name;
				id = TileGet(g_tilebrowse_first + idx, TILE_ID);
				w = TileGet(g_tilebrowse_first + idx, TILE_W);
				h = TileGet(g_tilebrowse_first + idx, TILE_H);
				frames = TileGet(g_tilebrowse_first + idx, TILE_FRAMES);
				ItemSetInt(IV_VALUE, id);
				ItemSetTxt(IV_CMDACTION,"DlgSetInt(DV_USER," + (str)id + "); DlgClose(1);");
				ItemSetTxt(IV_TOOLTIP, "id="+(str)id+" w="+(str)w+" h="+(str)h+" frames="+(str)frames+name );
			}
		}
	}

	ItemSelect(ItemFind(ID_DLGTITLE));
	ItemSetTxt(IV_TXT, "Browse  "+(str)(g_tilebrowse_first)+" / "+(str)TileCount());
}

func DlgTileBrowse_Key(cmd)
{
	tilesx = g_tilebrowse_cols;
	tilesy = g_tilebrowse_rows;
	
	if(cmd==KEY_PGDN)
	{
		g_tilebrowse_first += tilesx*tilesy;
		DlgTileBrowse_Update();
	}
	if(cmd==KEY_PGUP)
	{
		g_tilebrowse_first -= tilesx*tilesy;
		DlgTileBrowse_Update();
	}
	if(cmd==KEY_DOWN)
	{
		g_tilebrowse_first += tilesx;
		DlgTileBrowse_Update();
	}
	if(cmd==KEY_UP)
	{
		g_tilebrowse_first -= tilesx;
		DlgTileBrowse_Update();
	}
	if(cmd==KEY_HOME)
	{
		g_tilebrowse_first = 0;
		DlgTileBrowse_Update();
	}
	if(cmd==KEY_END)
	{
		g_tilebrowse_first = TileCount()-tilesx*tilesy;
		DlgTileBrowse_Update();
	}	
}

//////////////////////////////////////////////////////////////////////////////////
