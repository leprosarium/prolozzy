//////////////////////////////////////////////////////////////////////////////////
// Tile Map Dialog
// Use +/- to zoom, page up/down and home/end to change tile,
// First mouse button = select; Second mouse button = drag selection
// S=snap, G=grid, A=axes
// left,right,up,down = move selection
// shift + left,right,up,down = resize selection
//////////////////////////////////////////////////////////////////////////////////
int g_tilemap_scale;
int g_tilemap_snap;
int g_tilemap_grid;
int g_tilemap_axes;

func DlgTileMap_Create()
{
	if(TileCount()==0) return;
	tileid = ToolBrushGet(BRUSH_TILE);
	tileidx = TileFind(tileid);
	if(tileidx==-1) return;

	x=0; y=0;
	g_tilemap_scale = 2;
	g_tilemap_snap = 1;
	g_tilemap_grid = 1;
	g_tilemap_axes = 0;
	gs_inigetint( INIFILE, "editor", "tilemap_scale",	&g_tilemap_scale );
	gs_inigetint( INIFILE, "editor", "tilemap_snap",	&g_tilemap_snap );
	gs_inigetint( INIFILE, "editor", "tilemap_grid",	&g_tilemap_grid );
	gs_inigetint( INIFILE, "editor", "tilemap_axes",	&g_tilemap_axes );
	if(g_tilemap_scale<1) g_tilemap_scale=1;
	
	// dialog
	CreateDlgTitle( x, y, 128, 128, "Tile Map",1);
	DlgSetInt( DV_ID, ID_DLGTILEMAP );
	DlgSetInt( DV_CLOSEOUT, 1 );
	DlgSetTxt( DV_CLOSECMD, "DlgTileMap_Close();" );
	DlgAddKey( KEY_EQUALS, 		0, "DlgTileMap_Key(KEY_EQUALS);");
	DlgAddKey( KEY_MINUS,		0, "DlgTileMap_Key(KEY_MINUS);");
	DlgAddKey( KEY_ADD,   		0, "DlgTileMap_Key(KEY_ADD);");
	DlgAddKey( KEY_SUBTRACT,	0, "DlgTileMap_Key(KEY_SUBTRACT);");
	DlgAddKey( KEY_PGDN,		0, "DlgTileMap_Key(KEY_PGDN);");
	DlgAddKey( KEY_PGUP,		0, "DlgTileMap_Key(KEY_PGUP);");
	DlgAddKey( MOUSE_WHEELDN,	0, "DlgTileMap_Key(KEY_PGDN);");
	DlgAddKey( MOUSE_WHEELUP,	0, "DlgTileMap_Key(KEY_PGUP);");
	DlgAddKey( KEY_HOME,		0, "DlgTileMap_Key(KEY_HOME);");
	DlgAddKey( KEY_END,			0, "DlgTileMap_Key(KEY_END);");
	DlgAddKey( KEY_ESCAPE, 		0, "DlgClose();");
	ItemSetInt(IV_TXTALIGN, GUIALIGN_LEFT|GUIALIGN_CENTERY);

	// tile image
	ItemNew("cGUITile");
	ItemSetInt(IV_ID, ID_DLGITEM+0);
	ItemSetInt(IV_RECT, 0, 0, 0, 0);
	ItemSetInt(IV_VALUE, -1);
	ItemSetInt(IV_GUITILE_SCALE, g_tilemap_scale);
	ItemSetInt(IV_COLOR+1,COLOR_TILEBKGR);

	// tile map control
	ItemNew("cGUITileMap");
	ItemSetInt(IV_ID, ID_DLGITEM+1);
	ItemSetInt(IV_RECT, 0, 0, 0, 0);
	ItemSetInt(IV_VALUE, -1);
	ItemSetInt(IV_GUITILEMAP_SCALE, g_tilemap_scale);
	ItemSetInt(IV_GUITILEMAP_SNAP, g_tilemap_snap);
	ItemSetInt(IV_GUITILEMAP_GRID, g_tilemap_grid);
	ItemSetInt(IV_GUITILEMAP_AXES, g_tilemap_axes);
	ItemSetInt(IV_GUITILEMAP_MAP, ToolBrushGet(BRUSH_MAP+0), ToolBrushGet(BRUSH_MAP+1), ToolBrushGet(BRUSH_MAP+2), ToolBrushGet(BRUSH_MAP+3));
	
	// colors
	cursor=-1;
	w=12;x=16;y=DLGTITLEH+6;
	for(i=0;i<COLOR_USEDMAX;i++)
	{
		CreateBar(x,y,w,w,g_color_used[i]);
		ItemSetInt(IV_ID, ID_DLGITEM+10+i);
		ItemSetInt(IV_STYLE, GUISTYLE_BACKGR|GUISTYLE_BORDER);
		ItemSetInt(IV_COLOR+2,COLOR_BLACK);
		ItemSetTxt(IV_CMDACTION, "DlgTileMap_SetColor("+(str)i+");");
		x+=w;
		if(g_color_used[i]==ToolBrushGet(BRUSH_COLOR)) cursor=i;
	}
	// color cursor
	CreateBar(0,0,4,4,0xff000000);
	ItemSetInt(IV_ID, ID_DLGITEM+9);
	if(cursor!=-1) 
		DlgTileMap_SetColor(cursor);
	else
		ItemSetInt(IV_HIDDEN,1);
	
	
	// apply
	tileid = ToolBrushGet(BRUSH_TILE);
	DlgTileMap_SetTile(tileid);	
	DlgTileMap_Update();
	DlgMoveToMouse(); DlgDockUp();
}

func DlgTileMap_Close()
{
	ItemSelect(ItemFind(ID_DLGITEM+1));
	if( (ItemGetInt(IV_GUITILEMAP_MAP+2)-ItemGetInt(IV_GUITILEMAP_MAP+0)>0) &&
		(ItemGetInt(IV_GUITILEMAP_MAP+3)-ItemGetInt(IV_GUITILEMAP_MAP+1)>0) )
	{
		ToolBrushSet(BRUSH_MAP+0,ItemGetInt(IV_GUITILEMAP_MAP+0));
		ToolBrushSet(BRUSH_MAP+1,ItemGetInt(IV_GUITILEMAP_MAP+1));
		ToolBrushSet(BRUSH_MAP+2,ItemGetInt(IV_GUITILEMAP_MAP+2));
		ToolBrushSet(BRUSH_MAP+3,ItemGetInt(IV_GUITILEMAP_MAP+3));
	}

	g_tilemap_scale = ItemGetInt(IV_GUITILEMAP_SCALE);
	g_tilemap_snap = ItemGetInt(IV_GUITILEMAP_SNAP);
	g_tilemap_grid = ItemGetInt(IV_GUITILEMAP_GRID);
	g_tilemap_axes = ItemGetInt(IV_GUITILEMAP_AXES);
	gs_inisetint( INIFILE, "editor", "tilemap_scale", 	g_tilemap_scale );
	gs_inisetint( INIFILE, "editor", "tilemap_snap",	g_tilemap_snap );
	gs_inisetint( INIFILE, "editor", "tilemap_grid",	g_tilemap_grid );
	gs_inisetint( INIFILE, "editor", "tilemap_axes",	g_tilemap_axes );
}

func DlgTileMap_Update()
{
	scale = g_tilemap_scale;
	
	dlgwmin = 200;
	
	// resize
	ItemSelect(ItemFind(ID_DLGITEM+0));
	tileid = ItemGetInt(IV_VALUE);
	tileidx = TileFind(tileid); 
	if(tileidx==-1) return;
	tilew = TileGet(tileidx,TILE_W);
	tileh = TileGet(tileidx,TILE_H);
	if(tilew>512) tilew=512;
	if(tileh>600-32-16) tileh=600-32-16;
	
	space = 16; // space arround tile
	dlgw = tilew*scale + space*2;
	dlgh = tileh*scale + space*2 + DLGTITLEH;
	if(dlgw<dlgwmin) dlgw=dlgwmin;
	DlgResize(DlgGetInt(DV_X), DlgGetInt(DV_Y), dlgw, dlgh);

	itemx = dlgw/2 - tilew*scale/2;
	itemy = DLGTITLEH+16+(dlgh-DLGTITLEH-16)/2 - tileh*scale/2;
	
	ItemSelect(ItemFind(ID_DLGITEM+0));
	ItemSetInt(IV_RECT, itemx, itemy, itemx+tilew*scale, itemy+tileh*scale);
	ItemSetInt(IV_GUITILE_SCALE, scale);
	
	ItemSelect(ItemFind(ID_DLGITEM+1));
	ItemSetInt(IV_RECT, itemx, itemy, itemx+tilew*scale, itemy+tileh*scale);
	ItemSetInt(IV_GUITILEMAP_SCALE, scale);
}

func DlgTileMap_Key(key)
{
	if(key==KEY_ADD || key==KEY_EQUALS)
	{
		if(g_tilemap_scale<4) g_tilemap_scale++;
		DlgTileMap_Update();
	}
	else
	if(key==KEY_SUBTRACT || key==KEY_MINUS)
	{
		if(g_tilemap_scale>1) g_tilemap_scale--;
		DlgTileMap_Update();
	}
	else
	if(key==KEY_PGUP)
	{
		tileid = ToolBrushGet(BRUSH_TILE);
		tileidx = TileFind(tileid);
		tileidx--;
		if(tileidx==-1) tileidx = TileCount()-1;
		tileid = TileGet(tileidx,TILE_ID);
		DlgTileMap_SetTile(tileid);
		DlgTileMap_Update();
	}
	else
	if(key==KEY_PGDN)
	{
		tileid = ToolBrushGet(BRUSH_TILE);
		tileidx = TileFind(tileid);
		tileidx++;
		if(tileidx==TileCount()) tileidx = 0;
		tileid = TileGet(tileidx,TILE_ID);
		DlgTileMap_SetTile(tileid);
		DlgTileMap_Update();
	}
	else
	if(key==KEY_HOME)
	{
		tileidx=0;
		tileid = TileGet(tileidx,TILE_ID);
		DlgTileMap_SetTile(tileid);
		DlgTileMap_Update();
	}	
	else
	if(key==KEY_END)
	{
		tileidx=TileCount()-1;
		tileid = TileGet(tileidx,TILE_ID);
		DlgTileMap_SetTile(tileid);
		DlgTileMap_Update();
	}
}

func DlgTileMap_SetTile( tileid )
{
	ToolBrushSet(BRUSH_TILE, tileid);
	
	ItemSelect( ItemFind(ID_DLGITEM+0) );
	ItemSetInt(IV_VALUE, tileid);

	ItemSelect( ItemFind(ID_DLGITEM+1) );
	ItemSetInt(IV_VALUE, tileid);

	// title	
	ItemSelect(ItemFind(ID_DLGTITLE));
	tileidx = TileFind(tileid);
	ItemSetTxt(IV_TXT, "Mapping "+(str)tileidx+" / "+(str)TileCount()+"  id="+(str)tileid);
	if(tileidx!=-1)	ItemSetTxt(IV_TOOLTIP,TileGet(tileidx,TILE_NAME));
	else			ItemSetTxt(IV_TOOLTIP,"");
}

func DlgTileMap_SetColor( idx )
{
	color = g_color_used[idx];
	ItemSelect(ItemFind(ID_DLGITEM+9));
	w=12;
	x = 16 + idx*w + w/2 - 2;
	y = DLGTITLEH+6 + w/2 - 2;
	ItemSetInt(IV_POS,x,y,x+4,y+4);
	ItemSetInt(IV_HIDDEN,0);
	ToolBrushSet(BRUSH_COLOR,color);
}

//////////////////////////////////////////////////////////////////////////////////
