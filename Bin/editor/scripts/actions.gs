//////////////////////////////////////////////////////////////////////////////////
// Actions
//////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////
// MenuBar buttons and their actions
//////////////////////////////////////////////////////////////////////////////////
func Act_Menu()
{
	datatab = { 
		{"File",""},
		{" new",  		"DlgClose();Act_FileNew();",		KEY_N, 	"new map [N]"},
		{" open", 		"DlgClose();Act_FileOpen();",		KEY_O, 	"open map [O]"},
		{" save as",	"DlgClose();Act_FileSave();",		KEY_S, 	"save map [S]"},
		{" export", 	"DlgClose();Act_FileExport();",		KEY_E, 	"export map as image [E]"},
		{" info", 		"DlgClose();Act_FileInfo();",		KEY_I, 	"info about current map [I]"},
		{"Editor",""},
		{" options", 	"DlgClose();Act_Options();", 		KEY_P, "change editor preferences [P]"},
		{" help", 		"DlgClose();Act_Help();",			KEY_H, "open editor help [H]"},
		{" exit", 		"DlgClose();Act_Exit();", 			KEY_X, "exit editor [X]"}
	};
	CreatePullDown( 0,0, &datatab );
	DlgMoveToMouse(); DlgDockUp();
}

//................................................................................
//func Act_View()
//{
//	sel = g_view;
//	CreatePullDownSelect( 0, 0, "Act_ViewSet", g_viewname, sel );
//	DlgMoveToMouse(); DlgDockUp();
//}
//func Act_ViewSet( view )
//{
//	g_view = view;
//	MapRefresh();
//}

//................................................................................
//func Act_Tool()
//{
//	tool = !EdiGet(EDI_TOOL); // toggle tool
//	EdiSet(EDI_TOOL,tool);
//	DlgMenuBar_Refresh();
//}

//................................................................................
func Act_Props()
{
	if(EdiGet(EDI_TOOL)!=0) return;
	DlgProps_Create();
}

//................................................................................
func Act_Tile()
{
	if(EdiGet(EDI_TOOL)!=0) return;
	if(TileCount()==0) return;
	DlgTileBrowse_Create(0,0,"Act_TileBrowseSet();");
	DlgMoveToMouse(); DlgDockUp();
}

func Act_TileBrowseSet()
{
	ret = DlgGetInt(DV_CLOSERET);
	tileid = DlgGetInt(DV_USER);
	if(ret==1 && tileid!=-1)
	{
		tileidx = TileFind(tileid);
		frames = TileGet(tileidx,TILE_FRAMES); 
		if(frames<=0) frames=1; // safety
		ToolBrushSet(BRUSH_TILE,tileid);
		ToolBrushSet(BRUSH_MAP+0,0);
		ToolBrushSet(BRUSH_MAP+1,0);
		ToolBrushSet(BRUSH_MAP+2,TileGet(tileidx,TILE_W));
		ToolBrushSet(BRUSH_MAP+3,TileGet(tileidx,TILE_H));
	
		Act_Mapping();
	}
}

//................................................................................
func Act_Mapping()
{
	if(EdiGet(EDI_TOOL)!=0) return;
	if(TileCount()==0) return;
	DlgTileMap_Create();
}
	
//................................................................................
//func Act_Flip()
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	sel = ToolBrushGet(BRUSH_FLIP);
//	CreatePullDownSelect( 0, 0, "Act_FlipSet", g_brushprop[BRUSH_FLIP][BRUSHPROP_BROWSESEL], sel );
//	DlgMoveToMouse(); DlgDockUp();
//}
// call without param to toggle
//func Act_FlipSet( flip ) 
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	if(!?flip)
//	{
//		flip = ToolBrushGet(BRUSH_FLIP);
//		flip++; if(flip>FLIP_XYR) flip=0;
//	}
//	ToolBrushSet(BRUSH_FLIP,flip);
//}

//func Act_JustFlip() 
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	flip = ToolBrushGet(BRUSH_FLIP) & FLIP_XY;
//	flip++; if(flip>FLIP_XY) flip=0;
//	ToolBrushSet(BRUSH_FLIP, (ToolBrushGet(BRUSH_FLIP) & FLIP_R) | flip);
//}

func Act_JustRotate()
{
	if(EdiGet(EDI_TOOL)!=0) return;
	rotation = (ToolBrushGet(BRUSH_FLIP) & FLIP_R) ? 0 : FLIP_R; // reverse rotation
	ToolBrushSet(BRUSH_FLIP, (ToolBrushGet(BRUSH_FLIP) & FLIP_XY) | rotation);
}

//................................................................................
func Act_Color()
{
	if(EdiGet(EDI_TOOL)!=0) return;
	color = ToolBrushGet(BRUSH_COLOR);
	DlgColor_Create( 0, 0, "Act_ColorSet", color );
	DlgMoveToMouse(); DlgDockUp();
}

func Act_ColorSet( color )
{
	ToolBrushSet(BRUSH_COLOR,color);
	PushUsedColor(color);
}

func Act_ColorWin()
{
	color = ToolBrushGet(BRUSH_COLOR);
	ret = WinDlgOpenColor(&color);
	if(ret)
	{
		ToolBrushSet(BRUSH_COLOR,color);
		PushUsedColor(color);
	}
}

//................................................................................
//func Act_Shader()
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	sel = ToolBrushGet(BRUSH_SHADER);
//	CreatePullDownSelect( 0, 0, "Act_ShaderSet", g_brushprop[BRUSH_SHADER][BRUSHPROP_BROWSESEL], sel ); // user can add up to 10 shaders
//	DlgMoveToMouse(); DlgDockUp();
//}
//func Act_ShaderSet( shader )
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	ToolBrushSet(BRUSH_SHADER,shader);
//}

//................................................................................
//func Act_Type()
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	sel = ToolBrushGet(BRUSH_TYPE);
//	CreatePullDownSelect( 0, 0, "Act_TypeSet", g_brushprop[BRUSH_TYPE][BRUSHPROP_BROWSESEL], sel );
//	DlgMoveToMouse(); DlgDockUp();
//}
//func Act_TypeSet( value )
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	ToolBrushSet(BRUSH_TYPE,value);
//	MOD_BrushNew(value);
//}

//................................................................................
//func Act_Draw()
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	sel = ToolBrushGet(BRUSH_DRAW);
//	CreatePullDownSelect( 0, 0, "Act_DrawSet", g_brushprop[BRUSH_DRAW][BRUSHPROP_BROWSESEL], sel );
//	DlgMoveToMouse(); DlgDockUp();
//}
//func Act_DrawSet( value )
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	ToolBrushSet(BRUSH_DRAW,value);
//}

//................................................................................
//func Act_Material()
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	sel = ToolBrushGet(BRUSH_MATERIAL);
//	CreatePullDownSelect( 0, 0, "Act_MaterialSet", g_brushprop[BRUSH_MATERIAL][BRUSHPROP_BROWSESEL], sel );
//	DlgMoveToMouse(); DlgDockUp();
//}

//func Act_MaterialSet( value )
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	ToolBrushSet(BRUSH_MATERIAL,value);
//}

//................................................................................
//func Act_Class()
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	sel = ToolBrushGet(BRUSH_CLASS);
//	CreatePullDownSelect( 0, 0, "Act_ClassSet", g_brushprop[BRUSH_CLASS][BRUSHPROP_BROWSESEL], sel );
//	DlgMoveToMouse(); DlgDockUp();
//}

//func Act_ClassSet( value )
//{
//	if(EdiGet(EDI_TOOL)!=0) return;
//	ToolBrushSet(BRUSH_CLASS,value);
//}

//................................................................................
func Act_Search()
{
	if(EdiGet(EDI_TOOL)!=1) return;
	ScrBrushSearch();
}

//................................................................................
func Act_Change()
{
	if(EdiGet(EDI_TOOL)!=1) return;
	ScrBrushChange();
}

//................................................................................
func Act_Script()
{
	if(EdiGet(EDI_TOOL)!=1) return;
	datatab = { 
		{"Selection",""},
		{" invert", 			"DlgClose();ScrBrushInvert();",				KEY_I, 	"invert selection [I]"},
		{" move", 				"DlgClose();ScrBrushMove();", 				KEY_M, 	"move brushes [M]"},
		{" select by index",	"DlgClose();ScrSelectByIdx();", 			-1,		"select a brush by it's index"},
		{" keep topmost", 		"DlgClose();ScrBrushKeepTopmost();",		-1,		"keep the topmost brush from the current selection"},
		{"Custom",""},
		{" set block", 			"DlgClose();ScrCustomBlocking(1);",			KEY_B, 	"set material to blocking [B]"},
		{" set unblock",		"DlgClose();ScrCustomBlocking(0);",			KEY_U, 	"set material to unblocking [U]"},
		{" set group ids",		"DlgClose();ScrBrushGroupIds();",			-1, 	"set group ids for the brushes in the selection"}
	};
	CreatePullDown( 0,0, &datatab );
	DlgMoveToMouse(); DlgDockUp();
}

//................................................................................
func Act_Script2()
{
	if(EdiGet(EDI_TOOL)!=1) return;
	datatab = { 
		{"Release checks",""},
		{" check missing tiles",	"DlgClose();ScrCheckTile();",				-1,		"select all brushes with missing tiles"},
		{" check duplicate ids",	"DlgClose();ScrCheckId();",					-1,		"select all brushes with duplicate ids"},
		{" check overlapping",		"DlgClose();ScrCheckOverlapping();",		-1,		"select overlapping brushes\n(position,size,tile and map)"},
		{" check dynamic ids",		"DlgClose();ScrCheckDynamicBrushId();",		-1,		"select all dynamic brushes without valid ids"},
		{" check static ids",		"DlgClose();ScrCheckStaticBrushId();",		-1,		"select all static brushes with valid ids"},
		{" count rooms",			"DlgClose();ScrCountRooms();",				-1,		"count and mark all the rooms with brush content"}
	};
	CreatePullDown( 0,0, &datatab );
	DlgMoveToMouse(); DlgDockUp();
}

//................................................................................
func Act_Script3()
{
	if(EdiGet(EDI_TOOL)!=1) return;
	datatab = MOD_GetUserScripts();
	CreatePullDown( 0,0, &datatab );
	DlgMoveToMouse(); DlgDockUp();
}

//////////////////////////////////////////////////////////////////////////////////
// LAYERS
//////////////////////////////////////////////////////////////////////////////////
func Act_Layer( layer )
{
	param = ItemGetInt(IV_CMDACTIONPARAM);
	ItemSetInt(IV_CMDACTIONPARAM,0);
	if(layer<0 || layer>=LAYER_MAX) return;
	if(param==1) // activate set
	{
		// deactivate old active layer
		act = LayerActive();
		if(act==-1) return; // safety
		LayerSetButton(act,1);
		LayerSetButton(layer,2);
	}	
	else
	if(param==2) // visible toggle
	{
		act = LayerActive();
		if(act==layer) // cant hide active layer, keep button active
		{
			LayerSetButton(layer,2);
		}
		else
		{
			state = LayerGet(layer);
			state = state?0:1;
			LayerSetButton(layer,state);
		}
	}
	MapRefresh();
}


//////////////////////////////////////////////////////////////////////////////////
// File
//////////////////////////////////////////////////////////////////////////////////
func Act_FileNew()
{
	MsgBox("Question", "Do you want to create a new map?\n( current map will be lost if not saved )", ICON_QUESTION, {{"YES","Act_FileNewDo();"}, {"NO",""}} );
}
func Act_FileNewDo()
{
	MapReset();
	RoomNamesReset(0);
	RoomTextsReset(0);
	RoomPropsReset(0);
	Act_FileInfo();
}

func Act_FileOpen()
{
	ToolReset(); // safe
	ret = WinDlgOpenFile( &g_mapfile, "map", 0 );
	if(ret)
	{
		WaitCursor(1);
		ret = MAPLoad(g_mapfile);
		WaitCursor(0);
		if(!ret) MsgBoxOk("Error", "File open failed.\nFile might be incorrect or damaged.", ICON_ERROR );
	}
}

func Act_FileSave( silent )
{
	if(!?silent) silent=0;
	if(g_mapfile=="") g_mapfile = "noname.map";
	if(!silent || g_mapfile=="noname.map")
	{
		ToolReset(); // safe
		ret = WinDlgOpenFile( &g_mapfile, "map", 1 );
	}
	else 
		ret = 1;
	if(ret)
	{
		WaitCursor(1);
		ret = MAPSave(g_mapfile);
		WaitCursor(0);
		if(!ret) 
			MsgBoxOk("Error", "File save failed.", ICON_ERROR );
		else
			MsgBoxOk("Message", "File save successful.", ICON_INFO);
	}
}

func Act_FileExport()
{
	datatab = { 
		{"Export",""},
		{" image map", 	"DlgClose();MAPExportImage();",	-1, 	"export image map"},
		{" text map", 	"DlgClose();MAPExportText();",	-1, 	"export text map"},
		{" brushes ids","DlgClose();ExportBrushIds(1);",	-1,		"export ids from all selected brushes"}
	};
	CreatePullDown( 0,0, &datatab );
	DlgMoveToMouse();// DlgDockUp();	
}

func Act_FileInfo()
{
	DlgInfo_Create();
}


//////////////////////////////////////////////////////////////////////////////////
// Options
//////////////////////////////////////////////////////////////////////////////////
func Act_Options()
{
	DlgOptions_Create();
}

func Act_SnapSet( snap )
{
	if(!?snap) snap = !EdiGet(EDI_SNAP); // toggle
	EdiSet(EDI_SNAP,snap);
}	

func Act_GridSet( grid )
{
	if(!?grid) grid = !EdiGet(EDI_GRID); // toggle
	EdiSet(EDI_GRID,grid);
	MapRefresh();
}

func Act_AxesSet( axes )
{
	if(!?axes) axes = !EdiGet(EDI_AXES); // toggle
	EdiSet(EDI_AXES,axes);
}

func Act_ZoomSet( step )
{
	zoom = EdiGet(EDI_ZOOM);
	zoom+=step;
	if(zoom<1) zoom=1;
	if(zoom>4) zoom=4;
	EdiSet(EDI_ZOOM,zoom);
	MapRefresh();
}


//////////////////////////////////////////////////////////////////////////////////
// others
//////////////////////////////////////////////////////////////////////////////////
func Act_RoomProps()
{
	if(EdiGet(EDI_TOOL)!=0) return;
	rx = ToolBrushGet(BRUSH_X)/EdiGet(EDI_ROOMW);
	ry = ToolBrushGet(BRUSH_Y)/EdiGet(EDI_ROOMH);
	DlgRoomProps_Create(rx,ry);
}

func Act_Help()
{
	DlgHelp_Create();
}

func Act_Exit()
{
	EDI_Close();
}


//////////////////////////////////////////////////////////////////////////////////
// Tool special callback (called from code)
//////////////////////////////////////////////////////////////////////////////////
int g_tool_brushidx;
func Tool_PickMenu( brushidx )
{
	g_tool_brushidx = brushidx;
//	CreatePullDownSelect( MouseX()+4,MouseY()+4, "Tool_PickMenuSet", {"prop","pick brush","pick color","to front","to back","delete"} );
	datatab = { 
		{"prop",  		"DlgClose();DlgProps_Create(0,"+(str)brushidx+");",		KEY_P, 		"properties [P]"},
		{"pick brush",	"DlgClose();ToolCommand(TOOLCMD_PICKBRUSH);",			-1, 		"pick brush"},
		{"pick tile",	"DlgClose();ToolCommandPickBrush("+(str)brushidx+");",	KEY_T, 		"pick tile [T]"},
		{"pick color",	"DlgClose();ToolCommand(TOOLCMD_PICKCOLOR);",			KEY_C, 		"pick color [C]"},
		{"to front",	"DlgClose();ToolCommand(TOOLCMD_TOFRONT);",				KEY_F, 		"bring to front [F]"},
		{"to back",		"DlgClose();ToolCommand(TOOLCMD_TOBACK);",				KEY_B, 		"send to back [B]"},
		{"delete",		"DlgClose();ToolCommand(TOOLCMD_DELETE);",				KEY_DELETE, "delete [DEL]"}
	};
	CreatePullDown( MouseX()+4, MouseY()+4, &datatab );
	y = DlgGetInt(DV_Y);
	h = DlgGetInt(DV_Y2)-y;	
	if(y>ScrH()-h)
		DlgMove( DlgGetInt(DV_X), ScrH()-h );
}

func ToolCommandPickBrush( brushidx )
{
	ToolBrushSet(BRUSH_TILE,		MapBrushGet(brushidx,BRUSH_TILE));
	ToolBrushSet(BRUSH_MAP+0,	MapBrushGet(brushidx,BRUSH_MAP+0));
	ToolBrushSet(BRUSH_MAP+1,	MapBrushGet(brushidx,BRUSH_MAP+1));
	ToolBrushSet(BRUSH_MAP+2,	MapBrushGet(brushidx,BRUSH_MAP+2));
	ToolBrushSet(BRUSH_MAP+3,	MapBrushGet(brushidx,BRUSH_MAP+3));
	ToolBrushSet(BRUSH_FLIP,		MapBrushGet(brushidx,BRUSH_FLIP));
}

//////////////////////////////////////////////////////////////////////////////////
