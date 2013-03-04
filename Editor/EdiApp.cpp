//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiApp.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"

#include <iostream>

#include "Resource.h"
#include "E9App.h"
#include "EdiApp.h"
#include "Resource.h"

#include "SWI-cpp-m.h"

#include "EdiMap.h"
#include "Gui.h"
#include "GuiTile.h"

cEdiApp* cEdiApp::m_app = NULL;

PREDICATE_M(core, dl, 1)
{
	LPWSTR msg = A1;
	dlog(LOGGS, msg);
	dlog(LOGGS, L"\n");
	return true;
}

PREDICATE_M(core, ini, 4)
{
	static char tmp_fullpath[256];
	if(!GetFullPathName(A1, 255, tmp_fullpath, NULL )) 
		return false;

	PlTerm val = A4;	
	if(val.type() == PL_VARIABLE)
	{
		char value[256]; value[0]=0;
		if(GetPrivateProfileString(A2, A3, "", value, 256, tmp_fullpath ) == 0)
			return false;
		PlTerm ct;
		if (PL_chars_to_term(value, ct))
			return A4 = ct;
		PlTerm at;
		if(PL_unify_atom_chars(at, value))
			return A4 = at;
		throw PlResourceError(ct);
	}
	WritePrivateProfileString(A2, A3, val, tmp_fullpath );
	return true;
}

PREDICATE_M(core, tickCount, 1)
{
	return A1 = static_cast<int>(GetTickCount());
}


cEdiApp::cEdiApp()
{
	guard(cEdiApp::cEdiApp)

	m_exit				= 0;
	m_axes				= 0;
	m_snap				= 0;
	m_grid				= 1;
	m_gridsize			= 8;
	
	m_color[EDI_COLORBACK1-EDI_COLOR]		= 0xff202020;
	m_color[EDI_COLORBACK2-EDI_COLOR]		= 0xff808080;
	m_color[EDI_COLORGRID1-EDI_COLOR]		= 0x40ffffff;
	m_color[EDI_COLORGRID2-EDI_COLOR]		= 0x80ffffff;
	m_color[EDI_COLORGRID3-EDI_COLOR]		= 0x80ffff00;
	m_color[EDI_COLORMAP-EDI_COLOR]			= 0xff000000;
	
	strcpy(m_mapid,		"mapid");

	m_toolcrt			= TOOL_PAINT;
	m_tool[TOOL_PAINT]	= snew cEdiToolPaint();
	m_tool[TOOL_EDIT]	= snew cEdiToolEdit();

	// reset brush
	memset(&m_brush,0,sizeof(m_brush));
	m_brush.m_data[BRUSH_TILE]	= 0; // brush 0 must exist
	m_brush.m_data[BRUSH_MAP+0]	= 0;
	m_brush.m_data[BRUSH_MAP+1]	= 0;
	m_brush.m_data[BRUSH_MAP+2]	= 8;
	m_brush.m_data[BRUSH_MAP+3]	= 8;
	m_brush.m_data[BRUSH_COLOR] = 0xffffffff;

	for(int i=0;i<LAYER_MAX;i++) m_layer[i]=1;
	m_layer[0]=2;

	m_drawstats = 0;

	m_mscrollx = 0;
	m_mscrolly = 0;

	m_app = this;
	unguard()
}

cEdiApp::~cEdiApp()
{
	guard(cEdiApp::~cEdiApp)
	m_app = NULL;
	sdelete(m_tool[TOOL_PAINT]);
	sdelete(m_tool[TOOL_EDIT]);
	unguard()
}

BOOL cEdiApp::Init()
{
	guard(cEdiApp::Init)
	dlog(LOGAPP, L"App init.\n");

	// engine
	if(!InitApp())	 { ErrorMessage(L"Init app error.");   return FALSE; }
	if(!InitFiles()) { ErrorMessage(L"Init files error."); return FALSE; }
	if(!InitInput()) { ErrorMessage(L"Init input device error."); return FALSE; }
	if(!InitVideo()) { ErrorMessage(L"Init video device error."); return FALSE; }

	// editor
	g_paint.Init();
	g_map.Init();
	BOOL ret = GUIInit();
	g_gui->ScriptPrologDo("editor:init");

	// tools
	m_tool[TOOL_PAINT]->Init();
	m_tool[TOOL_EDIT]->Init();
	m_toolcrt = TOOL_PAINT;
	m_tool[m_toolcrt]->Switch(TRUE);

	// load param
	if(E9_AppGetStr(E9_APP_CMDLINE) && strstr(E9_AppGetStr(E9_APP_CMDLINE), "pmp"))
	{
		std::ostringstream s;
		s<< "editor:load('" << E9_AppGetStr(E9_APP_CMDLINE) << "')";
		g_gui->ScriptPrologDo(s.str());
	}

	// accept drop files
	long styleex = GetWindowLong(E9_GetHWND(),GWL_EXSTYLE) | WS_EX_ACCEPTFILES;
	SetWindowLong(E9_GetHWND(),GWL_EXSTYLE,styleex);

	return TRUE;
	unguard()
}

BOOL cEdiApp::InitApp()
{
	guard(cEdiApp::InitApp);
	E9_AppSetStr(E9_APP_NAME,EDI_NAME);
	E9_AppSetStr(E9_APP_ICON,MAKEINTRESOURCE(IDI_ICON));

	BOOL cool = TRUE;
	ini_getint( file_getfullpath(USER_INIFILE), "EDITOR", "options_cool",	&cool );
	E9_AppSetInt(E9_APP_COOL,cool);
	
	return TRUE;
	unguard();
}

BOOL cEdiApp::InitFiles()
{
	guard(cEdiApp::InitFiles);
	BOOL ok = F9_Init();
	if(!ok) return FALSE;
	int arc = F9_ArchiveOpen("editor.pak", F9_READ | F9_ARCHIVE_PAK );
	dlog(LOGAPP, L"using editor.pak file.\n");
	return TRUE;
	unguard();
}

BOOL cEdiApp::InitInput()
{
	guard(cEdiApp::InitInput);
	BOOL ok = I9_Init(E9_GetHWND(),E9_GetHINSTANCE(),I9_API_DEFAULT);
	if(!ok) return FALSE;

	// init devices
	ok &= I9_DeviceInit(I9_DEVICE_KEYBOARD);
	ok &= I9_DeviceInit(I9_DEVICE_MOUSE);
	if(!ok) return FALSE;
	
	return TRUE;
	unguard();
}

BOOL cEdiApp::InitVideo()
{
	guard(cEdiApp::InitVideo);
	char inifile[256];
	strcpy( inifile, file_getfullpath(USER_INIFILE) );

	int screensize = 1;
	int delta = 64; // substract from screen size
	ini_getint( inifile, "EDITOR", "options_screensize",	&screensize);
	if(screensize<0||screensize>=4) screensize=1;
	int screensizelist[4][2]={ {640,480}, {800,600}, {1024,768}, {1280,1024} };

	// custom sizes
	ini_getint( inifile, "EDITOR", "options_screenw",	&screensizelist[3][0]);
	ini_getint( inifile, "EDITOR", "options_screenh",	&screensizelist[3][1]);

	// default config
	r9Cfg cfg;
	int api			= R9_API_DEFAULT;
	cfg.m_windowed	= 1;
	cfg.m_bpp		= 32;
	cfg.m_width		= screensizelist[screensize][0]-delta/2;
	cfg.m_height	= screensizelist[screensize][1]-delta;
	cfg.m_refresh	= 85;

	// load config
	ini_getint( inifile, "EDITOR", "options_videoapi",		&api );

	// init interface
	if(!R9_InitInterface(api)) return FALSE;

	BOOL ok = R9_Init(E9_GetHWND(),&cfg,api);
	if(!ok) // try the other api
	{
		dlog(L"RENDER: init %S (api %i) failed, try the other api.\n",api?"OpenGL":"DirectX9",api);
		ok = R9_Init(E9_GetHWND(),&cfg,!api);
		if(!ok)	return FALSE;
	}

	R9_SetHandleReset(HandleReset);
	R9_SetState(R9_STATE_FILTER,FALSE);
	E9_AppSetInt(E9_APP_WINDOWED,cfg.m_windowed);

	return TRUE;
	unguard();
}

void cEdiApp::Done()
{
	guard(cEdiApp::Done)
	// must be able to destroy partial init too, in case Init has failed

	// script exit
	if(g_gui) g_gui->ScriptPrologDo("editor:done");

	// tools
	m_tool[m_toolcrt]->Switch(FALSE);
	m_tool[TOOL_PAINT]->Done();
	m_tool[TOOL_EDIT]->Done();
	
	// editor
	GUIDone();
	g_map.Done();
	g_paint.Done();

	// engine
	R9_Done();
	R9_DoneInterface();
	I9_Done();
	F9_ArchiveClose(0); // close first archive if found
	F9_Done();

	dlog(LOGAPP, L"App done.\n");
	unguard()
}

void cEdiApp::Activate( BOOL active )
{
	guard(cEdiApp::Activate)
	if(active)
	{
		if(I9_IsReady()) I9_Acquire();
	}
	else
	{
		if(I9_IsReady()) I9_Unacquire();
	}
	m_tool[m_toolcrt]->Reset();
	if(g_gui) g_gui->SetTooltip("");
	if(g_map.m_scrolling)
	{
		g_map.m_scrolling = 0;
		E9_AppSetCursor(E9_CURSOR_ARROW);
	}
	::InvalidateRect(E9_GetHWND(),NULL,0);
	unguard()
}

void cEdiApp::Close()
{
	guard(cEdiApp::Close);
	//if(g_gui->m_isbusy) return; // can't close now !
	int mode = m_tool[m_toolcrt]->m_mode;
	if(m_toolcrt==0 && mode==1) return;
	if(m_toolcrt==1 && (mode==1||mode==2)) return;
	m_tool[m_toolcrt]->Reset();
	g_gui->ScriptPrologDo("editor:close");
	g_gui->m_isbusy = TRUE; // avoid tools problems
	unguard()
}

void cEdiApp::DropFile( LPCWSTR filepath )
{
	guard(cEdiApp::DropFile);
	if(g_gui->m_isbusy) { BEEP_ERROR; return ; } // gui busy (modal dialog)
	if(!wcsstr(filepath, L".pmp")) { BEEP_ERROR; return ; } // not map
	
	try
	{
		PlCall("editor", "load", PlTermv(PlTerm(PlAtom(filepath))));
	}
	catch(PlException const & e)
	{
		PlException ee(e);
		LPCWSTR msg = static_cast<LPCWSTR>(ee);
		dlog(L"Exception: %s", msg);
	}


	g_map.Update(0.0f);
	g_map.Refresh();
	Draw();
	unguard();
}

void cEdiApp::Scroll( int dx, int dy )
{
	guard(cEdiApp::Scroll);
	m_mscrollx = dx;
	m_mscrolly = dy;
	unguard();
}

void cEdiApp::HandleReset()
{
	guard(cEdiApp::HandleReset);
	g_map.m_refresh = TRUE; // refresh map
	g_map.Refresh();
	EdiApp()->Draw();
	unguard();
}

BOOL cEdiApp::Update()
{
	guard(cEdiApp::Update)
	float dtime = (float)E9_AppGetInt(E9_APP_DELTATIME) / 1000.0f;

	// input
	if(!I9_IsReady()) return TRUE;
	I9_Update(dtime);

	// map
	if(!g_gui->m_isbusy)
		g_map.Update(dtime);
	BOOL map_isbusy = g_map.m_scrolling;
	
	// reset tooltip
	g_gui->SetTooltip("");

	static BOOL gui_wasbusy=FALSE;
	BOOL tool_isbusy = FALSE;

	// tool
	if(!g_gui->m_isbusy && !map_isbusy)
	{
		m_tool[m_toolcrt]->Update( dtime );
		tool_isbusy = m_tool[m_toolcrt]->m_isbusy;
	}
	else
	if(!gui_wasbusy)
	{
		m_tool[m_toolcrt]->Reset(); // reset tool mode
		tool_isbusy = FALSE;
	}
	gui_wasbusy = g_gui->m_isbusy;

	// statusbar
	m_tool[m_toolcrt]->BeginUserUpdate();
	g_gui->ScriptPrologDo("mod:userUpdate");
	m_tool[m_toolcrt]->EndUserUpdate();

	// gui
	g_gui->ReadInput();
	if(!tool_isbusy && !map_isbusy)
		g_gui->Update();

	// toggle info
	if(I9_GetKeyDown(I9K_F11)) m_drawstats = !m_drawstats;

	// print screen
	if(I9_GetKeyDown(I9K_SYSRQ)) 
	{
		BOOL ctrl = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
		fRect r(0,0,R9_GetWidth(),R9_GetHeight());
		R9_SaveScreenShot(&r,!ctrl);
	}

	if(m_exit) return FALSE;
	return TRUE;
	unguard()
}


void cEdiApp::Draw()
{
	guard(cEdiApp::Draw)
	if(!R9_IsReady()) return; // avoid painting if render is not ready
	R9_CheckDevice(); // check for lost device
	if(R9_BeginScene())
	{
		R9_Clear(EDI_COLORBACK1);

		// editor
		g_map.Draw();

		// tool
		R9_SetClipping( fRect(g_map.m_viewx, g_map.m_viewy, g_map.m_viewx+g_map.m_vieww, g_map.m_viewy+g_map.m_viewh) );
		if(g_gui && !g_gui->m_isbusy)
			m_tool[m_toolcrt]->Draw();
		R9_ResetClipping();

		// gui
		if(g_gui) g_gui->Draw();

		// stats
		if(m_drawstats) DrawStats();

		R9_EndScene();
		R9_Present();
	}

	unguard()
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Draw debug
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiApp::DrawStats()
{
	guard(cEdiApp::DrawStats)
	char sz[64];
	sprintf(sz, "fps: %i", E9_AppGetInt(E9_APP_FPS));
	
	float w = (float)R9_CHRW*(int)strlen(sz)+4;
	float h = (float)R9_CHRH+4;
	float x=R9_GetWidth()-w-2;
	float y=2;

	R9_DrawBar( fRect(x,y,x+w,y+h), 0xa0000000 );
	R9_DrawText( fV2(x+2,y+2), sz, 0xffffff80 );
	R9_Flush();
	unguard()
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// TOOLS
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiApp::ToolSet( int tool )
{
	guard(cEdiApp::ToolSet)
	if(tool<0 || tool>=TOOL_MAX) tool=0;
	if(tool==m_toolcrt) return;
	m_tool[m_toolcrt]->Switch(FALSE);
	m_toolcrt = tool;
	m_tool[m_toolcrt]->Switch(TRUE);
	unguard()
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// UTILS
//////////////////////////////////////////////////////////////////////////////////////////////////
int	cEdiApp::GetMouseX()
{
	guard(cEdiApp::GetMouseX)
	POINT mouse;
	GetCursorPos(&mouse);
	ScreenToClient(E9_GetHWND(), &mouse);
	return mouse.x;
	unguard()
}

int cEdiApp::GetMouseY()
{
	guard(cEdiApp::GetMouseY)
	POINT mouse;
	GetCursorPos(&mouse);
	ScreenToClient(E9_GetHWND(), &mouse);
	return mouse.y;
	unguard()
}

void cEdiApp::WaitCursor( BOOL on )
{
	guard(cEdiApp::WaitCursor)
	E9_AppSetCursor( on ? E9_CURSOR_WAIT : E9_CURSOR_ARROW );
	unguard()
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// UNDO
//////////////////////////////////////////////////////////////////////////////////////////////////
BOOL cEdiApp::Undo()
{
	guard(cEdiApp::Undo)
	switch(m_undoop)
	{
		case UNDOOP_ADD:
		{
			if(m_undoidx<0 || m_undoidx>g_map.m_brushcount) goto error;
			g_map.PartitionFix(m_undoidx, g_map.m_brushcount-1, 1); // fix indices before shifting
			g_map.BrushIns(m_undoidx, m_undobrush);
			g_map.PartitionAdd(m_undoidx);
			m_undoop = UNDOOP_DEL;
			g_map.m_refresh = TRUE;
			BEEP_OK;
			return TRUE;
		}
		case UNDOOP_DEL:
		{
			if(m_undoidx<0 || m_undoidx>g_map.m_brushcount) goto error;
			m_undobrush = g_map.m_brush[m_undoidx];
			g_map.PartitionDel(m_undoidx);
			g_map.PartitionFix(m_undoidx+1, g_map.m_brushcount-1, -1); // fix indices before shifting
			g_map.BrushDel(m_undoidx);
			m_undoop = UNDOOP_ADD;
			g_map.m_refresh = TRUE;
			BEEP_OK
			return TRUE;
		}
	}
error:
	BEEP_ERROR;
	return FALSE;
	unguard()
}

void cEdiApp::UndoSet( int op, int idx, tBrush* brush )
{
	guard(cEdiApp::UndoSet)
	m_undoop = op;
	m_undoidx = idx;
	if(brush) m_undobrush=*brush;
	unguard()
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// SCRIPTING
//////////////////////////////////////////////////////////////////////////////////////////////////

// tiles ...................................................................................

PREDICATE_M(edi, tileFind, 2) 
{
	int idx = g_paint.TileFind(A1);
	if(idx == -1)
		return false;
	return A2 = idx; 
}

PREDICATE_M(edi, tileCount, 1) 
{
	return A1 = g_paint.TileCount();
}


cTile *TileGet(PlTerm idx)
{
	cTile * tile = g_paint.TileGet(idx); 
	if(!tile)
		throw PlDomainError("invalid tile index", idx);
	return tile;
}

PREDICATE_M(edi, tileGetID, 2) 
{
	return A2 = TileGet(A1)->m_id; 
}

PREDICATE_M(edi, tileGetW, 2) 
{
	return A2 = TileGet(A1)->GetWidth(); 
}

PREDICATE_M(edi, tileGetH, 2) 
{
	return A2 = TileGet(A1)->GetHeight(); 
}

PREDICATE_M(edi, tileGetFrames, 2) 
{
	return A2 = TileGet(A1)->m_frames; 
}

PREDICATE_M(edi, tileGetName, 2) 
{
	cTile * t = TileGet(A1);
	return A2 = t->m_name ? t->m_name : ""; 
}

PREDICATE_M(edi, tileReload, 0)
{
	dlog(LOGAPP, L"Reload tiles ...\n");

	// clear old
	g_paint.TileUnload();

	// load from 2 ini dirs
	BOOL ok = TRUE;
	int loads = 0;
	static char tilepath[256];
	
	if(ini_getstr( file_getfullpath(USER_INIFILE), "editor", "options_tiledir", tilepath, 256 )) 
	{
		if(g_paint.TileLoad(tilepath)) loads++;
	}
	else dlog(LOGAPP, L"TileDir not specified in editor.ini\n");

	return loads > 0;
}


PREDICATE_M(edi, getTool, 1)
{
	return A1 = EdiApp()->m_toolcrt;
}

PREDICATE_M(edi, getAxes, 1)
{
	return A1 = EdiApp()->m_axes;
}

PREDICATE_M(edi, getSnap, 1)
{
	return A1 = EdiApp()->m_snap;
}

PREDICATE_M(edi, getGrid, 1)
{
	return A1 = EdiApp()->m_grid;
}

PREDICATE_M(edi, getGridSize, 1)
{
	return A1 = EdiApp()->m_gridsize;
}

PREDICATE_M(edi, getScrW, 1)
{
	return A1 = EdiApp()->GetScrW();
}

PREDICATE_M(edi, getScrH, 1)
{
	return A1 = EdiApp()->GetScrH();
}


PREDICATE_M(edi, getAxeX, 1)
{
	return A1 = EdiApp()->GetAxeX();
}

PREDICATE_M(edi, getAxeY, 1)
{
	return A1 = EdiApp()->GetAxeY();
}

PREDICATE_M(edi, getBrushRect, 1)
{
	return A1 = g_paint.m_brushrect;
}

PREDICATE_M(edi, getColorBack1, 1)
{
	return A1 = static_cast<int64>(EdiApp()->GetColor(EDI_COLORBACK1));
}

PREDICATE_M(edi, getColorBack2, 1)
{
	return A1 = static_cast<int64>(EdiApp()->GetColor(EDI_COLORBACK2));
}

PREDICATE_M(edi, getColorGrid1, 1)
{
	return A1 = static_cast<int64>(EdiApp()->GetColor(EDI_COLORGRID1));
}

PREDICATE_M(edi, getColorGrid2, 1)
{
	return A1 = static_cast<int64>(EdiApp()->GetColor(EDI_COLORGRID2));
}

PREDICATE_M(edi, getColorGrid3, 1)
{
	return A1 = static_cast<int64>(EdiApp()->GetColor(EDI_COLORGRID3));
}

PREDICATE_M(edi, getColorMap, 1)
{
	return A1 = static_cast<int64>(EdiApp()->GetColor(EDI_COLORMAP));
}

PREDICATE_M(edi, setTool, 1)
{
	EdiApp()->ToolSet(A1);
	return true;
}

PREDICATE_M(edi, setAxes, 1)
{
	EdiApp()->m_axes = A1;
	return true;
}

PREDICATE_M(edi, setSnap, 1)
{
	EdiApp()->m_snap = A1;
	return true;
}

PREDICATE_M(edi, setGrid, 1)
{
	EdiApp()->m_grid = A1;
	return true;
}

PREDICATE_M(edi, setGridSize, 1)
{
	EdiApp()->m_gridsize = A1;
	return true;
}



PREDICATE_M(edi, setBrushRect, 1)
{
	g_paint.m_brushrect = A1;
	return true;
}

PREDICATE_M(edi, setColorBack1, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORBACK1-EDI_COLOR] = l;
	return true;
}

PREDICATE_M(edi, setColorBack2, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORBACK2-EDI_COLOR] = l;
	return true;
}

PREDICATE_M(edi, setColorGrid1, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORGRID1-EDI_COLOR] = l;
	return true;
}

PREDICATE_M(edi, setColorGrid2, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORGRID2-EDI_COLOR] = l;
	return true;
}

PREDICATE_M(edi, setColorGrid3, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORGRID3-EDI_COLOR] = l;
	return true;
}


PREDICATE_M(edi, setColorMap, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORMAP-EDI_COLOR] = l;
	return true;
}

PREDICATE_M(edi, exit, 0)
{
	EdiApp()->m_exit=1;
	return true;
}

PREDICATE_M(edi, waitCursor, 1)
{
	EdiApp()->WaitCursor(A1);
	return true;
}

// layers ..................................................................................

PREDICATE_M(edi, layerGet, 2)
{
	int layer = A1;
	if(layer < 0 || layer >= LAYER_MAX) 
		throw PlDomainError ("invalid layer index", A1);
	return A2 = EdiApp()->LayerGet(layer);
}

PREDICATE_M(edi, layerSet, 2)
{
	int layer = A1;
	if(layer < 0 || layer >= LAYER_MAX) 
		throw PlDomainError("invalid layer index", A1);
	EdiApp()->LayerSet(layer, A2);
	return true;
}

PREDICATE_M(edi, layerActive, 1)
{
	return A1 = EdiApp()->LayerActive();
}

// tool brush ..............................................................................

#define TOOL_BRUSH_PROP(Prop, PROP)\
GET_TOOL_BRUSH_PROP(Prop, PROP)\
SET_TOOL_BRUSH_PROP(Prop, PROP)

#define GET_TOOL_BRUSH_PROP(Prop, PROP) PREDICATE_M(edi, toolBrushGet##Prop, 1)\
{\
	return A1 = EdiApp()->m_brush.m_data[BRUSH_##PROP];\
	return true;\
}

#define SET_TOOL_BRUSH_PROP(Prop, PROP) PREDICATE_M(edi, toolBrushSet##Prop, 1)\
{\
	EdiApp()->m_brush.m_data[BRUSH_##PROP] = A1;\
	return true;\
}

TOOL_BRUSH_PROP(Layer, LAYER)
TOOL_BRUSH_PROP(X, X)
TOOL_BRUSH_PROP(Y, Y)
TOOL_BRUSH_PROP(W, W)
TOOL_BRUSH_PROP(H, H)
TOOL_BRUSH_PROP(Tile, TILE)
TOOL_BRUSH_PROP(Frame, FRAME)
TOOL_BRUSH_PROP(MapX1, MAP)
TOOL_BRUSH_PROP(MapY1, MAP+1)
TOOL_BRUSH_PROP(MapX2, MAP+2)
TOOL_BRUSH_PROP(MapY2, MAP+3)
TOOL_BRUSH_PROP(Flip, FLIP)
TOOL_BRUSH_PROP(Shader, SHADER)
TOOL_BRUSH_PROP(Scale, SCALE)
TOOL_BRUSH_PROP(Select, SELECT)

TOOL_BRUSH_PROP(Type, TYPE)
TOOL_BRUSH_PROP(ID, ID)
TOOL_BRUSH_PROP(Material, MATERIAL)
TOOL_BRUSH_PROP(Draw, DRAW)
TOOL_BRUSH_PROP(Disable, DISABLE)
TOOL_BRUSH_PROP(Delay, DELAY)
TOOL_BRUSH_PROP(Anim, ANIM)
TOOL_BRUSH_PROP(Collider, COLLIDER)
TOOL_BRUSH_PROP(Class, CLASS)
TOOL_BRUSH_PROP(Status, STATUS)
TOOL_BRUSH_PROP(Target, TARGET)
TOOL_BRUSH_PROP(Death, DEATH)

PREDICATE_M(edi, toolBrushSetColor, 1)
{
	int64 color = A1;
	EdiApp()->m_brush.m_data[BRUSH_COLOR] = color;
	return true;
}

PREDICATE_M(edi, toolBrushSetCustom, 2) 
{
	int idx = A1;
	idx += BRUSH_CUSTOM;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A1);
	EdiApp()->m_brush.m_data[idx] = A2;
	return true;
}

PREDICATE_M(edi, toolBrushSetUser, 2) 
{
	int idx = A1;
	idx += BRUSH_USER;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A1);
	EdiApp()->m_brush.m_data[idx] = A2;
	return true;
}

PREDICATE_M(edi, toolBrushSet, 2) 
{
	int idx = A1;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A1);
	if(idx == BRUSH_COLOR) 
	{
		int64 color = A2;
		EdiApp()->m_brush.m_data[idx] = color;
	}
	else {
		EdiApp()->m_brush.m_data[idx] = A2;
	}
	return true;
}


PREDICATE_M(edi, toolBrushGetColor, 1)
{
	int64 color = static_cast<unsigned>(EdiApp()->m_brush.m_data[BRUSH_COLOR]);
	return A1 = color;
}

PREDICATE_M(edi, toolBrushGetCustom, 2) 
{
	int idx = A1;
	idx += BRUSH_CUSTOM;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A1);
	return A2 = EdiApp()->m_brush.m_data[idx];
}

PREDICATE_M(edi, toolBrushGetUser, 2) 
{
	int idx = A1;
	idx += BRUSH_USER;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A1);
	return A2 = EdiApp()->m_brush.m_data[idx];
}

PREDICATE_M(edi, toolBrushGet, 2) 
{
	int idx = A1;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A1);
	if(idx == BRUSH_COLOR)
	{
		int64 color = static_cast<unsigned>(EdiApp()->m_brush.m_data[idx]);
		return A2 = color;
	}
	return A2 = EdiApp()->m_brush.m_data[idx];
}

PREDICATE_M(edi, toolReset, 0)
{
	EdiApp()->m_tool[EdiApp()->m_toolcrt]->Reset(); 
	return true; 
}

PREDICATE_M(edi, toolCommand, 1)
{
	EdiApp()->m_tool[EdiApp()->m_toolcrt]->Command(A1); 
	return true; 
}


void cEdiApp::ErrorMessage( LPCWSTR msg )
{
	dlog(LOGERR, L"ERROR:\n%S\n", msg);
	sys_msgbox( E9_GetHWND(), msg, L"ERROR", MB_OK );
}

//////////////////////////////////////////////////////////////////////////////////////////////////
