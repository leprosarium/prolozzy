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
		std::string value;
		if(!(ini_get(tmp_fullpath, A2, A3) >> value))
			return false;
		PlTerm ct;
		if (PL_chars_to_term(value.c_str(), ct))
			return A4 = ct;
		return A4 = value;
	}
	ini_set<std::string>(tmp_fullpath, A2, A3, val);
	return true;
}

PREDICATE_M(core, tickCount, 1)
{
	return A1 = static_cast<int>(GetTickCount());
}


cEdiApp::cEdiApp()
{

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
	m_tool[TOOL_PAINT]	= new cEdiToolPaint();
	m_tool[TOOL_EDIT]	= new cEdiToolEdit();

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
}

cEdiApp::~cEdiApp()
{
	m_app = NULL;
	delete m_tool[TOOL_PAINT];
	delete m_tool[TOOL_EDIT];
}

BOOL cEdiApp::Init()
{
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
	if(App.CmdLine() && strstr(App.CmdLine(), "pmp"))
	{
		std::ostringstream s;
		s<< "editor:load('" << App.CmdLine() << "')";
		g_gui->ScriptPrologDo(s.str());
	}

	// accept drop files
	long styleex = GetWindowLong(E9_GetHWND(),GWL_EXSTYLE) | WS_EX_ACCEPTFILES;
	SetWindowLong(E9_GetHWND(),GWL_EXSTYLE,styleex);

	return TRUE;
}

BOOL cEdiApp::InitApp()
{
	App.Name(EDI_NAME);
	App.Icon(MAKEINTRESOURCE(IDI_ICON));

	bool cool = true;
	ini_get( file_getfullpath(USER_INIFILE), "EDITOR", "options_cool") >> cool;
	App.Cool(cool);
	
	return TRUE;
}

BOOL cEdiApp::InitFiles()
{
	if(!F9_Init()) return FALSE;
	files->MakeIndex("editor\\");
	return TRUE;
}

BOOL cEdiApp::InitInput()
{
	BOOL ok = I9_Init(E9_GetHWND(),E9_GetHINSTANCE(),I9_API_DEFAULT);
	if(!ok) return FALSE;

	// init devices
	ok &= I9_DeviceInit(I9_DEVICE_KEYBOARD);
	ok &= I9_DeviceInit(I9_DEVICE_MOUSE);
	if(!ok) return FALSE;
	
	return TRUE;
}

BOOL cEdiApp::InitVideo()
{
	std::string inifile = file_getfullpath(USER_INIFILE);

	int screensize = 1;
	int delta = 64; // substract from screen size
	ini_get(inifile, "EDITOR", "options_screensize") >> screensize;
	if(screensize<0||screensize>=4) screensize=1;
	int screensizelist[4][2]={ {640,480}, {800,600}, {1024,768}, {1280,1024} };

	// custom sizes
	ini_get(inifile, "EDITOR", "options_screenw") >> screensizelist[3][0];
	ini_get(inifile, "EDITOR", "options_screenh") >> screensizelist[3][1];

	// default config
	r9Cfg cfg;
	Api api	= Api::Default;
	cfg.bpp		= 32;
	cfg.width		= screensizelist[screensize][0]-delta/2;
	cfg.height	= screensizelist[screensize][1]-delta;
	cfg.refresh	= 85;

	// load config
	int apiv;
	if(ini_get(inifile, "EDITOR", "options_videoapi") >> apiv)
		if(apiv == static_cast<int>(Api::DirectX))
			api = Api::DirectX;
		else if(apiv == static_cast<int>(Api::OpenGL))
			api = Api::OpenGL;


	// init interface
	if(!R9_InitInterface(api)) return FALSE;

	BOOL ok = R9_Init(E9_GetHWND(),&cfg,api);
	if(!ok) // try the other api
	{
		dlog(L"RENDER: init %S failed, try the other api.\n", api == Api::OpenGL ? "OpenGL" : "DirectX9");
		api = api == Api::DirectX ? Api::OpenGL : Api::DirectX;
		ok = R9_Init(E9_GetHWND(),&cfg,api);
		if(!ok)	return FALSE;
	}

	R9_SetHandleReset(HandleReset);
	R9_SetFilter(Filter::Point);
	App.Windowed(cfg.windowed);

	return TRUE;
}

void cEdiApp::Done()
{
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
	F9_Done();

	dlog(LOGAPP, L"App done.\n");
}

void cEdiApp::Activate(bool active)
{
	if(active)
	{
		if(I9_IsReady()) I9_Acquire();
	}
	else
	{
		if(I9_IsReady()) I9_Unacquire();
	}
	m_tool[m_toolcrt]->Reset();
	if(g_gui) g_gui->ToolTip.clear();
	if(g_map.m_scrolling)
	{
		g_map.m_scrolling = 0;
		App.SetCursor(Cursor::Arrow);
	}
	::InvalidateRect(E9_GetHWND(),NULL,0);
}

void cEdiApp::Close()
{
	//if(g_gui->m_isbusy) return; // can't close now !
	int mode = m_tool[m_toolcrt]->m_mode;
	if(m_toolcrt==0 && mode==1) return;
	if(m_toolcrt==1 && (mode==1||mode==2)) return;
	m_tool[m_toolcrt]->Reset();
	g_gui->ScriptPrologDo("editor:close");
	g_gui->m_isbusy = TRUE; // avoid tools problems
}

void cEdiApp::DropFile( LPCWSTR filepath )
{
	if(g_gui->m_isbusy) { BEEP_ERROR(); return ; } // gui busy (modal dialog)
	if(!wcsstr(filepath, L".pmp")) { BEEP_ERROR(); return ; } // not map
	
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
}

void cEdiApp::Scroll( int dx, int dy )
{
	m_mscrollx = dx;
	m_mscrolly = dy;
}

void cEdiApp::HandleReset()
{
	g_map.m_refresh = TRUE; // refresh map
	g_map.Refresh();
	EdiApp()->Draw();
}

bool cEdiApp::Update()
{
	float dtime = (float)App.DeltaTime() / 1000.0f;

	// input
	if(!I9_IsReady()) return true;
	I9_Update(dtime);

	// map
	if(!g_gui->m_isbusy)
		g_map.Update(dtime);
	BOOL map_isbusy = g_map.m_scrolling;
	
	g_gui->ToolTip.clear();

	static bool gui_wasbusy=false;
	bool tool_isbusy = false;

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
		tool_isbusy = false;
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
		bool ctrl = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
		fRect r(0,0,R9_GetWidth(),R9_GetHeight());
		R9_SaveScreenShot(&r,!ctrl);
	}

	if(m_exit) return false;
	return true;
}


void cEdiApp::Draw()
{
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

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Draw debug
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiApp::DrawStats()
{
	std::ostringstream o;
	o << "fps: " << App.FPS();
	std::string str = o.str();
	fV2 sz = fV2(ChrW * str.size(), ChrH) + 4;
	fV2 p(R9_GetWidth() - sz.x - 2, 2.0f);

	R9_DrawBar( fRect(p, p + sz), 0xa0000000 );
	R9_DrawText( p + 2, str, 0xffffff80 );
	R9_Flush();
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// TOOLS
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiApp::ToolSet( int tool )
{
	if(tool<0 || tool>=TOOL_MAX) tool=0;
	if(tool==m_toolcrt) return;
	m_tool[m_toolcrt]->Switch(FALSE);
	m_toolcrt = tool;
	m_tool[m_toolcrt]->Switch(TRUE);
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// UTILS
//////////////////////////////////////////////////////////////////////////////////////////////////
int	cEdiApp::GetMouseX()
{
	POINT mouse;
	GetCursorPos(&mouse);
	ScreenToClient(E9_GetHWND(), &mouse);
	return mouse.x;
}

int cEdiApp::GetMouseY()
{
	POINT mouse;
	GetCursorPos(&mouse);
	ScreenToClient(E9_GetHWND(), &mouse);
	return mouse.y;
}

void cEdiApp::WaitCursor( BOOL on )
{
	App.SetCursor( on ? Cursor::Wait : Cursor::Arrow );
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// UNDO
//////////////////////////////////////////////////////////////////////////////////////////////////
BOOL cEdiApp::Undo()
{
	switch(m_undoop)
	{
		case UNDOOP_ADD:
		{
			if(!g_map.validBrushIdx(m_undoidx)) goto error;
			g_map.BrushIns(m_undoidx, m_undobrush);
			g_map.PartitionAdd(g_map.m_brush[m_undoidx]);
			m_undoop = UNDOOP_DEL;
			g_map.m_refresh = TRUE;
			BEEP_OK();
			return TRUE;
		}
		case UNDOOP_DEL:
		{
			if(!g_map.validBrushIdx(m_undoidx)) goto error;
			m_undobrush = * g_map.m_brush[m_undoidx];
			g_map.PartitionDel(g_map.m_brush[m_undoidx]);
			g_map.BrushDel(m_undoidx);
			m_undoop = UNDOOP_ADD;
			g_map.m_refresh = TRUE;
			BEEP_OK();
			return TRUE;
		}
	}
error:
	BEEP_ERROR();
	return FALSE;
}

void cEdiApp::UndoSet( int op, int idx, tBrush* brush )
{
	m_undoop = op;
	m_undoidx = idx;
	if(brush) m_undobrush=*brush;
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
	int loads = 0;
	std::string tilepath;
	if(ini_get(file_getfullpath(USER_INIFILE), "editor", "options_tiledir") >> tilepath)
		if(g_paint.TileLoad(tilepath)) loads++;
	else 
		dlog(LOGAPP, L"TileDir not specified in editor.ini\n");
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
	EdiApp()->m_color[EDI_COLORBACK1-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, setColorBack2, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORBACK2-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, setColorGrid1, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORGRID1-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, setColorGrid2, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORGRID2-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, setColorGrid3, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORGRID3-EDI_COLOR] = static_cast<dword>(l);
	return true;
}


PREDICATE_M(edi, setColorMap, 1)
{
	int64 l = A1;
	EdiApp()->m_color[EDI_COLORMAP-EDI_COLOR] = static_cast<dword>(l);
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


PREDICATE_M(edi, toolBrush, 1)
{
	return g_map.UnifyBrush(A1, & EdiApp()->m_brush);
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
