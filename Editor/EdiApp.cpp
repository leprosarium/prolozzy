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

#include "gs.h"
#include "EdiMap.h"
#include "Gui.h"
#include "GuiTile.h"

cEdiApp* cEdiApp::m_app = NULL;

PREDICATE_M(core, dl, 1)
{
	char * msg = A1;
	dlog(LOGGS, msg);
	dlog(LOGGS, "\n");
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
	dlog(LOGAPP,"App init.\n");

	// engine
	if(!InitApp())	 { ERRORMESSAGE("Init app error.");   return FALSE; }
	if(!InitFiles()) { ERRORMESSAGE("Init files error."); return FALSE; }
	if(!InitInput()) { ERRORMESSAGE("Init input device error."); return FALSE; }
	if(!InitVideo()) { ERRORMESSAGE("Init video device error."); return FALSE; }

	// editor
	g_paint.Init();
	g_map.Init();
	BOOL ret = GUIInit();
	ScriptRegister(); // register additional
	if(!g_gui->ScriptCompile("Editor\\Scripts\\editor.gs")) 
	{ 
		ERRORMESSAGE("Script compiling error."); 
		return FALSE; 
	}
	g_gui->ScriptPrologDo("editor:init");
	g_gui->ScriptDo("EDI_Init();");


	// tools
	m_tool[TOOL_PAINT]->Init();
	m_tool[TOOL_EDIT]->Init();
	m_toolcrt = TOOL_PAINT;
	m_tool[m_toolcrt]->Switch(TRUE);

	// load param
	if(E9_AppGetStr(E9_APP_CMDLINE) && strstr(E9_AppGetStr(E9_APP_CMDLINE),"map"))
	{
		int fp = gs_findfn(g_gui->m_vm, "EDI_Load");
		if(fp!=-1)
		{
			gs_pushstr(g_gui->m_vm,(char*)E9_AppGetStr(E9_APP_CMDLINE));
			gs_runfn(g_gui->m_vm, fp, 1);
			gs_pop(g_gui->m_vm);
		}
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
	dlog(LOGAPP,"using editor.pak file.\n");
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
		dlog("RENDER: init %s (api %i) failed, try the other api.\n",api?"OpenGL":"DirectX9",api);
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
	if(g_gui) g_gui->ScriptPrologDo("EDI_Done();");

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

	dlog(LOGAPP,"App done.\n");
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
}

void cEdiApp::DropFile( char* filepath )
{
	guard(cEdiApp::DropFile);
	sassert(filepath);
	if(g_gui->m_isbusy) { BEEP_ERROR; return ; } // gui busy (modal dialog)
	if(!strstr(filepath,".map")) { BEEP_ERROR; return ; } // not map

	int fp = gs_findfn(g_gui->m_vm, "EDI_Load"); if(fp==-1) return;
	gs_pushstr(g_gui->m_vm,filepath);
	gs_runfn(g_gui->m_vm, fp, 1);
	gs_pop(g_gui->m_vm);

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

PREDICATE_M(edi, waitCursor, 1)
{
	E9_AppSetCursor(static_cast<int>(A1) ? E9_CURSOR_WAIT : E9_CURSOR_ARROW );
	return true;
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
#define GS_ERROR( p1,p2,desc )	gs_error( vm, GSE_USER, p1, p2, desc );

// tiles ...................................................................................

PREDICATE_M(edi, tileFind, 2) 
{
	int idx = g_paint.TileFind(A1);
	if(idx == -1)
		return false;
	return A2 = idx; 
}


int gsTileFind( gsVM* vm ) 
{
	guard(gsTileFind)
	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,-1);
	int idx = g_paint.TileFind(gs_toint(vm,0)); 
	gs_pushint(vm,idx);
	return 1;
	unguard()
}

PREDICATE_M(edi, tileCount, 1) 
{
	return A1 = g_paint.TileCount();
}


int gsTileCount( gsVM* vm )  
{
	guard(gsTileCount)
	gs_pushint(vm,g_paint.TileCount());
	return 1;
	unguard()
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

int gsTileGet( gsVM* vm ) 
{
	guard(gsTileGet)
	if(!gs_ckparams(vm,2))		GS_RETURNINT(vm,0);
	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,0);
	if(!gs_cktype(vm,1,GS_INT)) GS_RETURNINT(vm,0);
	int idx = gs_toint(vm,0);
	cTile* tile = g_paint.TileGet(idx); 
	if(!tile) { GS_ERROR(idx,g_paint.TileCount(),"invalid tile index"); GS_RETURNINT(vm,0); }
	int ret = 0;
	switch( gs_toint(vm,1) )
	{
		case TILE_ID:		ret = tile->m_id; break;
		case TILE_W:		ret = tile->GetWidth(); break;
		case TILE_H:		ret = tile->GetHeight(); break;
		case TILE_FRAMES:	ret = tile->m_frames; break;
		case TILE_NAME:		gs_pushstr(vm,tile->m_name?tile->m_name:""); return 1;
	}
	gs_pushint(vm,ret);
	return 1;
	unguard()
}

PREDICATE_M(edi, tileReload, 0)
{
	dlog(LOGAPP,"Reload tiles ...\n");

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
	else dlog(LOGAPP,"TileDir not specified in editor.ini\n");

	return loads > 0;
}


int gsTileReload( gsVM* vm )
{
	guard(gsTileReload)
	dlog(LOGAPP,"Reload tiles ...\n");

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
	else dlog(LOGAPP,"TileDir not specified in editor.ini\n");

	gs_pushint(vm,loads>0);
	return 1;
	unguard()
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

PREDICATE_M(edi, getMapW, 1)
{
	return A1 = g_map.m_mapw;
}

PREDICATE_M(edi, getMapH, 1)
{
	return A1 = g_map.m_maph;
}

PREDICATE_M(edi, getRoomW, 1)
{
	return A1 = g_map.m_roomw;
}

PREDICATE_M(edi, getRoomH, 1)
{
	return A1 = g_map.m_roomh;
}

PREDICATE_M(edi, getRoomGrid, 1)
{
	return A1 = g_map.m_roomgrid;
}

PREDICATE_M(edi, getCamX, 1)
{
	return A1 = g_map.m_camx;
}

PREDICATE_M(edi, getCamY, 1)
{
	return A1 = g_map.m_camy;
}

PREDICATE_M(edi, getAxeX, 1)
{
	return A1 = EdiApp()->GetAxeX();
}

PREDICATE_M(edi, getAxeY, 1)
{
	return A1 = EdiApp()->GetAxeY();
}

PREDICATE_M(edi, getZoom, 1)
{
	return  A1 = g_map.m_camz;
}

PREDICATE_M(edi, getSelect, 1)
{
	return A1 = g_map.m_selectcount;
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

// edi .....................................................................................
int gsEdiGet( gsVM* vm ) 
{
	guard(gsEdiGet)
	if(!gs_ckparams(vm,1))		GS_RETURNINT(vm,0);
	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,0);
	int ret = 0;
	int var = gs_toint(vm,0);
	switch(var)
	{
		case EDI_TOOL:			ret = EdiApp()->m_toolcrt; break;
		case EDI_AXES:			ret = EdiApp()->m_axes; break;
		case EDI_SNAP:			ret = EdiApp()->m_snap; break;
		case EDI_GRID:			ret = EdiApp()->m_grid; break;
		case EDI_GRIDSIZE:		ret = EdiApp()->m_gridsize; break;
		case EDI_SCRW:			ret = EdiApp()->GetScrW(); break;
		case EDI_SCRH:			ret = EdiApp()->GetScrH(); break;
		case EDI_MAPW:			ret = g_map.m_mapw; break;
		case EDI_MAPH:			ret = g_map.m_maph; break;
		case EDI_ROOMW:			ret = g_map.m_roomw; break;
		case EDI_ROOMH:			ret = g_map.m_roomh; break;
		case EDI_ROOMGRID:		ret = g_map.m_roomgrid; break;
		case EDI_CAMX:			ret = g_map.m_camx; break;
		case EDI_CAMY:			ret = g_map.m_camy; break;
		case EDI_AXEX:			ret = EdiApp()->GetAxeX(); break;
		case EDI_AXEY:			ret = EdiApp()->GetAxeY(); break;
		case EDI_ZOOM:			ret = g_map.m_camz; break;
		case EDI_SELECT:		ret = g_map.m_selectcount; break;
		case EDI_BRUSHRECT:		ret = g_paint.m_brushrect; break;
	}

	if( var>=EDI_COLOR && var<=EDI_COLORMAX )
		ret = EdiApp()->m_color[var-EDI_COLOR];

	gs_pushint(vm,ret);
	return 1;
	unguard()
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

PREDICATE_M(edi, setRoomW, 1)
{
	g_map.m_roomw = A1;
	return true;
}

PREDICATE_M(edi, setRoomH, 1)
{
	g_map.m_roomh = A1;
	return true;
}

PREDICATE_M(edi, setRoomGrid, 1)
{
	g_map.m_roomgrid = A1;
	return true;
}

PREDICATE_M(edi, setCamX, 1)
{
	g_map.m_camx = A1;
	return true;
}

PREDICATE_M(edi, setCamY, 1)
{
	g_map.m_camy = A1;
	return true;
}

PREDICATE_M(edi, setZoom, 1)
{
	g_map.m_camz = A1;
	return true;
}

PREDICATE_M(edi, setSelect, 1)
{
	g_map.m_selectcount = A1;
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

int gsEdiSet( gsVM* vm ) 
{
	guard(gsEdiSet)
	if(!gs_ckparams(vm,2))		return 0;
	if(!gs_cktype(vm,0,GS_INT)) return 0;

	int val=0;
	char* szval=NULL;
	if(gs_type(vm,1)==GS_INT) val = gs_toint(vm,1);
	if(gs_type(vm,1)==GS_STR) szval = gs_tostr(vm,1);

	int var = gs_toint(vm,0);
	
	switch(var)
	{
		case EDI_TOOL:		EdiApp()->ToolSet(val); break;
		case EDI_AXES:		EdiApp()->m_axes = val; break;
		case EDI_SNAP:		EdiApp()->m_snap = val; break;
		case EDI_GRID:		EdiApp()->m_grid = val; break;
		case EDI_GRIDSIZE:	EdiApp()->m_gridsize = val; break;
		case EDI_ROOMW:		g_map.m_roomw = val; break;
		case EDI_ROOMH:		g_map.m_roomh = val; break;
		case EDI_ROOMGRID:	g_map.m_roomgrid = val; break;
		case EDI_CAMX:		g_map.m_camx = val; break;
		case EDI_CAMY:		g_map.m_camy = val; break;
		case EDI_ZOOM:		g_map.m_camz = val; break;
		case EDI_SELECT:	g_map.m_selectcount = val; break;
		case EDI_BRUSHRECT:	g_paint.m_brushrect = val; break;
	}

	if( var>=EDI_COLOR && var<=EDI_COLORMAX )
		EdiApp()->m_color[var-EDI_COLOR] = val;
	
	return 0;
	unguard()
}

PREDICATE_M(edi, exit, 0)
{
	EdiApp()->m_exit=1;
	return true;
}

int gsEdiExit( gsVM* vm )
{
	guard(gsEdiExit)
	EdiApp()->m_exit=1;
	return 0;
	unguard()
}

int gsWaitCursor(gsVM* vm )
{
	guard(gsWaitCursor)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	EdiApp()->WaitCursor(gs_toint(vm,0));
	return 0;
	unguard()
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
int gsToolBrushGet( gsVM* vm ) 
{
	guard(gsToolBrushGet)
	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,0);
	tBrush& brush = EdiApp()->m_brush;
	int idx = gs_toint(vm,0);
	if(idx<0 || idx>=BRUSH_MAX) { GS_ERROR(idx,BRUSH_MAX,"invalid brush variable"); GS_RETURNINT(vm,0); }
	gs_pushint(vm, brush.m_data[idx]);
	return 1;
	unguard()
}

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


int gsToolBrushSet( gsVM* vm ) 
{
	guard(gsToolBrushSet)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	if(!gs_cktype(vm,1,GS_INT)) return 0;
	tBrush& brush = EdiApp()->m_brush;
	int idx = gs_toint(vm,0);
	if(idx<0 || idx>=BRUSH_MAX) { GS_ERROR(idx,BRUSH_MAX,"invalid brush variable"); return 0; }
	brush.m_data[idx] = gs_toint(vm,1);
	return 0;
	unguard()
}

PREDICATE_M(edi, toolReset, 0)
{
	EdiApp()->m_tool[EdiApp()->m_toolcrt]->Reset(); 
	return true; 
}

int gsToolReset( gsVM* vm )
{
	guard(gsToolReset)
	EdiApp()->m_tool[EdiApp()->m_toolcrt]->Reset(); 
	return 0; 
	unguard()
}

PREDICATE_M(edi, toolCommand, 1)
{
	EdiApp()->m_tool[EdiApp()->m_toolcrt]->Command(A1); 
	return true; 
}

int gsToolCommand( gsVM* vm )
{
	guard(gsToolCommand)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	EdiApp()->m_tool[EdiApp()->m_toolcrt]->Command( gs_toint(vm,0) ); 
	return 0; 
	unguard()
}

PREDICATE_M(map, load, 1)
{
	return g_map.Load(WideStringToMultiByte(A1));
}

PREDICATE_M(map, brushCount, 1)
{
	return A1 = g_map.m_brushcount;
}

// map brush ................................................................................
int gsMapBrushCount( gsVM* vm ) 
{
	guard(gsMapBrushCount)
	gs_pushint(vm, g_map.m_brushcount);
	return 1;
	unguard()
}

#define MAP_BRUSH_PROP(Prop, PROP)\
GET_MAP_BRUSH_PROP(Prop, PROP)\
SET_MAP_BRUSH_PROP(Prop, PROP)

#define GET_MAP_BRUSH_PROP(Prop, PROP) PREDICATE_M(map, brushGet##Prop, 2)\
{\
	return A2 = g_map.GetBrush(A1).m_data[BRUSH_##PROP];\
}

#define SET_MAP_BRUSH_PROP(Prop, PROP) PREDICATE_M(map, brushSet##Prop, 2)\
{\
	g_map.GetBrush(A1).m_data[BRUSH_##PROP] = A2; \
	return true;\
}

MAP_BRUSH_PROP(Layer, LAYER)
MAP_BRUSH_PROP(X, X)
MAP_BRUSH_PROP(Y, Y)
MAP_BRUSH_PROP(W, W)
MAP_BRUSH_PROP(H, H)
MAP_BRUSH_PROP(Tile, TILE)
MAP_BRUSH_PROP(Frame, FRAME)
MAP_BRUSH_PROP(MapX1, MAP)
MAP_BRUSH_PROP(MapY1, MAP+1)
MAP_BRUSH_PROP(MapX2, MAP+2)
MAP_BRUSH_PROP(MapY2, MAP+3)
MAP_BRUSH_PROP(Flip, FLIP)
MAP_BRUSH_PROP(Shader, SHADER)
MAP_BRUSH_PROP(Scale, SCALE)
MAP_BRUSH_PROP(Select, SELECT)

MAP_BRUSH_PROP(Type, TYPE)
MAP_BRUSH_PROP(ID, ID)
MAP_BRUSH_PROP(Material, MATERIAL)
MAP_BRUSH_PROP(Draw, DRAW)
MAP_BRUSH_PROP(Disable, DISABLE)
MAP_BRUSH_PROP(Delay, DELAY)
MAP_BRUSH_PROP(Anim, ANIM)
MAP_BRUSH_PROP(Collider, COLLIDER)
MAP_BRUSH_PROP(Class, CLASS)
MAP_BRUSH_PROP(Status, STATUS)
MAP_BRUSH_PROP(Target, TARGET)
MAP_BRUSH_PROP(Death, DEATH)

PREDICATE_M(map, brushGetColor, 2) 
{
	int64 color = static_cast<unsigned>(g_map.GetBrush(A1).m_data[BRUSH_COLOR]);
	return A2 = color;
}


PREDICATE_M(map, brushGet, 3) 
{
	tBrush & brush = g_map.GetBrush(A1);
	int idx = A2;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A2);
	if(idx == BRUSH_COLOR) {
		int64 color = static_cast<unsigned>(brush.m_data[idx]);
		return A3 = color;
	}
	return A3 = brush.m_data[idx];
}


int gsMapBrushGet( gsVM* vm ) 
{
	guard(gsMapBrushGet)
	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,0);
	if(!gs_cktype(vm,1,GS_INT)) GS_RETURNINT(vm,0);
	int bi = gs_toint(vm,0);
	if(bi<0 || bi>=g_map.m_brushcount) { GS_ERROR(bi,g_map.m_brushcount,"invalid map brush index"); GS_RETURNINT(vm,0); }
	tBrush& brush = g_map.m_brush[bi];
	int idx = gs_toint(vm,1);
	if(idx<0 || idx>=BRUSH_MAX) { GS_ERROR(idx,BRUSH_MAX,"invalid brush variable"); GS_RETURNINT(vm,0); }
	gs_pushint(vm, brush.m_data[idx]);
	return 1;
	unguard()
}

PREDICATE_M(map, brushSetColor , 2) 
{
	int64 color = A2;
	g_map.GetBrush(A1).m_data[BRUSH_COLOR] = color;
	return true;
}


PREDICATE_M(map, brushSet , 3) 
{
	tBrush & brush = g_map.GetBrush(A1);
	int idx = A2;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A2);
	if(idx == BRUSH_COLOR)
	{
		int64 color = A3;
		brush.m_data[idx] = color;
	} 
	else 
	{
		brush.m_data[idx] = A3;
	}
	return true;
}


int gsMapBrushSet( gsVM* vm ) 
{
	guard(gsMapBrushSet)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	if(!gs_cktype(vm,1,GS_INT)) return 0;
	if(!gs_cktype(vm,2,GS_INT)) return 0;
	int bi = gs_toint(vm,0);
	if(bi<0 || bi>=g_map.m_brushcount) { GS_ERROR(bi,g_map.m_brushcount,"invalid map brush index"); return 0; }
	tBrush& brush = g_map.m_brush[bi];
	int idx = gs_toint(vm,1);
	if(idx<0 || idx>=BRUSH_MAX) { GS_ERROR(idx,BRUSH_MAX,"invalid brush variable"); return 0; }
	brush.m_data[idx] = gs_toint(vm,2);
	return 0;
	unguard()
}

PREDICATE_M(map, brushNew, 0)
{
	g_map.BrushNew();
	EdiApp()->UndoReset();
	return true;
}
PREDICATE_M(map, brushNew, 1)
{
	int idx = g_map.BrushNew();
	EdiApp()->UndoReset();
	return A1 = idx;
}


int gsMapBrushNew( gsVM* vm )
{
	guard(gsMapBrushNew)
	int idx = g_map.BrushNew();
	gs_pushint(vm,idx);
	EdiApp()->UndoReset();
	return 1;
	unguard()
}

PREDICATE_M(map, brushDel, 1)
{
	g_map.BrushDel(A1);
	EdiApp()->UndoReset();
	return 0;
}


int gsMapBrushDel( gsVM* vm )
{
	guard(gsMapBrushDel)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	int idx = gs_toint(vm,0);
	g_map.BrushDel(idx);
	EdiApp()->UndoReset();
	return 0;
	unguard()
}

PREDICATE_M(map, repartition, 0)
{
	return g_map.PartitionRepartition();
}


int gsMapRepartition( gsVM* vm )
{
	guard(gsMapRepartition)
	BOOL ok = g_map.PartitionRepartition();
	gs_pushint(vm,ok);
	return 1;
	unguard()
}

PREDICATE_M(map, refresh, 0) 
{
	g_map.m_refresh = TRUE;
	return true;
}

int gsMapRefresh( gsVM* vm ) 
{
	guard(gsMapRefresh)
	g_map.m_refresh = TRUE;
	return 0;
	unguard()
}

PREDICATE_M(map, reset, 0)
{
	g_map.Reset();
	EdiApp()->UndoReset();
	return true;
}

int gsMapReset( gsVM* vm )
{
	guard(gsMapReset)
	g_map.Reset();
	EdiApp()->UndoReset();
	return 0;
	unguard()
}

PREDICATE_M(map, resize, 2)
{
	int ret = g_map.Resize(A1, A2); 
	EdiApp()->UndoReset();
	return ret; 
}

int gsMapResize( gsVM* vm )
{
	guard(gsMapReset)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	if(!gs_cktype(vm,1,GS_INT)) return 0;
	int ret=g_map.Resize(gs_toint(vm,0), gs_toint(vm,1)); 
	EdiApp()->UndoReset();
	gs_pushint(vm,ret); 
	return 1;
	unguard()
}

// markers ....................................................................................
int gsMarkerCount( gsVM* vm )
{
	guard(gsMarkerCount)
	gs_pushint(vm, g_map.m_marker.Size());
	return 1;
	unguard()
}

int gsMarkerX( gsVM* vm )
{
	guard(gsMarkerX)
	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,0);
	int idx = gs_toint(vm,0);
	gs_pushint(vm, g_map.m_marker.Get(idx).x);
	return 1;
	unguard()
}

int gsMarkerY( gsVM* vm )
{
	guard(gsMarkerY)
	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,0);
	int idx = gs_toint(vm,0);
	gs_pushint(vm, g_map.m_marker.Get(idx).y);
	return 1;
	unguard()
}

int gsMarkerZ( gsVM* vm )
{
	guard(gsMarkerZ)
	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,0);
	int idx = gs_toint(vm,0);
	gs_pushint(vm, g_map.m_marker.Get(idx).z);
	return 1;
	unguard()
}

int gsMarkerAdd( gsVM* vm )
{
	guard(gsMarkerAdd)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	if(!gs_cktype(vm,1,GS_INT)) return 0;
	if(!gs_cktype(vm,2,GS_INT)) return 0;
	g_map.m_marker.Add( tMarker(gs_toint(vm,0),gs_toint(vm,1),gs_toint(vm,2)) );
	return 0;
	unguard()
}

int gsMarkerToggle( gsVM* vm )
{
	guard(gsMarkerToggle)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	if(!gs_cktype(vm,1,GS_INT)) return 0;
	g_map.MarkerToggle(gs_toint(vm,0),gs_toint(vm,1));
	return 0;
	unguard()
}

int gsMarkerGoto( gsVM* vm )
{
	guard(gsMarkerGoto)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	int dir = gs_toint(vm,0);
	g_map.MarkerGoto(dir);
	return 0;
	unguard()
}

int gsMarkerClear( gsVM* vm )
{
	guard(gsMarkerClear)
	g_map.MarkerClear();
	return 0;
	unguard()
}

PREDICATE_M(selection, goto, 1)
{
	g_map.SelectionGoto(A1);
	return true;
}

int gsSelectionGoto( gsVM* vm )
{
	guard(gsSelectionGoto)
	if(!gs_cktype(vm,0,GS_INT)) return 0;
	int dir = gs_toint(vm,0);
	g_map.SelectionGoto(dir);
	return 0;
	unguard()
}

PREDICATE_M(selection, refresh, 0)
{
	g_map.SelectionRefresh();
	return 0;
}

int gsSelectionRefresh( gsVM* vm )
{
	guard(gsSelectionRefresh)
	g_map.SelectionRefresh();
	return 0;
	unguard()
}

int gsSaveMapImage( gsVM* vm )
{
	guard(gsSaveMapImage)
	if(!gs_cktype(vm,0,GS_STR)) GS_RETURNINT(vm,0);
	BOOL ret = g_map.SaveMapImage(gs_tostr(vm,0));
	g_map.m_refresh = TRUE;
	gs_pushint(vm,ret);
	return 1;
	unguard()
}

// register ...................................................................................
#define GS_REGCONST( c )	gs_regint( vm, #c, c );

void cEdiApp::ScriptRegister()
{
	guard(cEdiApp::ScriptRegister)
	gsVM *vm = g_gui->m_vm;
//	gs_setdebug(vm,GSDBG_HIGH);

	gs_regfn( vm, "TileFind",			gsTileFind );				// int(id) > int(idx/-1)
	gs_regfn( vm, "TileCount",			gsTileCount );				// > int(count)
	gs_regfn( vm, "TileGet",			gsTileGet );				// int(idx), int(data) > int(val/0)
	gs_regfn( vm, "TileReload",			gsTileReload );				// > int(1/0)

	gs_regfn( vm, "EdiGet",				gsEdiGet );					// int(data) > int(val)
	gs_regfn( vm, "EdiSet",				gsEdiSet );					// int(data), int(val)
	gs_regfn( vm, "EdiExit",			gsEdiExit );				// 
	gs_regfn( vm, "WaitCursor",			gsWaitCursor );				// int(1/0)

	gs_regfn( vm, "ToolBrushGet",		gsToolBrushGet );			// int(data) > int(val/0)
	gs_regfn( vm, "ToolBrushSet",		gsToolBrushSet );			// int(data), int(val)
	gs_regfn( vm, "ToolReset",			gsToolReset );				//
	gs_regfn( vm, "ToolCommand",		gsToolCommand );			// int(cmd)

	gs_regfn( vm, "MapBrushCount",		gsMapBrushCount );			// > int(count)
	gs_regfn( vm, "MapBrushGet",		gsMapBrushGet );			// int(idx), int(data) > int(val/0)
	gs_regfn( vm, "MapBrushSet",		gsMapBrushSet );			// int(idx), int(data), int(val)
	gs_regfn( vm, "MapBrushNew",		gsMapBrushNew );			// > int(idx)
	gs_regfn( vm, "MapBrushDel",		gsMapBrushDel );			// int(idx)
	gs_regfn( vm, "MapRepartition",		gsMapRepartition );			// > int(1/0)
	gs_regfn( vm, "MapRefresh",			gsMapRefresh );				//
	gs_regfn( vm, "MapReset",			gsMapReset );				//
	gs_regfn( vm, "MapResize",			gsMapResize );				// int(w), int(h) > int(0=crop,1=no crop)

	gs_regfn( vm, "MarkerCount",		gsMarkerCount );			// > int(count)
	gs_regfn( vm, "MarkerX",			gsMarkerX );				// int(idx) > int(x)
	gs_regfn( vm, "MarkerY",			gsMarkerY );				// int(idx) > int(y)
	gs_regfn( vm, "MarkerZ",			gsMarkerZ );				// int(idx) > int(z)
	gs_regfn( vm, "MarkerAdd",			gsMarkerAdd );				// int(x), int(y), int(z)
	gs_regfn( vm, "MarkerToggle",		gsMarkerToggle );			// int(x), int(y)
	gs_regfn( vm, "MarkerGoto",			gsMarkerGoto );				// int(direction -1/1)
	gs_regfn( vm, "MarkerClear",		gsMarkerClear );			//
	gs_regfn( vm, "SelectionGoto",		gsSelectionGoto );			// int(direction -1/1)
	gs_regfn( vm, "SelectionRefresh",	gsSelectionRefresh );		//

	gs_regfn( vm, "SaveMapImage",		gsSaveMapImage );			// str(filename) > int(1/0)

	// gui constant item variables
	GS_REGCONST( IV_GUITILE_SCALE		);
	GS_REGCONST( IV_GUITILE_SHRINK		);
	GS_REGCONST( IV_GUITILEMAP_SCALE	);
	GS_REGCONST( IV_GUITILEMAP_SNAP		);
	GS_REGCONST( IV_GUITILEMAP_GRID		);
	GS_REGCONST( IV_GUITILEMAP_AXES		);
	GS_REGCONST( IV_GUITILEMAP_MAP		);

	// edi data
	GS_REGCONST( EDI_TOOL				);
	GS_REGCONST( EDI_AXES				);
	GS_REGCONST( EDI_SNAP				);
	GS_REGCONST( EDI_GRID				);
	GS_REGCONST( EDI_GRIDSIZE			);
	GS_REGCONST( EDI_SCRW				);
	GS_REGCONST( EDI_SCRH				);
	GS_REGCONST( EDI_MAPW				);
	GS_REGCONST( EDI_MAPH				);
	GS_REGCONST( EDI_ROOMW				);
	GS_REGCONST( EDI_ROOMH				);
	GS_REGCONST( EDI_ROOMGRID			);
	GS_REGCONST( EDI_CAMX				);
	GS_REGCONST( EDI_CAMY				);
	GS_REGCONST( EDI_AXEX				);
	GS_REGCONST( EDI_AXEY				);
	GS_REGCONST( EDI_ZOOM				);
	GS_REGCONST( EDI_SELECT				);
	GS_REGCONST( EDI_BRUSHRECT			);
	
	GS_REGCONST( EDI_COLOR				);
	GS_REGCONST( EDI_COLORBACK1			);
	GS_REGCONST( EDI_COLORBACK2			);
	GS_REGCONST( EDI_COLORGRID1			);
	GS_REGCONST( EDI_COLORGRID2			);
	GS_REGCONST( EDI_COLORGRID3			);
	GS_REGCONST( EDI_COLORMAP			);
	GS_REGCONST( EDI_COLORMAX			);

	// tile data
	GS_REGCONST( TILE_ID		);
	GS_REGCONST( TILE_W			);
	GS_REGCONST( TILE_H			);
	GS_REGCONST( TILE_FRAMES	);
	GS_REGCONST( TILE_NAME		);

	// brush data
	GS_REGCONST( BRUSH_LAYER	);
	GS_REGCONST( BRUSH_X		);
	GS_REGCONST( BRUSH_Y		);
	GS_REGCONST( BRUSH_W		);
	GS_REGCONST( BRUSH_H		);
	GS_REGCONST( BRUSH_TILE		);
	GS_REGCONST( BRUSH_FRAME	);
	GS_REGCONST( BRUSH_MAP		);
	GS_REGCONST( BRUSH_FLIP		);
	GS_REGCONST( BRUSH_COLOR	);
	GS_REGCONST( BRUSH_SHADER	);
	GS_REGCONST( BRUSH_SCALE	);
	GS_REGCONST( BRUSH_SELECT	);
	GS_REGCONST( BRUSH_CUSTOM	);
	GS_REGCONST( BRUSH_MAX		);

	// editor
	GS_REGCONST( LAYER_MAX			);
	GS_REGCONST( TOOL_MAX			);
	GS_REGCONST( TOOLCMD_PICKBRUSH	);
	GS_REGCONST( TOOLCMD_PICKCOLOR	);
	GS_REGCONST( TOOLCMD_TOFRONT	);
	GS_REGCONST( TOOLCMD_TOBACK		);
	GS_REGCONST( TOOLCMD_DELETE		);

	unguard();
}

void cEdiApp::ErrorMessage( char* msg )
{
	dlog(LOGERR, "ERROR:\n%s\n", msg);
	sys_msgbox( E9_GetHWND(), msg, "ERROR", MB_OK );
}

//////////////////////////////////////////////////////////////////////////////////////////////////
