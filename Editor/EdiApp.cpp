//////////////////////////////////////////////////////////////////////////////////////////////////
// Editor::app.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "EdiApp.h"

#include "Resource.h"
#include "Resource.h"

#include "SWI-cpp-m.h"
#include "PlBrush.h"
#include "EdiMap.h"
#include "Gui.h"
#include "GuiTile.h"

#include "eInput.h"

Editor * Editor::app = nullptr;

PREDICATE_M(core, dl, 1)
{
	LPWSTR msg = A1;
	elog::scr() << msg << std::endl;
	return true;
}

PREDICATE_M(core, ini, 4)
{
	static wchar_t tmp_fullpath[256];
	if(!GetFullPathNameW(A1, 255, tmp_fullpath, NULL )) 
		return false;

	PlTerm val = A4;	
	if(val.type() == PL_VARIABLE)
	{
		std::wstring value;
		if(!(ini_get(tmp_fullpath, A2, A3) >> value))
			return false;
		PlTerm ct;
		if (PL_wchars_to_term(value.c_str(), ct))
			return A4 = ct;
		return A4 = value;
	}
	ini_set<std::wstring>(tmp_fullpath, A2, A3, val);
	return true;
}

PREDICATE_M(core, tickCount, 1)
{
	return A1 = static_cast<int>(GetTickCount());
}

Editor::Tools::Tools()
{
	tools.push_back(new cEdiToolPaint());
	tools.push_back(new cEdiToolEdit());
	active = tools.front();
}

Editor::Tools::~Tools()
{
	for (auto t : tools) delete t;
}

void Editor::Tools::Init()
{
	active = tools.front();
	active->Switch(true);
}

void Editor::Tools::Done()
{
	active->Switch(false);
}

bool Editor::Tools::OnClose()
{
	for (auto t : tools)
		if (t->OnClose())
			return true;
	active->Reset();
	return false;
}

Editor::Layers::Layers()
{
	for (int i = 0; i<LAYER_MAX; i++) layers[i] = true;
}

Editor::Editor(HINSTANCE hinstance, LPCTSTR cmdline) : App(hinstance, cmdline)
{
	App::OnActivate = [this](bool a) { OnActivate(a); }; 
	App::OnClose = [this]() { return OnClose(); };
	App::OnPaint = [this]() { Draw(); };
	App::OnUpdate = [this]() { return Update(); };
	App::OnMsg = [this](UINT msg,WPARAM wp,LPARAM lp) { OnMsg(msg, wp, lp); };

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
	
	m_drawstats = 0;

	m_mscrollx = 0;
	m_mscrolly = 0;

	app = this;

	Init();
}

Editor::~Editor()
{
	Done();
	app = nullptr;
}

void Editor::Init()
{
	elog::app() << "App init." << std::endl;

	// engine
	if(!InitApp()) throw std::exception("Init app error.");
	if(!InitFiles()) throw std::exception("Init files error.");
	if(!InitInput()) throw std::exception("Init input device error.");
	if(!InitVideo()) throw std::exception("Init video device error.");


	// editor
	g_paint.Init();
	g_map.Init();
	BOOL ret = GUIInit();
	g_gui->ScriptPrologDo("editor:init");

	tools.Init();

	// load param
	if(CmdLine().find("pmp") != std::string::npos)
	{
		std::ostringstream s;
		s<< "editor:load('" << CmdLine() << "')";
		g_gui->ScriptPrologDo(s.str());
	}

	// accept drop files
	long styleex = GetWindowLong(Wnd(),GWL_EXSTYLE) | WS_EX_ACCEPTFILES;
	SetWindowLong(Wnd(),GWL_EXSTYLE,styleex);
}

bool Editor::InitApp()
{
	Name(EDI_NAME);
	Icon(IDI_ICON);

	bool cool = true;
	ini_get( file_getfullpath(USER_INIFILE), L"EDITOR", L"options_cool") >> cool;
	Cool(cool);
	
	return true;
}

bool Editor::InitFiles()
{
	if(!F9_Init()) return false;
	files->MakeIndex(L"editor\\");
	return true;
}

bool Editor::InitInput()
{
	return eInput::Init(Wnd(), Instance()) && eInput::Init<Keyboard>() && eInput::Init<Mouse>();
}

bool Editor::InitVideo()
{
	std::wstring inifile = file_getfullpath(USER_INIFILE);

	int screensize = 1;
	int delta = 64; // substract from screen size
	ini_get(inifile, L"EDITOR", L"options_screensize") >> screensize;
	if(screensize<0||screensize>=4) screensize=1;
	int screensizelist[4][2]={ {640,480}, {800,600}, {1024,768}, {1280,1024} };

	// custom sizes
	ini_get(inifile, L"EDITOR", L"options_screenw") >> screensizelist[3][0];
	ini_get(inifile, L"EDITOR", L"options_screenh") >> screensizelist[3][1];

	// default config
	r9Cfg cfg;
	Api api	= Api::Default;
	cfg.bpp		= 32;
	cfg.width		= screensizelist[screensize][0]-delta/2;
	cfg.height	= screensizelist[screensize][1]-delta;
	cfg.refresh	= 85;

	// load config
	int apiv;
	if(ini_get(inifile, L"EDITOR", L"options_videoapi") >> apiv)
		if(apiv == static_cast<int>(Api::DirectX))
			api = Api::DirectX;
		else if(apiv == static_cast<int>(Api::OpenGL))
			api = Api::OpenGL;


	// init interface
	if(!R9_InitInterface(api)) return false;

	BOOL ok = R9_Init(Wnd(), &cfg, api);
	if(!ok) // try the other api
	{
		elog::app() << "RENDER: init " << api << " failed, try the other api." << std::endl;
		api = api == Api::DirectX ? Api::OpenGL : Api::DirectX;
		ok = R9_Init(Wnd(), &cfg, api);
		if(!ok)	return false;
	}

	R9_SetHandleReset(HandleReset);
	R9_SetFilter(Filter::Point);
	Windowed(cfg.windowed);

	return true;
}

void Editor::Done()
{
	// must be able to destroy partial init too, in case Init has failed

	// script exit
	if(g_gui) g_gui->ScriptPrologDo("editor:done");

	tools.Done();
	
	// editor
	GUIDone();
	g_map.Done();
	g_paint.Done();

	// engine
	R9_Done();
	R9_DoneInterface();
	F9_Done();
	eInput::Done();
	elog::app() << "App done." << std::endl;
}

void Editor::OnActivate(bool active)
{
	if(active)
		eInput::Acquire();
	else
		eInput::Unacquire();
	tools()->Reset();
	if(g_gui) g_gui->ToolTip.clear();
	if(g_map.m_scrolling)
	{
		g_map.m_scrolling = 0;
		SetCursor(Cursor::Arrow);
	}
	::InvalidateRect(Wnd(),NULL,0);
}

bool Editor::OnClose()
{
	//if(g_gui->m_isbusy) return; // can't close now !
	if (tools.OnClose())
		return false;
	g_gui->ScriptPrologDo("editor:close");
	g_gui->m_isbusy = TRUE; // avoid tools problems
	return false;
}

void Editor::Tools::Set(PlAtom tool)
{
	auto i = std::find_if(tools.begin(), tools.end(), [tool](cEdiTool * e) { return e->name == tool; });
	if (i == tools.end()) i = tools.begin();
	if (active == *i) return;
	active->Switch(false);
	active = *i;
	active->Switch(true);
}


void Editor::OnMsg(UINT msg, WPARAM wparam, LPARAM lparam)
{
	switch(msg)
	{
	case WM_DROPFILES:
	{
		WCHAR filepath[MAX_PATH];
		if(DragQueryFileW((HDROP)wparam, 0xFFFFFFFF, NULL, 0)<1) break; // count
		if(DragQueryFileW((HDROP)wparam, 0, filepath, MAX_PATH)==0) break; // error
		DropFile( filepath );
		break;
	}
	case WM_MOUSEWHEEL: // good for laptop touch pads
	{
		int delta = (short int)HIWORD(wparam);
		if(abs(delta)>100) 
		{
			if(GetAsyncKeyState(VK_SHIFT))
				Scroll(delta>0?-1:1,0);
			else
				Scroll(0,delta>0?-1:1);
		}
		break;
	}
	case WM_VSCROLL: // good for laptop touch pads
	{
		int sub = LOWORD(wparam);
		if(sub==SB_LINEUP || sub==SB_PAGEUP)		Scroll(0,-1);
		if(sub==SB_LINEDOWN || sub==SB_PAGEDOWN)	Scroll(0, 1);
		break;
	}
	case WM_HSCROLL: // good for laptop touch pads
	{
		int sub = LOWORD(wparam);
		if(sub==SB_LINELEFT || sub==SB_PAGELEFT	)	Scroll(-1,0);
		if(sub==SB_LINERIGHT || sub==SB_PAGERIGHT )	Scroll( 1,0);
		break;
	}
	};
}


void Editor::DropFile( LPCWSTR filepath )
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
		elog::err() << "Exception: " << msg << std::endl;
	}


	g_map.Update(0.0f);
	g_map.Refresh();
	Draw();
}

void Editor::Scroll( int dx, int dy )
{
	m_mscrollx = dx;
	m_mscrolly = dy;
}

void Editor::HandleReset()
{
	g_map.Refresh();
	Editor::app->Draw();
}

bool Editor::Update()
{
	float dtime = (float)DeltaTime() / 1000.0f;

	// input
	eInput::Update(dtime);

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
		tools()->Update( dtime );
		tool_isbusy = tools()->IsBusy();
	}
	else
	if(!gui_wasbusy)
	{
		tools()->Reset(); // reset tool mode
		tool_isbusy = false;
	}
	gui_wasbusy = g_gui->m_isbusy;

	// statusbar
	g_gui->ScriptPrologDo("mod", "userUpdate");
	tools()->UserUpdate();

	// gui
	g_gui->ReadInput();
	if(!tool_isbusy && !map_isbusy)
		g_gui->Update();

	// toggle info
	if(einput->isKeyDown(DIK_F11)) m_drawstats = !m_drawstats;

	// print screen
	if(einput->isKeyDown(DIK_SYSRQ)) 
	{
		fRect r(0,0,R9_GetWidth(),R9_GetHeight());
		R9_SaveScreenShot(&r,!einput->ctrl());
	}

	if(m_exit) return false;
	return true;
}


void Editor::Draw()
{
	if(!R9_IsReady()) return; // avoid painting if render is not ready
	R9_CheckDevice(); // check for lost device
	if(R9_BeginScene())
	{
		R9_Clear(EDI_COLORBACK1);

		// editor
		g_map.Draw();

		// tool
		R9_SetClipping(fRect(g_map.view));
		if(g_gui && !g_gui->m_isbusy)
			tools()->Draw();
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
void Editor::DrawStats()
{
	std::wostringstream o;
	o << L"fps: " << FPS();
	std::wstring str = o.str();
	fV2 sz = fV2(ChrW * str.size(), ChrH) + 4;
	fV2 p(R9_GetWidth() - sz.x - 2, 2.0f);

	R9_DrawBar( fRect(p, p + sz), 0xa0000000 );
	R9_DrawText( p + 2, str, 0xffffff80 );
	R9_Flush();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// UTILS
//////////////////////////////////////////////////////////////////////////////////////////////////

iV2 Editor::GetMousePos() const
{
	POINT mouse;
	GetCursorPos(&mouse);
	ScreenToClient(Wnd(), &mouse);
	return iV2(mouse.x, mouse.y);
}

void Editor::WaitCursor( BOOL on )
{
	SetCursor( on ? Cursor::Wait : Cursor::Arrow );
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


Tile *TileGet(PlTerm idx)
{
	Tile * tile = g_paint.TileGet(idx); 
	if(!tile)
		throw PlDomainError("invalid tile index", idx);
	return tile;
}

PREDICATE_M(edi, tileGetID, 2) 
{
	return A2 = TileGet(A1)->id; 
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
	return A2 = TileGet(A1)->frames; 
}

PREDICATE_M(edi, tileGetName, 2) 
{
	return A2 = TileGet(A1)->name;
}

PREDICATE_M(edi, tileReload, 0)
{
	elog::app() << "Reload tiles ..." << std::endl;

	// clear old
	g_paint.TileUnload();

	// load from 2 ini dirs
	int loads = 0;
	std::wstring tilepath;
	if(ini_get(file_getfullpath(USER_INIFILE), L"editor", L"options_tiledir") >> tilepath)
		if(g_paint.TileLoad(tilepath)) loads++;
	else 
		elog::app() << "TileDir not specified in editor.ini" << std::endl;
	return loads > 0;
}


PREDICATE_M(edi, getTool, 1)
{
	return A1 = Editor::app->tools()->name;
}

PREDICATE_M(edi, getAxes, 1)
{
	return A1 = Editor::app->m_axes;
}

PREDICATE_M(edi, getSnap, 1)
{
	return A1 = Editor::app->m_snap;
}

PREDICATE_M(edi, getGrid, 1)
{
	return A1 = Editor::app->m_grid;
}

PREDICATE_M(edi, getGridSize, 1)
{
	return A1 = Editor::app->m_gridsize;
}

PREDICATE_M(edi, getScrW, 1)
{
	return A1 = Editor::app->GetScrW();
}

PREDICATE_M(edi, getScrH, 1)
{
	return A1 = Editor::app->GetScrH();
}

PREDICATE_M(edi, getAxe, 2)
{
	iV2 axe = Editor::app->GetAxe();
	return (A1 = axe.x) && (A2 = axe.y);
}

PREDICATE_M(edi, getBrushRect, 1)
{
	return A1 = g_paint.m_brushrect;
}

PREDICATE_M(edi, getColorBack1, 1)
{
	return A1 = static_cast<int64>(Editor::app->GetColor(EDI_COLORBACK1));
}

PREDICATE_M(edi, getColorBack2, 1)
{
	return A1 = static_cast<int64>(Editor::app->GetColor(EDI_COLORBACK2));
}

PREDICATE_M(edi, getColorGrid1, 1)
{
	return A1 = static_cast<int64>(Editor::app->GetColor(EDI_COLORGRID1));
}

PREDICATE_M(edi, getColorGrid2, 1)
{
	return A1 = static_cast<int64>(Editor::app->GetColor(EDI_COLORGRID2));
}

PREDICATE_M(edi, getColorGrid3, 1)
{
	return A1 = static_cast<int64>(Editor::app->GetColor(EDI_COLORGRID3));
}

PREDICATE_M(edi, getColorMap, 1)
{
	return A1 = static_cast<int64>(Editor::app->GetColor(EDI_COLORMAP));
}

PREDICATE_M(edi, setTool, 1)
{
	Editor::app->tools.Set(static_cast<PlAtom>(A1));
	return true;
}

PREDICATE_M(edi, setAxes, 1)
{
	Editor::app->m_axes = A1;
	return true;
}

PREDICATE_M(edi, setSnap, 1)
{
	Editor::app->m_snap = A1;
	return true;
}

PREDICATE_M(edi, setGrid, 1)
{
	Editor::app->m_grid = A1;
	return true;
}

PREDICATE_M(edi, setGridSize, 1)
{
	Editor::app->m_gridsize = A1;
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
	Editor::app->m_color[EDI_COLORBACK1-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, setColorBack2, 1)
{
	int64 l = A1;
	Editor::app->m_color[EDI_COLORBACK2-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, setColorGrid1, 1)
{
	int64 l = A1;
	Editor::app->m_color[EDI_COLORGRID1-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, setColorGrid2, 1)
{
	int64 l = A1;
	Editor::app->m_color[EDI_COLORGRID2-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, setColorGrid3, 1)
{
	int64 l = A1;
	Editor::app->m_color[EDI_COLORGRID3-EDI_COLOR] = static_cast<dword>(l);
	return true;
}


PREDICATE_M(edi, setColorMap, 1)
{
	int64 l = A1;
	Editor::app->m_color[EDI_COLORMAP-EDI_COLOR] = static_cast<dword>(l);
	return true;
}

PREDICATE_M(edi, exit, 0)
{
	Editor::app->m_exit=1;
	return true;
}

PREDICATE_M(edi, waitCursor, 1)
{
	Editor::app->WaitCursor(A1);
	return true;
}

// layers ..................................................................................

PREDICATE_M(edi, layerGet, 2)
{
	int layer = A1;
	if(!Editor::app->layers.IsValid(layer))
		throw PlDomainError ("invalid layer index", A1);
	return A2 = Editor::app->layers.Get(layer);
}

PREDICATE_M(edi, layerSet, 2)
{
	int layer = A1;
	if(!Editor::app->layers.IsValid(layer))
		throw PlDomainError("invalid layer index", A1);
	Editor::app->layers.Set(layer, A2);
	return true;
}

// tool brush ..............................................................................

PREDICATE_M(edi, toolBrush, 1)
{
	return PlBrush(&Editor::app->m_brush) = A1;
}

PREDICATE_M(edi, toolReset, 0)
{
	Editor::app->tools()->Reset(); 
	return true; 
}

PREDICATE_M(edi, toolCommand, 1)
{
	Editor::app->tools()->Command(A1); 
	return true; 
}



//////////////////////////////////////////////////////////////////////////////////////////////////
