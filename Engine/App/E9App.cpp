///////////////////////////////////////////////////////////////////////////////////////////////////
// E9App.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9App.h"
#include "D9Log.h"

const char*	e9App::m_cmdline					= NULL;
char	    e9App::m_path[MAX_PATH]				= "";
HINSTANCE	e9App::m_hinstance					= NULL;
HWND		e9App::m_hwnd						= NULL;
bool		e9App::m_active						= false;
bool		e9App::m_minimized					= false;
bool		e9App::m_windowed					= true;
bool		e9App::m_cool						= true;
Cursor		e9App::m_cursor						= Cursor::Default;
HCURSOR		e9App::m_hcursor[Cursor::Max]		= { NULL, NULL, NULL, NULL, NULL };

int			e9App::m_frame						= 0;
dword		e9App::m_tick						= 0;
dword		e9App::m_ticklast					= 0;
float		e9App::m_fps						= 0;

e9App::Callback e9App::OnInit;
e9App::Callback e9App::OnDone;
e9App::Callback e9App::OnRun;
e9App::Callback e9App::OnActivate;
e9App::Callback e9App::OnClose;
e9App::Callback e9App::OnPaint;
std::function<LRESULT (UINT, WPARAM, LPARAM)> e9App::OnMsg;


bool e9App::Init( HINSTANCE hinstance, const char* cmdline )
{
	// init
	m_hinstance	= hinstance;
	m_cmdline = cmdline;
	GetModuleFileName( NULL, m_path, MAX_PATH );
	E9_SetHINSTANCE(hinstance);

	// cursors
	m_hcursor[static_cast<size_t>(Cursor::None)]	= NULL;
	m_hcursor[static_cast<size_t>(Cursor::Arrow)]	= LoadCursor(NULL, IDC_ARROW);
	m_hcursor[static_cast<size_t>(Cursor::Wait)]	= LoadCursor(NULL, IDC_WAIT);
	m_hcursor[static_cast<size_t>(Cursor::Hand)]	= LoadCursor(NULL, IDC_HAND);
	m_hcursor[static_cast<size_t>(Cursor::Custom)]	= NULL;

	if(!InitWindow()) return false;
	E9_SetHWND(m_hwnd);

	// user callback
	BOOL ok=TRUE;
	if(OnInit) ok = OnInit();
	if(!ok) return false; //@TODO: needs DestroyWindow(m_hwnd) ???

	// show window
	SetForegroundWindow( m_hwnd );
	ShowWindow( m_hwnd, TRUE );
	UpdateWindow( m_hwnd );
	SetFocus( m_hwnd );

	return true;
}

void e9App::Done()
{
	if(OnDone) OnDone();
}

void e9App::Run()
{
	dlog(LOGAPP, L"Main loop start.\n\n" );

	MSG	msg;
	BOOL finished = FALSE;

	while(TRUE)
	{
		while( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
		{
			if( GetMessage( &msg, NULL, 0, 0) )
			{
				TranslateMessage( &msg );
				DispatchMessage( &msg );
			}
			else
			{
				finished = TRUE; break;
			}
		}

		if( finished ) break;

		if( m_active && !m_minimized )
		{
			UpdateClocks();
			BOOL ok = TRUE;
			if(OnRun) ok = OnRun();
			if(!ok) break;
		}
		else
		{
			Sleep(10); // do something good for the operation system
		}

		if(m_cool)
			Sleep(1); // STUPID HARDWARE (cpu cool)
	}

	dlog(LOGAPP, L"\nMain loop finished.\n");
}

const char * e9App::Name()
{
	if(!m_hwnd) return nullptr;
	static char name[64];
	GetWindowText(m_hwnd, name, 64);
	return name;
}

void e9App::Name(const char * name) 
{ 
	if(m_hwnd) 
		SetWindowText(m_hwnd, name); 
}

void e9App::Icon(const char * name) 
{ 
	if(m_hwnd)
		if(HICON hIcon = LoadIcon(m_hinstance, name))
			PostMessage(m_hwnd,WM_SETICON,ICON_BIG,(LPARAM)(HICON)hIcon);
}

void e9App::SetCursor( Cursor cursor )
{
	if(cursor < Cursor::None || cursor >= Cursor::Max) return;
	::SetCursor(m_hcursor[static_cast<size_t>(cursor)]);
	m_cursor = cursor;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
int e9App::InitWindow()
{
	BOOL ok;
	
	// register window
	WNDCLASSEX wcex;
	ZeroMemory(&wcex, sizeof(wcex));
	wcex.cbSize			= sizeof(WNDCLASSEX);
	wcex.style			= CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
	wcex.lpfnWndProc	= (WNDPROC)WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= m_hinstance;
	wcex.hIcon			= NULL;	// let the user set the icon later
	wcex.hCursor		= NULL; // LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)GetStockObject(BLACK_BRUSH); //(COLOR_WINDOW+1);
	wcex.lpszMenuName	= NULL;
	wcex.lpszClassName	= E9_APP_CLASSNAME;
	wcex.hIconSm		= NULL;	// use small icon from default icon
	ok = RegisterClassEx(&wcex);
	if(!ok) { dlog(LOGERR, L"APP: failed to register main window class.\n"); return FALSE; }

	// create window
	int style = (WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN ) & ~(WS_MAXIMIZEBOX|WS_SIZEBOX); // WS_POPUP
	int width = 640;
	int height= 480;
	int cx = (sys_desktopwidth()-width) / 2;
	int cy = (sys_desktopheight()-height) / 2;
	RECT rec = {cx,cy,cx+width,cy+height};
	AdjustWindowRectEx( &rec, style, FALSE, 0 );
	m_hwnd = CreateWindowEx(	0, 
								E9_APP_CLASSNAME, 
								"E9APP", 
								style,
								rec.left, rec.top, rec.right-rec.left, rec.bottom-rec.top, 
								NULL, NULL, m_hinstance, 
								NULL );
	if(m_hwnd==NULL) { dlog(LOGERR, L"APP: failed to create main window.\n"); return FALSE; }

	return TRUE;
}

void e9App::UpdateClocks()
{
	m_frame++;

	// real time
	m_ticklast = m_tick;
	m_tick = sys_gettickcount();
	if( m_ticklast==0 ) m_ticklast=m_tick;
	if( m_tick<m_ticklast ) m_ticklast=0; // overflow

	// fps
	static int fpstick = 0;
	static int fpscount = 0;
	if( fpstick==0 ) fpstick=m_tick;
	if( m_tick - fpstick >= 1000 )
	{
		m_fps = (float)fpscount*1000.0f / (float)(m_tick-fpstick);
		fpscount = 0;
		fpstick = m_tick;
	}
	fpscount++;
			
}

LRESULT	CALLBACK e9App::WndProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
	if(OnMsg) 
	{
		OnMsg(msg, wParam, lParam);
//		m_param[3] = NULL;
//		if(Call<Callback::OnMsg>()) return (LRESULT)m_param[3]; // return result fi user processed it
	}

	int cmd;
	switch(msg)
	{
		case WM_ACTIVATEAPP:
		{
			bool active = (wParam!=0);
			bool changed = (m_active!=active);
			m_active = active;
			m_minimized = false;
			if(changed) dlog(LOGAPP, L"APP: activate %S\n",m_active?"on":"off");
			if(changed && OnActivate) OnActivate();
			break;
		}

		case WM_ACTIVATE:
		{
			bool active = !(LOWORD(wParam)==WA_INACTIVE);
			bool changed = (m_active!=active);
			m_active = active;
			m_minimized = HIWORD(wParam)!=0;
			if(changed) dlog(LOGAPP, L"APP: activate %S\n",m_active?"on":"off");
			if(changed && OnActivate) OnActivate();
			break;
		}

		case WM_CLOSE:
			if(OnClose) 
			{
				int ret = OnClose();
				if(!ret) return 0;
			}
			break;

		case WM_DESTROY:
			PostQuitMessage(0); 
			break;

		case WM_PAINT:				
			if(m_windowed && !m_minimized)
				if(OnPaint) OnPaint();
			break;

		case WM_SETCURSOR:			
			if(!m_windowed)	{ SetCursor(Cursor::None); return 0; } // @TODO d3ddevice ShowCursor(FALSE);
			::SetCursor(m_hcursor[static_cast<size_t>(m_cursor)]);
			break;

		case WM_SYSKEYUP:		// ignore ALT key press; was loosing app focus 
			return 0;				

		case WM_SYSCOMMAND:		// ignore sysmenu and others
			cmd = (int)wParam & 0xffff0;
			if(cmd!=SC_RESTORE && cmd!=SC_MINIMIZE && cmd!=SC_CLOSE && cmd!=SC_MOVE )
				return 0;
			break;

		case WM_CONTEXTMENU:	// ignore sysmen when rclick
			return 0;
	}
	return DefWindowProc( hwnd, msg, wParam, lParam );
}

///////////////////////////////////////////////////////////////////////////////////////////////////
