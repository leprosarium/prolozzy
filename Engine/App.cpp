#include "stdafx.h"
#include "App.h"
#include "Input\eInput.h"

LPCWSTR App::ClassName = L"APP_CLASS";

HINSTANCE App::instance;
HWND App::wnd;

App::App(HINSTANCE hinstance, LPCTSTR cmdline) : 
	cmdLine(cmdline),
	active(),
	minimized(),
	windowed(true),
	cool(true),
	cursor(Cursor::Default),
	frame(),
	tick(),
	ticklast(),
	fps(),
	fpstick(),
	fpscount(),
	OnActivate([](bool){}),
	OnClose([](){return true;}),
	OnPaint([](){}),
	OnUpdate([](){return true;}),
	OnMsg([](UINT,WPARAM,LPARAM){})
{
	instance = hinstance;
	if(FAILED(CoInitialize(NULL))) 
		throw std::exception("Failed to initialize COM.");
	TCHAR p[MAX_PATH];
	GetModuleFileName(NULL, p, MAX_PATH);
	path = p;

	cursors[static_cast<size_t>(Cursor::None)] = NULL;
	cursors[static_cast<size_t>(Cursor::Arrow)] = LoadCursor(NULL, IDC_ARROW);
	cursors[static_cast<size_t>(Cursor::Wait)] = LoadCursor(NULL, IDC_WAIT);
	cursors[static_cast<size_t>(Cursor::Hand)] = LoadCursor(NULL, IDC_HAND);
	cursors[static_cast<size_t>(Cursor::Custom)] = NULL;
	InitWindow();
}

App::~App()
{
	CoUninitialize();
}

void App::InitWindow()
{
	WNDCLASSEXW wcex;
	ZeroMemory(&wcex, sizeof(wcex));
	wcex.cbSize			= sizeof(WNDCLASSEX);
	wcex.style			= CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
	wcex.lpfnWndProc	= (WNDPROC)_WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= instance;
	wcex.hIcon			= NULL;	// let the user set the icon later
	wcex.hCursor		= NULL; // LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)GetStockObject(BLACK_BRUSH); //(COLOR_WINDOW+1);
	wcex.lpszMenuName	= NULL;
	wcex.lpszClassName	= ClassName;
	wcex.hIconSm		= NULL;	// use small icon from default icon
	if(!RegisterClassExW(&wcex))
		throw std::exception("Failed to register main window class.");

	// create window
	int style = (WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN ) & ~(WS_MAXIMIZEBOX|WS_SIZEBOX); // WS_POPUP
	int width = 640;
	int height= 480;
	int cx = (sys_desktopwidth()-width) / 2;
	int cy = (sys_desktopheight()-height) / 2;
	RECT rec = {cx,cy,cx+width,cy+height};
	AdjustWindowRectEx( &rec, style, FALSE, 0 );
	wnd = CreateWindowExW(0, 
						ClassName, 
						L"APP", 
						style,
						rec.left, rec.top, rec.right-rec.left, rec.bottom-rec.top, 
						NULL, NULL, instance, 
						this);
	if(!wnd)
		throw std::exception("Failed to create main window.");
}

LRESULT	CALLBACK App::_WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	App * app;
	if(msg == WM_CREATE)
	{
		app = reinterpret_cast<App*>(reinterpret_cast<LPCREATESTRUCT>(lParam)->lpCreateParams);
		SetWindowLongPtrW(hwnd, GWL_USERDATA, reinterpret_cast<LONG_PTR>(app));
	}
	else
	{
		app = reinterpret_cast<App*>(GetWindowLongPtrW(hwnd, GWL_USERDATA));
		if(!app) return DefWindowProcW(hwnd, msg, wParam, lParam);
	}
	return app->WndProc(msg, wParam, lParam);
}

LRESULT	App::WndProc(UINT msg, WPARAM wParam, LPARAM lParam )
{
	OnMsg(msg, wParam, lParam);
	switch(msg)
	{
		case WM_ACTIVATEAPP:
		{
			bool act = (wParam!=0);
			bool changed = (active != act);
			active = act;
			minimized = false;
			if(changed) 
			{
				elog::app() << "Activate " << (active ? "on" : "off") << std::endl;
				OnActivate(active);
			}
			break;
		}

		case WM_ACTIVATE:
		{
			bool act = !(LOWORD(wParam) == WA_INACTIVE);
			bool changed = (active!=act);
			active = act;
			minimized = HIWORD(wParam) != 0;
			if(changed) 
			{
				elog::app() << "Activate " << (active ? "on" : "off") << std::endl;
				OnActivate(active);
			}
			break;
		}

		case WM_CLOSE:
			if(!OnClose()) return 0;
			break;
		case WM_DESTROY:
			PostQuitMessage(0); 
			break;
		case WM_PAINT:				
			if(windowed && !minimized)
				OnPaint();
			break;
		case WM_SETCURSOR:			
			SetCursor(windowed ? cursor : Cursor::None);
			return 0;
		case WM_SYSKEYUP:		// ignore ALT key press; was loosing app focus 
			return 0;				
		case WM_SYSCOMMAND:		// ignore sysmenu and others
		{
			int cmd = (int)wParam & 0xffff0;
			if(cmd!=SC_RESTORE && cmd!=SC_MINIMIZE && cmd!=SC_CLOSE && cmd!=SC_MOVE )
				return 0;
			break;
		}
		case WM_CONTEXTMENU:	// ignore sysmen when rclick
			return 0;
		case WM_CHAR:
		{
			WCHAR ch = wParam;
			einput->keyQueue += ch;
		}
	}
	return DefWindowProcW(wnd, msg, wParam, lParam);
}

void App::SetCursor(Cursor cur)
{
	::SetCursor(cursors[static_cast<size_t>(cursor = cur)]);
}

bool App::ProcessMessages()
{
	MSG	msg;
	while(PeekMessageW(&msg, NULL, 0, 0, PM_NOREMOVE))
	{
		if(GetMessageW(&msg, NULL, 0, 0))
		{
			TranslateMessage(&msg);
			DispatchMessageW(&msg);
		}
		else
			return true;
	}
	return false;
}

void App::Loop()
{
	while(true)
	{
		if(ProcessMessages())
			break;

		if(active && !minimized )
		{
			UpdateClocks();
			if(!OnUpdate()) break;
			OnPaint();
		}
		else
			Sleep(10); // do something good for the operation system
		if(cool)
			Sleep(1); // STUPID HARDWARE (cpu cool)
	}
}

void App::UpdateClocks()
{
	frame++;
	// real time
	ticklast = tick;
	tick = sys_gettickcount();
	if(ticklast == 0) ticklast = tick;
	if(tick < ticklast) ticklast = 0; // overflow

	if(fpstick == 0) fpstick = tick;
	if(tick - fpstick >= 1000)
	{
		fps = fpscount * 1000.0f / static_cast<float>(tick - fpstick);
		fpscount = 0;
		fpstick = tick;
	}
	fpscount++;
}

void App::Name(const std::string & name) 
{ 
	if(wnd) 
		SetWindowText(wnd, name.c_str()); 
}

void App::Icon(const std::string & name) 
{ 
	if(wnd)
		if(HICON hIcon = LoadIcon(instance, name.c_str()))
			PostMessageW(wnd, WM_SETICON, ICON_BIG,(LPARAM)(HICON)hIcon);
}

void App::Icon(int res) 
{ 
	if(wnd)
		if(HICON hIcon = LoadIcon(instance, MAKEINTRESOURCE(res)))
			PostMessageW(wnd, WM_SETICON, ICON_BIG,(LPARAM)(HICON)hIcon);
}

void App::ErrorMessage(LPCWSTR msg)
{
	elog::err() << "DizzyAGE ERROR:" << std::endl << msg << std::endl;
	sys_msgbox( Wnd(), msg, L"DizzyAGE ERROR", MB_OK );
}

void App::ErrorMessage(LPCSTR msg)
{
	elog::err() << "DizzyAGE ERROR:" << std::endl << msg << std::endl;
	sys_msgbox(Wnd(), msg, "DizzyAGE ERROR", MB_OK);
}