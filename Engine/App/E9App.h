///////////////////////////////////////////////////////////////////////////////////////////////////
// E9App.h
// Game application
// Interface:
// Callback::OnInit, Callback::OnDone, Callback::OnRun, Callback::OnActivate, Callback::OnPaint
// E9_APP_NAME, E9_APP_ICON, E9_APP_CMDLINE, E9_APP_PATH, E9_APP_ACTIVE, E9_APP_WINDOWED, E9_APP_FRAME, E9_APP_DELTATIME, E9_APP_FPS
// E9_AppSetCallbacks, E9_AppInit, E9_AppDone, E9_AppRun, E9_AppSetStr, E9_AppGetStr, E9_AppSetInt, E9_AppGetInt
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __E9APP_H__
#define __E9APP_H__

#include <functional>

#include "E9System.h"
#include "E9Engine.h"

// properties

#define E9_APP_PARAM			11	// generic callback param (void-r) 4 values (0=msg,1=wparam,2=lparam,3=return)

// cursors
enum class Cursor
{
	None,
	Arrow,
	Wait,
	Hand,
	Custom,
	Max,
	Default = Arrow
};

#define E9_APP_CLASSNAME		"E9_APPCLASS"


class e9App
{
public:

	typedef std::function<int ()> Callback;

	static const char * Name();
	static void Name(const char * name);
	static void Icon(const char * name);
	static void Windowed(bool w) { m_windowed = w; }
	static void Cool(bool c) { m_cool = c; }
	static void SetCursor(Cursor cursor);


	static const char * CmdLine() { return m_cmdline; }
	static const char * Path() { return m_path; }
	static bool Active() { return m_active; }
	static bool Windowed() { return m_windowed; }
	static bool Cool() { return m_cool; }
	static int Frame() { return m_frame; }
	static int DeltaTime() { return m_tick - m_ticklast; }
	static int FPS() { return static_cast<int>(m_fps); }

	static bool Init(HINSTANCE	hinstance, const char* cmdline);	// init the application
	static void	Done();												// done the application
	static void Run();												// main loop

	static void UpdateClocks();
protected:

	static int InitWindow();
	static LRESULT CALLBACK WndProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam );

private:
	static HWND m_hwnd;						// main window handle
	static HINSTANCE m_hinstance;				// instance handle

	static HCURSOR m_hcursor[Cursor::Max];		// mouse cursors
	static bool m_minimized;					// minimized state
	static char m_path[MAX_PATH];				// path to exe
	static const char* m_cmdline;				// pointer to command line string
	static bool m_windowed;						// if windowed or fullscreen
	static bool m_cool;							// cool cpu
	static bool m_active;						// active state

	static Cursor m_cursor;						// current cursor index
	static int m_frame;							// current frame
	static float m_fps;							// fps value
	static dword m_tick;						// real tick
	static dword m_ticklast;					// old real tick

public:
	static Callback OnInit;			// called in Init, after window was created but is still hidden; user return 0 if failure or 1 if success
	static Callback OnDone;			// called in Done
	static Callback OnRun;			// called in main loop when the application is active; return 0 to exit or 1 to continue
	static Callback OnActivate;		// called when application gets activated or deactivated; check the active state
	static Callback OnClose;		// called when application receive close message return 0 to refuse closing
	static Callback OnPaint;		// called when application need painting and is windowed, deactivated and not minimized
	static std::function<LRESULT (UINT, WPARAM, LPARAM)> OnMsg;		// called on every window message


};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
