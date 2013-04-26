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

	e9App();

	void Name(const char * name);
	void Icon(const char * name);
	void Windowed(bool w) { m_windowed = w; }
	void Cool(bool c) { m_cool = c; }
	void SetCursor(Cursor cursor);


	const char * Name() const;
	const char * CmdLine() const { return m_cmdline; }
	const char * Path() const { return m_path; }
	bool Active() const { return m_active; }
	bool Windowed() const { return m_windowed; }
	bool Cool() const { return m_cool; }
	int Frame() const { return m_frame; }
	int DeltaTime() const { return m_tick - m_ticklast; }
	int FPS() const { return static_cast<int>(m_fps); }

	bool Init(HINSTANCE	hinstance, const char* cmdline);	// init the application
	void Done();												// done the application
	void Run();												// main loop

	void UpdateClocks();
protected:

	int InitWindow();
	static LRESULT CALLBACK _WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
	LRESULT CALLBACK WndProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam );

private:
	HWND m_hwnd;						// main window handle
	HINSTANCE m_hinstance;				// instance handle

	HCURSOR m_hcursor[Cursor::Max];		// mouse cursors
	bool m_minimized;					// minimized state
	char m_path[MAX_PATH];				// path to exe
	const char* m_cmdline;				// pointer to command line string
	bool m_windowed;						// if windowed or fullscreen
	bool m_cool;							// cool cpu
	bool m_active;						// active state

	Cursor m_cursor;						// current cursor index
	int m_frame;							// current frame
	float m_fps;							// fps value
	dword m_tick;						// real tick
	dword m_ticklast;					// old real tick

public:
	Callback OnInit;			// called in Init, after window was created but is still hidden; user return 0 if failure or 1 if success
	Callback OnDone;			// called in Done
	Callback OnRun;			// called in main loop when the application is active; return 0 to exit or 1 to continue
	Callback OnActivate;		// called when application gets activated or deactivated; check the active state
	Callback OnClose;		// called when application receive close message return 0 to refuse closing
	Callback OnPaint;		// called when application need painting and is windowed, deactivated and not minimized
	std::function<LRESULT (UINT, WPARAM, LPARAM)> OnMsg;		// called on every window message
};

extern e9App App;

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
