#ifndef __APP__
#define __APP__
#include "stdafx.h"

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

class App
{
	static LPCWSTR ClassName;
	HINSTANCE instance;
	HWND wnd;
	std::string cmdLine, path;
	bool active;
	bool minimized;
	bool windowed;
	bool cool;
	HCURSOR cursors[Cursor::Max];	
	Cursor cursor;
	int frame;
	float fps;
	dword tick;
	dword ticklast;
	dword fpstick;
	int fpscount;

	void UpdateClocks();
	void InitWindow();
	static LRESULT CALLBACK _WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
	LRESULT WndProc(UINT msg, WPARAM wParam, LPARAM lParam);

public:
	App(HINSTANCE hinstance, LPCTSTR cmdline);
	virtual ~App();

	virtual void OnActivate(bool) {}
	virtual bool OnClose() { return true; }
	virtual bool OnPaint() {}
	virtual bool OnRun() { return true; }

	void Run();	
	void SetCursor(Cursor cursor);

};

#endif