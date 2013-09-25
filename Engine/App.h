#ifndef __APP__
#define __APP__
#include "stdafx.h"
#include "E9System.h"

class App
{
public:
	enum class Cursor { None, Arrow, Wait, Hand, Custom, Max, Default = Arrow };
private:
	static LPCWSTR ClassName;
	static HINSTANCE instance;
	static HWND wnd;
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
	virtual void OnPaint() {}
	virtual bool OnRun() { return true; }
	virtual void OnMsg(UINT msg, WPARAM wparam, LPARAM lparam) {}

	void Run();	
	void SetCursor(Cursor cursor);

	void Name(const std::string & name);
	void Icon(const std::string & name);
	void Icon(int);
	void Cool(bool c) { cool = c; }
	void Windowed(bool w) { windowed = w; }

	int DeltaTime() const { return tick - ticklast; }
	int FPS() const { return static_cast<int>(fps); }

	bool Active() const { return active; }
	bool Windowed() const { return windowed; }
	bool Cool() const { return cool; }
	int Frame() const { return frame; }
	std::string CmdLine() const { return cmdLine; }

	static HINSTANCE Instance() { return instance; }
	static HWND Wnd() { return wnd; }
	static void ErrorMessage(LPCSTR msg);	// error message box
};

#endif