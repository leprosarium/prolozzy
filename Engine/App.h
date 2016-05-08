#ifndef __APP__
#define __APP__
#include "stdafx.h"
#include "D9Log.h"
#include "E9System.h"

class App
{
public:
	enum Cursor { None, Arrow, Wait, Hand, Custom, Max, Default = Arrow };
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
	bool ProcessMessages();

public:
	std::function<void(bool)> OnActivate;
	std::function<bool()> OnClose;
	std::function<void()> OnPaint;
	std::function<bool()> OnUpdate;
	std::function<void(UINT,WPARAM,LPARAM)> OnMsg;

	template<class T>
	static void Run(HINSTANCE hinstance, LPCTSTR cmdline);

	App(HINSTANCE hinstance, LPCTSTR cmdline);
	~App();

	void Loop();	

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
	static void ErrorMessage(LPCWSTR msg);	// error message box
	static void ErrorMessage(LPCSTR msg);	// error message box
};



template<class Application>
void App::Run(HINSTANCE hinstance, LPCTSTR cl)
{
	try
	{
		Application app(hinstance, cl);
		app.Loop();
	}
	catch(const std::exception & e)
	{
		ErrorMessage(e.what());
	}
}



#endif