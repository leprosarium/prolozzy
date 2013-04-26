///////////////////////////////////////////////////////////////////////////////////////////////////
// Editor.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "resource.h"

#include "SWI-cpp-m.h"

#include "E9App.h"
#include "EdiApp.h"
#include "EdiDef.h"


static cEdiApp* g_ediapp = NULL;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Init
///////////////////////////////////////////////////////////////////////////////////////////////////
int AppOnInit()
{
	assert(g_ediapp==NULL);
	g_ediapp = new cEdiApp();
	return g_ediapp->Init();
}

int AppOnDone()
{
	assert(g_ediapp!=NULL);
	g_ediapp->Done();
	delete g_ediapp;
	return 0;
}

int AppOnActivate()
{
	assert(g_ediapp!=NULL);
	g_ediapp->Activate(e9App::Active());
	return 0;
}

int AppOnClose()
{
	assert(g_ediapp!=NULL);
	g_ediapp->Close();
	return 0; // always refuese close
}

int AppOnRun()
{
	assert(g_ediapp!=NULL);
	if(!g_ediapp->Update()) return 0; // exit
	g_ediapp->Draw();
	return 1;
}

int AppOnPaint()
{
	assert(g_ediapp!=NULL);
	g_ediapp->Draw();
	return 0;
}

LRESULT AppOnMsg(UINT msg, WPARAM wparam, LPARAM lparam)
{
	if(g_ediapp==NULL) return 0;
	
	switch(msg)
	{
	case WM_DROPFILES:
	{
		WCHAR filepath[MAX_PATH];
		if(DragQueryFileW((HDROP)wparam, 0xFFFFFFFF, NULL, 0)<1) return 0; // count
		if(DragQueryFileW((HDROP)wparam, 0, filepath, MAX_PATH)==0) return 0; // error
		g_ediapp->DropFile( filepath );
		break;
	}
	case WM_MOUSEWHEEL: // good for laptop touch pads
	{
		int delta = (short int)HIWORD(wparam);
		if(abs(delta)>100) 
		{
			if(GetAsyncKeyState(VK_SHIFT))
				g_ediapp->Scroll(delta>0?-1:1,0);
			else
				g_ediapp->Scroll(0,delta>0?-1:1);
		}
		break;
	}
	case WM_VSCROLL: // good for laptop touch pads
	{
		int sub = LOWORD(wparam);
		if(sub==SB_LINEUP || sub==SB_PAGEUP)		g_ediapp->Scroll(0,-1);
		if(sub==SB_LINEDOWN || sub==SB_PAGEDOWN)	g_ediapp->Scroll(0, 1);
		break;
	}
	case WM_HSCROLL: // good for laptop touch pads
	{
		int sub = LOWORD(wparam);
		if(sub==SB_LINELEFT || sub==SB_PAGELEFT	)	g_ediapp->Scroll(-1,0);
		if(sub==SB_LINERIGHT || sub==SB_PAGERIGHT )	g_ediapp->Scroll( 1,0);
		break;
	}
	};
	
	return 0;
}

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{

	PL_action(PL_ACTION_GUIAPP, TRUE);
	char *av[] = { __argv[0], 0};
	PlEngine e(sizeof(av) / sizeof(*av) - 1, av);

	// init debug
	bool openlog = true;

	d9Log::Init("editor.log");													\
	E9_OpenChannels( openlog );		
	
	// init engine
	if(!E9_Init()) return 1;

	// prepare application callbacks
	e9App::OnInit = AppOnInit;
	e9App::OnDone = AppOnDone;
	e9App::OnRun = AppOnRun;
	e9App::OnActivate = AppOnActivate;
	e9App::OnClose = AppOnClose;
	e9App::OnPaint = AppOnPaint;
	e9App::OnMsg = AppOnMsg;

	// init and run application
	if(e9App::Init(hInstance, lpCmdLine))
		e9App::Run();
	e9App::Done(); // done application destroys partial init if needed

	// done engine
	E9_Done();

	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

