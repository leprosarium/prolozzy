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
	g_ediapp->Activate( E9_AppGetInt(E9_APP_ACTIVE) );
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

int AppOnMsg()
{
	if(g_ediapp==NULL) return 0;
	UINT msg		= (UINT)(intptr)E9_AppGetVoid(E9_APP_PARAM+0);
	WPARAM wparam	= (WPARAM)E9_AppGetVoid(E9_APP_PARAM+1);
	LPARAM lparam	= (LPARAM)E9_AppGetVoid(E9_APP_PARAM+2);
	
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
	BOOL openlog = TRUE;
	D9_INIT("editor.log",nullptr,openlog);
	
	// init engine
	if(!E9_Init()) goto done;

	// prepare application callbacks
	E9_AppSetCallback( E9_APP_ONINIT,		AppOnInit );
	E9_AppSetCallback( E9_APP_ONDONE,		AppOnDone );
	E9_AppSetCallback( E9_APP_ONRUN,		AppOnRun );
	E9_AppSetCallback( E9_APP_ONACTIVATE,	AppOnActivate );
	E9_AppSetCallback( E9_APP_ONCLOSE,		AppOnClose );
	E9_AppSetCallback( E9_APP_ONPAINT,		AppOnPaint );
	E9_AppSetCallback( E9_APP_ONMSG,		AppOnMsg );

	// init and run application
	if(E9_AppInit(hInstance, lpCmdLine))
		E9_AppRun();
	E9_AppDone(); // done application destroys partial init if needed

	// done engine
	E9_Done();

	done:
	// done debug
	D9_DONE();
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

