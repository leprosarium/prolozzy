///////////////////////////////////////////////////////////////////////////////////////////////////
// Game.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "resource.h"

#include "E9App.h"
#include "DizApp.h"
#include "DizDef.h"
#include "DizCfg.h"

#include "SWI-cpp-m.h"

static cDizApp* g_dizapp = nullptr;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Init
///////////////////////////////////////////////////////////////////////////////////////////////////
int AppOnInit()
{
	return (g_dizapp = new cDizApp())->Init();
}

int AppOnDone()
{
	delete g_dizapp;
	return 0;
}

int AppOnActivate()
{
	g_dizapp->Activate(App.Active());
	return 0;
}

int AppOnRun()
{
	if(!g_dizapp->Update()) return 0; // exit
	g_dizapp->Draw();
	return 1;
}

int AppOnPaint()
{
	g_dizapp->Draw();
	return 0;
}


int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPCTSTR   lpCmdLine,
                     int       nCmdShow)
{
    PL_action(PL_ACTION_GUIAPP, TRUE);
	char *av[] = { __argv[0], 0};
	PlEngine e(sizeof(av) / sizeof(*av) - 1, av);
	
	// init debug
	bool open = false;
	ini_get(file_getfullpath(GetIniFile()), "ADVANCED", "log") >> open;

	d9Log::Init(GetLogFile());
	E9_OpenChannels(open);	

	// init engine
	if(E9_Init())
	{
		App.OnInit = AppOnInit;
		App.OnDone = AppOnDone;
		App.OnActivate = AppOnActivate;
		// init and run application
		if(App.Init(hInstance, lpCmdLine)) {
			App.OnRun = AppOnRun;
			App.OnPaint = AppOnPaint;
			App.Run();
		}
		App.Done(); // done application destroys partial init if needed

		// done engine
		E9_Done();
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

