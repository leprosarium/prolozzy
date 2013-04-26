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

static cDizApp* g_dizapp = NULL;

///////////////////////////////////////////////////////////////////////////////////////////////////
// Init
///////////////////////////////////////////////////////////////////////////////////////////////////
int AppOnInit()
{
	assert(g_dizapp==NULL);
	g_dizapp = new cDizApp();
	return g_dizapp->Init();
}

int AppOnDone()
{
	assert(g_dizapp!=NULL);
	g_dizapp->Done();
	delete g_dizapp;
	return 0;
}

int AppOnActivate()
{
	assert(g_dizapp!=NULL);
	g_dizapp->Activate(e9App::Active());
	return 0;
}

int AppOnRun()
{
	assert(g_dizapp!=NULL);
	if(!g_dizapp->Update()) return 0; // exit
	g_dizapp->Draw();
	return 1;
}

int AppOnPaint()
{
	assert(g_dizapp!=NULL);
	g_dizapp->Draw();
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
	bool open = false;
	ini_get(file_getfullpath(GetIniFile()), "ADVANCED", "log",  open);

	d9Log::Init(GetLogFile());													\
	E9_OpenChannels(open);	

	// init engine
	if(E9_Init())
	{

		// prepare application callbacks
		e9App::OnInit = AppOnInit;
		e9App::OnDone = AppOnDone;
		e9App::OnRun = AppOnRun;
		e9App::OnActivate = AppOnActivate;
		e9App::OnPaint = AppOnPaint;

		// init and run application
		if(e9App::Init(hInstance, lpCmdLine))
			e9App::Run();
		e9App::Done(); // done application destroys partial init if needed

		// done engine
		E9_Done();
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

