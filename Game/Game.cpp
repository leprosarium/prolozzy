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
	g_dizapp->Activate( E9_AppGetInt(E9_APP_ACTIVE) );
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
	int openlog = 0;
	ini_getint( file_getfullpath(GetIniFile()), "ADVANCED", "log",  &openlog );

	d9Log::Init(GetLogFile());													\
	E9_OpenChannels( openlog != 0);	

	// init engine
	if(E9_Init())
	{

		// prepare application callbacks
		E9_AppSetCallback( E9_APP_ONINIT,		AppOnInit );
		E9_AppSetCallback( E9_APP_ONDONE,		AppOnDone );
		E9_AppSetCallback( E9_APP_ONRUN,		AppOnRun );
		E9_AppSetCallback( E9_APP_ONACTIVATE,	AppOnActivate );
		E9_AppSetCallback( E9_APP_ONPAINT,		AppOnPaint );

		// init and run application
		if(E9_AppInit(hInstance, lpCmdLine))
			E9_AppRun();
		E9_AppDone(); // done application destroys partial init if needed

		// done engine
		E9_Done();
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

