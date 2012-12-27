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
	guard(AppOnInit);
	sassert(g_dizapp==NULL);
	g_dizapp = snew cDizApp();
	return g_dizapp->Init();
	unguard();
}

int AppOnDone()
{
	guard(AppOnDone);
	sassert(g_dizapp!=NULL);
	g_dizapp->Done();
	sdelete(g_dizapp);
	return 0;
	unguard();
}

int AppOnActivate()
{
	guard(AppOnActivate);
	sassert(g_dizapp!=NULL);
	g_dizapp->Activate( E9_AppGetInt(E9_APP_ACTIVE) );
	return 0;
	unguard();
}

int AppOnRun()
{
	guard(AppOnRun);
	sassert(g_dizapp!=NULL);
	if(!g_dizapp->Update()) return 0; // exit
	g_dizapp->Draw();
	return 1;
	unguard();
}

int AppOnPaint()
{
	guard(AppOnPaint);
	sassert(g_dizapp!=NULL);
	g_dizapp->Draw();
	return 0;
	unguard();
}


int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
    PL_action(PL_ACTION_GUIAPP, TRUE);
	char *av[] = { __argv[0], "-x", "boot32.prc", 0};
	PlEngine e(sizeof(av) / sizeof(*av) - 1, av);
	
	// init debug
	BOOL openlog = false;
	ini_getint( file_getfullpath(GetIniFile()), "ADVANCED", "log",  &openlog );
	D9_INIT(GetLogFile(),NULL,openlog);


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
	// done debug
	D9_DONE();
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

