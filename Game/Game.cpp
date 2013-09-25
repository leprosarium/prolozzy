///////////////////////////////////////////////////////////////////////////////////////////////////
// Game.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "resource.h"

#include "DizApp.h"
#include "DizDef.h"
#include "DizCfg.h"

#include "SWI-cpp-m.h"


int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR   lpCmdLine,
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

	try
	{
		DizApp app(hInstance, lpCmdLine);
		app.Run();
	}
	catch(const std::exception & e)
	{
		App::ErrorMessage(e.what());
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

