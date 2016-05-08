///////////////////////////////////////////////////////////////////////////////////////////////////
// Game.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "resource.h"

#include "DizApp.h"
#include "DizDef.h"
#include "DizCfg.h"
#include "E9String.h"
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
	ini_get(file_getfullpath(GetIniFile()), L"ADVANCED", L"log") >> open;

	elog::elog.init(GetLogFile());
	elog::elog.openChannels(open);

	App::Run<DizApp>(hInstance, lpCmdLine);
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

