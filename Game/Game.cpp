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
	d9Log::openChannels(open);	
	elog::elog.init(L"test.log");
	elog::elog.openChannels(open);
	elog::app() << L"Test " << 124 << L" Прoверка " << 3.1415926 << std::endl;

	App::Run<DizApp>(hInstance, lpCmdLine);
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

