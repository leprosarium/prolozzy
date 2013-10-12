///////////////////////////////////////////////////////////////////////////////////////////////////
// Editor.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "resource.h"

#include "SWI-cpp-m.h"

#include "EdiApp.h"
#include "EdiDef.h"


int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{

	PL_action(PL_ACTION_GUIAPP, TRUE);
	char *av[] = { __argv[0], 0};
	PlEngine e(sizeof(av) / sizeof(*av) - 1, av);

	elog::elog.init(L"editor.log");
	elog::elog.openChannels(true);


	App::Run<Editor>(hInstance, lpCmdLine);
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

