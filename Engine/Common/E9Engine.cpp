
#include "stdafx.h"
#include "E9Engine.h"
#include "D9Log.h"

void E9_OpenChannels(bool open)
{
	dword logflags = D9_LOGFLAG_DEFAULT;
	if(!open) logflags &= ~D9_LOGFLAG_OPEN;

	d9Log::setChannel( LOGNUL, "NUL", logflags,				DWORD_GREY);
	d9Log::setChannel( LOGSYS, "SYS", D9_LOGFLAG_DEFAULT,	DWORD_RED);
	d9Log::setChannel( LOGERR, "ERR", D9_LOGFLAG_DEFAULT,	DWORD_RED);
	d9Log::setChannel( LOGENG, "ENG", logflags, 			DWORD_BLUE);
	d9Log::setChannel( LOGDBG, "DBG", logflags, 			DWORD_ORANGE);
	d9Log::setChannel( LOGFIL, "FIL", logflags, 			DWORD_DGREEN);
	d9Log::setChannel( LOGINP, "INP", logflags, 			DWORD_GREEN);
	d9Log::setChannel( LOGRND, "RND", logflags, 			DWORD_LRED);
	d9Log::setChannel( LOGSND, "SND", logflags, 			DWORD_LRED);
	d9Log::setChannel( LOGGS , "GS ", logflags, 			DWORD_LBLUE);
	d9Log::setChannel( LOGAPP, "APP", logflags, 			DWORD_GREEN);
}

