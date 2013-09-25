#ifndef __E9ENGINE_H__
#define __E9ENGINE_H__

#include "E9System.h"
#include "E9Config.h"

#ifdef _DEBUG
#pragma comment( lib, "..\\Engine\\engine_d.lib" )
#else
#pragma comment( lib, "..\\Engine\\engine.lib" )
#endif


// Engine log channels
#define LOGNUL				0			// default
#define LOGSYS				1			// system - open on release
#define LOGERR				2			// error - open on release
#define LOGENG				3			// engine
#define LOGDBG				4			// debug
#define LOGFIL				5			// files
#define LOGINP				6			// input
#define LOGRND				7			// render
#define LOGSND				8			// sound
#define LOGGS				9			// script
#define LOGAPP				10			// application
//		...

void E9_OpenChannels(bool open = true);		// open debug channels; called by debug init

#endif

