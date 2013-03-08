///////////////////////////////////////////////////////////////////////////////////////////////////
// D9Debug.h
// The debug system (include this file)
// Interface:
// D9_INIT, D9_DONE
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __D9DEBUG_H__
#define __D9DEBUG_H__

#include <assert.h>

#include "D9Log.h"
#include "D9Breakpoint.h"

#ifdef _DEBUG
#define D9_BUILDMODE	"_DEBUG  "
#else
#define D9_BUILDMODE	"_RELEASE"
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////

// must be placed at the begining of main function
#define D9_INIT( logfile, callback, openlog )										\
	D9_LogInit(logfile,callback);													\
	E9_OpenChannels( openlog );														\
	dlog( LOGDBG, L"------------------------------------------\n" );					\
	dlog( LOGDBG, L"| RENE DEBUG INIT ( %S )           |\n", D9_BUILDMODE );			\
	dlog( LOGDBG, L"------------------------------------------\n" );

// must be placed at the end of main function
#define D9_DONE()																	\
	dlog( LOGDBG, L"------------------------------------------\n" );					\
	dlog( LOGDBG, L"| RENE DEBUG DONE                        |\n" );					\
	dlog( LOGDBG, L"------------------------------------------\n" );
#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
