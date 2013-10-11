///////////////////////////////////////////////////////////////////////////////////////////////////
// D9Log.h
// Log debug messages
// Interface:
// dlog
// D9_LogInit, D9_LogPrintF, D9_LogPrintV, D9_LogSetCallback, 
// D9_LogSetChannel, D9_LogSetChannelFlag, D9_LogOpenChannel
// D9_LogStore, D9_LogGetBuffer
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __D9LOG_H__
#define __D9LOG_H__

#include "E9System.h"

#include <string>
#include <functional>

#define D9_LOG_BUFFERSIZE				1024				// size of a single log message and of the storing buffer

struct d9LogChannel
{
	bool open;
	bool file;
	bool debug;
	bool callback;
	std::string name;
	dword flags;
	dword color;
	d9LogChannel() : open(), file(), debug(), callback(), color(0xffffffff) {}
	d9LogChannel(const std::string & name, dword color, bool open = true, bool file = true, bool debug = true, bool callback = true) : name(name), color(color), open(open), file(file), debug(debug), callback(callback) {}
};

enum class Channel
{
	nul,	// default
	sys,	// system - open on release
	err,	// error - open on release
	eng,	// engine
	dbg,	// debug
	fil,	// files
	inp,	// input
	rnd,	// render
	snd,	// sound
	scr,	// script
	app,	// application
	max
};

class d9Log
{
public:
	typedef std::function<void(Channel,LPCWSTR)> logCallback;
private:
	static std::string logfile;				// log file
	static d9LogChannel	logc[Channel::max];	// channels
	static logCallback callback;					// log user callback
	static d9LogChannel & get(Channel ch) { return logc[static_cast<size_t>(ch)]; }
public:

	static void Init(const std::string & logfile);
	static void	clear();

	static void printV(Channel ch, LPCWSTR fmt, va_list args);
	static void printF(Channel ch, LPCWSTR fmt, ... ) { va_list	args; va_start(args, fmt); printV(ch, fmt, args); va_end(args); }
	static void printF(LPCWSTR fmt, ... ) { va_list	args; va_start(args, fmt); printV(Channel::nul, fmt, args); va_end(args); }
	static void printBuf(Channel ch, LPCSTR buffer, size_t size);

	static dword getColor(Channel ch) { return get(ch).color; }
	static void setCallback(logCallback c) { callback = c; }
	static void openChannels(bool open = true);
};

///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////

#define	dlog	d9Log::printF


#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
