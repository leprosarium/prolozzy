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
#include "E9Engine.h"

#include <string>
#include <functional>
///////////////////////////////////////////////////////////////////////////////////////////////////
// Defines
///////////////////////////////////////////////////////////////////////////////////////////////////

#define D9_LOG_BUFFERSIZE				1024				// size of a single log message and of the storing buffer

// flags
#define D9_LOGFLAG_OPEN					(1<<0)				// channel is opened
#define D9_LOGFLAG_FILE					(1<<1)				// send to file
#define D9_LOGFLAG_DEBUG				(1<<2)				// send to debug window
#define D9_LOGFLAG_CALLBACK				(1<<3)				// send to callback
#define D9_LOGFLAG_DEFAULT				(D9_LOGFLAG_OPEN|D9_LOGFLAG_FILE|D9_LOGFLAG_DEBUG|D9_LOGFLAG_CALLBACK)

struct d9LogChannel
{
	d9LogChannel() : m_flags(), m_color(0xffffffff) {}

	std::string m_name;	// channel name
	dword	m_flags;	// status flags (0=closed)
	dword	m_color;	// color option
};

class d9Log
{
public:
	static const int ChannelMax = 64;					// max number of log channels

	typedef std::function<void(int,LPCWSTR)> callback;
private:
	static std::string m_logfile;				// log file
	static d9LogChannel	m_logc[ChannelMax];		// channels
	static bool m_store;						// if log is stored or not
	static std::wstring	m_buffer;				// stored log
	static callback m_callback;					// log user callback
public:

	static void Init(const char* logfile, callback c = nullptr);
	static void	clear();
	static void store(bool enable);

	static void printV(size_t ch, LPCWSTR fmt, va_list args);
	static void printF(size_t ch, LPCWSTR fmt, ... ) { va_list	args; va_start(args, fmt); printV(ch, fmt, args); va_end(args); }
	static void printF(LPCWSTR fmt, ... ) { va_list	args; va_start(args, fmt); printV(0, fmt, args); va_end(args); }
	static void printBuf(size_t ch, LPCSTR buffer, size_t size);


	static void setChannel(size_t ch, const char* name, dword flags, dword color=0xffffffff);
	static void	setFlag(size_t ch, dword flag, bool on) { if(on) m_logc[ch].m_flags |= flag; else m_logc[ch].m_flags &= ~flag; }
	static dword getColor(size_t ch) { return m_logc[ch].m_color; }
	static void setCallback(callback c) { m_callback = c; }

	static void	openChannel(int ch, bool open) { setFlag(ch, D9_LOGFLAG_OPEN, open); }

};

///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////

#define	dlog	d9Log::printF


#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
