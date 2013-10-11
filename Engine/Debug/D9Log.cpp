///////////////////////////////////////////////////////////////////////////////////////////////////
// D9Log.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "D9Log.h"

std::string d9Log::logfile;
d9LogChannel d9Log::logc[Channel::max];
d9Log::logCallback d9Log::callback;

void d9Log::Init(const std::string & file)
{ 
	if(!file.empty())
	{
		logfile = file;
		clear();
	}
}

void d9Log::clear()
{
	file_delete(logfile);
}

void d9Log::printBuf(Channel ch, LPCSTR buf, size_t size)
{
	d9LogChannel & c = get(ch);
	if(!c.open) return;

	// send to file
	if( c.file && logfile.size() )
		if(FILE * f = fopen(logfile.c_str(), "at"))
		{
			fwrite(buf, 1, size, f);
			fclose(f);
		}

	std::wstring msg(buf, buf + size);
	LPCWSTR cmsg = msg.c_str();

	if( c.debug )
		sys_outputdebugstring(cmsg);

	if( c.callback && callback )
		callback(ch, cmsg);

}


void d9Log::printV(Channel ch, LPCWSTR fmt, va_list args)
{
	d9LogChannel & c = get(ch);
	if(!c.open) return;

	static WCHAR msg[D9_LOG_BUFFERSIZE];
	if(c.debug || c.callback)
	{
		_vsnwprintf(msg, D9_LOG_BUFFERSIZE, fmt, args);
		msg[D9_LOG_BUFFERSIZE-1]=0;
	}
	
	if(c.debug)
		sys_outputdebugstring(msg);

	if( c.file && logfile.size() )
		if(FILE * f = fopen(logfile.c_str(), "at"))
		{
			vfwprintf( f, fmt, args );
			fclose(f);
		}

	if( c.callback && callback )
		callback(ch, msg);
}

void d9Log::openChannels(bool open)
{
	get(Channel::nul) = d9LogChannel("NUL", DWORD_GREY, open);
	get(Channel::sys) = d9LogChannel("SYS", DWORD_RED);
	get(Channel::err) = d9LogChannel("ERR", DWORD_RED);
	get(Channel::eng) = d9LogChannel("ENG", DWORD_BLUE, open);
	get(Channel::dbg) = d9LogChannel("DBG", DWORD_ORANGE, open);
	get(Channel::fil) = d9LogChannel("FIL", DWORD_DGREEN, open);
	get(Channel::inp) = d9LogChannel("INP", DWORD_GREEN, open);
	get(Channel::rnd) = d9LogChannel("RND", DWORD_LRED, open);
	get(Channel::snd) = d9LogChannel("SND", DWORD_LRED, open);
	get(Channel::scr) = d9LogChannel("SCR", DWORD_LBLUE, open);
	get(Channel::app) = d9LogChannel("APP", DWORD_GREEN, open);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
