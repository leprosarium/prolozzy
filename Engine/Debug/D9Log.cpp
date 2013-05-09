///////////////////////////////////////////////////////////////////////////////////////////////////
// D9Log.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "D9Log.h"

std::string d9Log::m_logfile;
d9LogChannel d9Log::m_logc[ChannelMax];
bool d9Log::m_store = false;
std::wstring d9Log::m_buffer;
d9Log::callback d9Log::m_callback;

void d9Log::Init(const std::string & logfile, callback callback)
{ 
	if(!logfile.empty())
	{
		m_logfile = logfile;
		clear();
	}
	m_callback = callback;
}

void d9Log::clear()
{
	file_delete(m_logfile);
}

void d9Log::setChannel(size_t ch, const std::string & name, dword flags, dword color)
{ 
	if(ch >= ChannelMax) return;
	m_logc[ch].m_name = name;
	m_logc[ch].m_flags = flags;
	m_logc[ch].m_color = color;
}

void d9Log::store(bool enable)
{
	if( enable && !m_store ) 
		m_buffer.clear(); // reset
	m_store = enable;
}

void d9Log::printBuf(size_t ch, LPCSTR buffer, size_t size)
{
	if(ch>=ChannelMax) return;
	int flags = m_logc[ch].m_flags;
	if(!(flags & D9_LOGFLAG_OPEN)) return;

	// send to file
	if( (flags & D9_LOGFLAG_FILE) && m_logfile.size() )
		if(FILE * f = fopen(m_logfile.c_str(), "at"))
		{
			fwrite(buffer, 1, size, f);
			fclose(f);
		}

	std::wstring msg(buffer, buffer + size);
	LPCWSTR cmsg = msg.c_str();

	// send to debug
	if( flags & D9_LOGFLAG_DEBUG )
		sys_outputdebugstring(cmsg);

	// send to callback
	if( (flags & D9_LOGFLAG_CALLBACK) && m_callback )
		m_callback(ch, cmsg);

			// store
	if(m_store)	
		m_buffer.append(msg);
}

void d9Log::printV(size_t ch, LPCWSTR fmt, va_list args)
{
	if(ch >= ChannelMax) return;
	int flags = m_logc[ch].m_flags;
	if(!(flags & D9_LOGFLAG_OPEN)) return;

	static WCHAR msg[D9_LOG_BUFFERSIZE];
	if( flags & (D9_LOGFLAG_DEBUG|D9_LOGFLAG_CALLBACK) )
	{
		_vsnwprintf(msg, D9_LOG_BUFFERSIZE, fmt, args);
		msg[D9_LOG_BUFFERSIZE-1]=0;
	}
	
	// send to debug
	if( flags & D9_LOGFLAG_DEBUG )
		sys_outputdebugstring(msg);

	// send to file
	if( (flags & D9_LOGFLAG_FILE) && m_logfile.size() )
		if(FILE * f = fopen(m_logfile.c_str(), "at"))
		{
			vfwprintf( f, fmt, args );
			fclose(f);
		}

	// send to callback
	if( (flags & D9_LOGFLAG_CALLBACK) && m_callback )
		m_callback(ch, msg);

	// store
	if(m_store)
		m_buffer.append(msg);
}



///////////////////////////////////////////////////////////////////////////////////////////////////
