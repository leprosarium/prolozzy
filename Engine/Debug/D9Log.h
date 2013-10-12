#ifndef __D9LOG_H__
#define __D9LOG_H__
#include "stdafx.h"
#include "E9System.h"

#define D9_LOG_BUFFERSIZE				1024				// size of a single log message and of the storing buffer

namespace elog
{
	class outbuf : public std::wstreambuf
	{
	protected:
		static const int bufferSize = 100;
		wchar_t buffer[bufferSize];
	public:
		outbuf() { setp(buffer, buffer + (bufferSize - 2)); }

	protected:
	    int flushBuffer()
		{
			int num = pptr() - pbase();
			if(flushBuffer(num) != num)
				return traits_type::eof();
			pbump(-num);
			return num;
		}
		virtual int flushBuffer(int num) = 0;

	    virtual int_type overflow(int_type c)
		{
			if(c != traits_type::eof())
			{
				*pptr() = c;
				pbump(1);
			}
			if(flushBuffer() == traits_type::eof())
				return traits_type::eof();
	        return c;
		}

		virtual int sync()
		{
			if (flushBuffer() == traits_type::eof())
	            return -1;
			return 0;
		}
	};

	class sysbuf : public outbuf
	{
	public:
		virtual ~sysbuf() { sync(); }
		virtual int flushBuffer(int num)
		{
			buffer[num] = 0;
			OutputDebugStringW(buffer);
			return num;
		}
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
	
	class channel : public std::wostream
	{
		struct composebuf : public std::wstreambuf
		{
			virtual int_type overflow(int_type c)
			{
				for(auto buf:bufs) buf->sputc(c);
				return c;
			}
			virtual int sync()
			{
				for(auto buf:bufs) buf->pubsync();
				return 0;
			}
            std::vector<std::wstreambuf *> bufs;
		} * buf;
    public: 
        channel() : std::wostream(buf = new composebuf) {}   
		~channel() { delete buf; }
		channel & add(std::wstreambuf * b) { buf->bufs.push_back(b); return *this; }
	private:
		channel(const channel &);
		channel & operator=(const channel &);
	};

	class log
	{
		wchar_t wBuffer[128];
		std::wfilebuf file;
		sysbuf sys;
		channel channels[Channel::max];
	public:
		~log() { file.close(); }
		bool init(const std::wstring & name) { file.open(name, std::ios::out | std::ios::binary); file.pubsetbuf(wBuffer, sizeof(wBuffer)/sizeof(wchar_t)); return file.is_open(); }
		channel & get(Channel  ch) { return channels[static_cast<size_t>(ch)]; }
		channel & operator()(Channel  ch) { return get(ch); }
		void openChannels(bool open = true);
	};
	extern log elog;

	inline channel & nul() { return elog.get(Channel::nul); }
	inline channel & sys() { return elog.get(Channel::sys); }
	inline channel & err() { return elog.get(Channel::err); }
	inline channel & eng() { return elog.get(Channel::eng); }
	inline channel & dbg() { return elog.get(Channel::dbg); }
	inline channel & fil() { return elog.get(Channel::fil); }
	inline channel & inp() { return elog.get(Channel::inp); }
	inline channel & rnd() { return elog.get(Channel::rnd); }
	inline channel & snd() { return elog.get(Channel::snd); }
	inline channel & scr() { return elog.get(Channel::scr); }
	inline channel & app() { return elog.get(Channel::app); }
}

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
