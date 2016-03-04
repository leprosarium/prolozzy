#ifndef __D9LOG_H__
#define __D9LOG_H__
#include "stdafx.h"
#include "E9System.h"

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
		channel channels[static_cast<size_t>(Channel::max)];
		bool _open;
	public:
		log() : _open() {}
		~log() { file.close(); }
		bool init(const std::wstring & name) { file.open(name, std::ios::out | std::ios::binary); file.pubsetbuf(wBuffer, sizeof(wBuffer)/sizeof(wchar_t)); return file.is_open(); }
		channel & get(Channel ch) { return channels[static_cast<size_t>(ch)]; }
		channel & operator()(Channel  ch) { return get(ch); }
		void openChannels(bool opn = true);
		bool open() const { return _open; }
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

#endif

