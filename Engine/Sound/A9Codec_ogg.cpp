///////////////////////////////////////////////////////////////////////////////////////////////////
// A9Codec_ogg.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "A9Codec_ogg.h"

// make sure the paths to the libs are ok
#ifdef _DEBUG
#pragma comment( lib, "..\\Libs\\Ogg\\win32\\Static_Debug\\ogg_static_d.lib" )
#pragma comment( lib, "..\\Libs\\Vorbis\\win32\\Vorbis_Static_Debug\\vorbis_static_d.lib" )
#pragma comment( lib, "..\\Libs\\Vorbis\\win32\\VorbisFile_Static_Debug\\vorbisfile_static_d.lib" )
#else
#pragma comment( lib, "..\\Libs\\Ogg\\win32\\Static_Release\\ogg_static.lib" )
#pragma comment( lib, "..\\Libs\\Vorbis\\win32\\Vorbis_Static_Release\\vorbis_static.lib" )
#pragma comment( lib, "..\\Libs\\Vorbis\\win32\\VorbisFile_Static_Release\\vorbisfile_static.lib" )
#endif

#define PCMSIZE 4096	// pcm read buffer size

// use our file system
size_t ogg_read		(void *ptr, size_t size, size_t nmemb, void *datasource) { return static_cast<size_t>(static_cast<F9FILE>(datasource)->Read(ptr, (int)(size*nmemb))); }
int    ogg_seek		(void *datasource, ogg_int64_t offset, int whence) { return static_cast<F9FILE>(datasource)->Seek((int)offset, whence); }
int    ogg_close	(void *datasource) { files->FileClose((F9FILE)datasource); return 0; }
long   ogg_tell		(void *datasource) { return static_cast<long>(static_cast<F9FILE>(datasource)->Tell()); }

///////////////////////////////////////////////////////////////////////////////////////////////////
a9Codec_ogg::a9Codec_ogg()
{
	m_file = NULL;
	m_pcmbuf = NULL;
	m_pcmpos = 0;
	m_pcmcnt = 0;
}

a9Codec_ogg::~a9Codec_ogg()
{
}

int a9Codec_ogg::Init()
{
	return A9_OK;
}

int a9Codec_ogg::Done()
{
	return A9_OK;
}

int	a9Codec_ogg::Open( const std::wstring & name )
{
	if(m_status!=A9_CODEC_CLOSED) return A9_FAIL;

	// set user callbacks
	ov_callbacks mycallbacks = { ogg_read, ogg_seek, ogg_close, ogg_tell };
	m_file = files->OpenFile(name); 
	if(!m_file) return A9_FAIL;

	if(ov_open_callbacks(m_file, &m_oggfile, NULL, 0, mycallbacks) < 0) { files->FileClose(m_file); return A9_UNSUPORTED; }

	m_pcmbuf = new byte[PCMSIZE];
	m_pcmpos = 0;
	m_pcmcnt = 0;

    char** comment		= ov_comment(&m_oggfile,-1)->user_comments;
    vorbis_info* vi		= ov_info(&m_oggfile,-1);
	m_info.m_signed		= 1;
	m_info.m_depth		= 16;
	m_info.m_channels	= vi->channels;
	m_info.m_frequency	= vi->rate;
	m_info.m_size		= (int)ov_pcm_total(&m_oggfile,-1);

	m_status = A9_CODEC_OPENED;
	return A9_OK;
}

int	a9Codec_ogg::BeginRender( int pos, int loop )
{
	if(m_status!=A9_CODEC_OPENED) return A9_FAIL;
	m_loop = loop;
	m_pcmpos = 0;
	m_pcmcnt = 0;
	int ret = ov_pcm_seek( &m_oggfile, pos ); // reset for looping
	if(ret!=0) return A9_FAIL; 

	m_status = A9_CODEC_RENDERING;
	return A9_OK;
}

int	a9Codec_ogg::Render( byte* buffer, int size )
{
	if(m_status!=A9_CODEC_RENDERING) return A9_FAIL;
	if(size<=0) return A9_FAIL;
	int pos=0; // current position in buffer (in bytes)
	int memsize = size * m_info.SampleSize(); // size of buffer (in bytes)

	// leftovers
	if(m_pcmpos>0)
	{
		memcpy(buffer, m_pcmbuf+m_pcmpos, m_pcmcnt);
		pos += m_pcmcnt;
	}

	while(1)
	{
		m_pcmpos = 0;
		int current_section;
		long ret=ov_read( &m_oggfile, (char*)m_pcmbuf, PCMSIZE, 0, m_info.m_depth>>3, m_info.m_signed, &current_section );
		if(ret==0) 
		{
			if(m_loop)
			{
				ret = ov_raw_seek( &m_oggfile, 0 ); // reset for looping
				if(ret==0) continue; 
				else ret =-1; // report error
			}
			else
			{
				// EOS
				return pos/m_info.SampleSize();
			}
		}
		if(ret<0) return A9_ERROR;

		if(pos+ret>memsize) // too much, keep leftovers
		{
			m_pcmpos = memsize-pos;
			m_pcmcnt = ret - m_pcmpos;
			ret = memsize-pos;
			memcpy(buffer+pos, m_pcmbuf, ret);
			return size;
		}
		else
		{
			memcpy(buffer+pos, m_pcmbuf, ret);
			pos += ret;
			if(pos==memsize) return size; // fix!
		}
	}
}

int	a9Codec_ogg::EndRender()
{
	if(m_status!=A9_CODEC_RENDERING) return A9_FAIL;
	m_pcmpos = 0;
	m_pcmcnt = 0;
	m_status = A9_CODEC_OPENED;
	return A9_OK;
}

int	a9Codec_ogg::Close()
{
	if(m_status!=A9_CODEC_OPENED) return A9_FAIL;
	delete [] m_pcmbuf;
	ov_clear(&m_oggfile); // closes m_file too
	m_file = NULL;
	m_status = A9_CODEC_CLOSED;
	return A9_OK;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
