///////////////////////////////////////////////////////////////////////////////////////////////////
// A9Codec.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"

#include <string>

#include "A9Codec.h"

#ifdef A9_ENABLE_WAV	
	#include "A9Codec_wav.h"
#endif
#ifdef A9_ENABLE_OGG	
	#include "A9Codec_ogg.h"
#endif
#ifdef A9_ENABLE_DUMB	
	#include "A9Codec_dumb.h"
#endif
#ifdef A9_ENABLE_YM	
	#include "A9Codec_ym.h"
#endif

a9Codec::a9Codec()
{
	m_loop				= 0;
	m_status			= A9_CODEC_CLOSED; 

	m_info.m_depth		= 0;
	m_info.m_signed		= 0;
	m_info.m_channels	= 0;
	m_info.m_frequency	= 0;
	m_info.m_size		= 0;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////
int A9_CodecFind( const std::wstring & filename )
{
	std::wstring ext = file_path2ext(filename);

	struct tFileSuport { int codec; std::wstring extensions; };
	static tFileSuport filesupport[] = 
	{
		#ifdef A9_ENABLE_WAV
		{ A9_CODEC_WAV,		L"wav" },
		#endif
		#ifdef A9_ENABLE_OGG
		{ A9_CODEC_OGG,		L"ogg" },
		#endif
		#ifdef A9_ENABLE_DUMB
		{ A9_CODEC_DUMB,	L"mod" },
		{ A9_CODEC_DUMB,	L"it" },
		{ A9_CODEC_DUMB,	L"xm" },
		{ A9_CODEC_DUMB,	L"s3m" },
		#endif
		#ifdef A9_ENABLE_YM
		{ A9_CODEC_YM,		L"ym" },
		#endif
		{ A9_CODEC_UNKNOWN, L"" }
	};

	for(int i = 0;filesupport[i].codec!=A9_CODEC_UNKNOWN;i++)
		if(ext == filesupport[i].extensions)
			return filesupport[i].codec;
	
	return A9_CODEC_UNKNOWN;
}

int A9_CodecInit( int type )
{
	switch(type)
	{
	#ifdef A9_ENABLE_WAV
		case A9_CODEC_WAV:	return a9Codec_wav::Init();
	#endif
	#ifdef A9_ENABLE_OGG
		case A9_CODEC_OGG:	return a9Codec_ogg::Init();
	#endif
	#ifdef A9_ENABLE_DUMB
		case A9_CODEC_DUMB:	return a9Codec_dumb::Init();
	#endif
	#ifdef A9_ENABLE_YM
		case A9_CODEC_YM:	return a9Codec_ym::Init();
	#endif
	}
	return A9_FAIL;
}

int A9_CodecDone( int type )
{
	switch(type)
	{
	#ifdef A9_ENABLE_WAV
		case A9_CODEC_WAV:	return a9Codec_wav::Done();
	#endif
	#ifdef A9_ENABLE_OGG
		case A9_CODEC_OGG:	return a9Codec_ogg::Done();
	#endif
	#ifdef A9_ENABLE_DUMB
		case A9_CODEC_DUMB:	return a9Codec_dumb::Done();
	#endif
	#ifdef A9_ENABLE_YM
		case A9_CODEC_YM:	return a9Codec_ym::Done();
	#endif
	}
	return A9_FAIL;
}

void A9_CodecInitAll()
{
	int codecs[4] = { A9_CODEC_WAV, A9_CODEC_OGG, A9_CODEC_DUMB, A9_CODEC_YM };
	for(int i=0;i<4;i++)
		A9_CodecInit(codecs[i]);
}

void A9_CodecDoneAll()
{
	int codecs[4] = { A9_CODEC_WAV, A9_CODEC_OGG, A9_CODEC_DUMB, A9_CODEC_YM };
	for(int i=0;i<4;i++)
		A9_CodecDone(codecs[i]);
}

A9CODEC	A9_CodecCreate( int type )
{
	switch(type)
	{
		#ifdef A9_ENABLE_WAV
		case A9_CODEC_WAV:	return new a9Codec_wav();
		#endif
		#ifdef A9_ENABLE_OGG
		case A9_CODEC_OGG:	return new a9Codec_ogg();
		#endif
		#ifdef A9_ENABLE_DUMB
		case A9_CODEC_DUMB:	return new a9Codec_dumb();
		#endif
		#ifdef A9_ENABLE_YM
		case A9_CODEC_YM:	return new a9Codec_ym();
		#endif
	}
	return NULL;
}

void A9_CodecDestroy( A9CODEC codec )
{
	assert(codec);
	delete codec;
}

int	A9_CodecDecodeToWave( A9CODEC codec, byte* buffer )
{
	if(codec->m_status!=A9_CODEC_OPENED) return A9_FAIL;
	int ret;
	int datasize = codec->m_info.DataSize();
	a9WavHeader* header = (a9WavHeader*)buffer;
	
	header->m_riff[0]	= 'R';
	header->m_riff[1]	= 'I';
	header->m_riff[2]	= 'F';
	header->m_riff[3]	= 'F';
	header->m_filesize	= (8+16)+(8+datasize);

	header->m_wave[0]	= 'W';
	header->m_wave[1]	= 'A';
	header->m_wave[2]	= 'V';
	header->m_wave[3]	= 'E';

	header->m_fmt[0]	= 'f';
	header->m_fmt[1]	= 'm';
	header->m_fmt[2]	= 't';
	header->m_fmt[3]	= ' ';
	header->m_fmtsize	= 16;

	header->m_tag			= 1;
	header->m_channels		= codec->m_info.m_channels;
	header->m_frequency		= codec->m_info.m_frequency;
	header->m_depth			= codec->m_info.m_depth;
	header->m_blockalign	= header->m_channels * header->m_depth / 8;
	header->m_bytespersec	= header->m_frequency * header->m_blockalign;

	header->m_data[0]	= 'd';
	header->m_data[1]	= 'a';
	header->m_data[2]	= 't';
	header->m_data[3]	= 'a';
	header->m_datasize	= datasize;

	byte* data = buffer + sizeof(a9WavHeader);

	ret = codec->BeginRender(0,0); if(ret!=A9_OK) return ret;
	ret = codec->Render(data,codec->m_info.m_size);
	if( ret!=codec->m_info.m_size ) { codec->EndRender(); return A9_FAIL; }
	ret = codec->EndRender();

	return A9_OK;
}

int A9_CodecDecodeToWave( const std::wstring & filename, byte* &buffer, int& size )
{
	// load and decode file
	int codectype = A9_CodecFind(filename);
	A9CODEC codec = A9_CodecCreate(codectype);
	if(!codec) return A9_FAIL;

	// open codec
	int ret = A9_CodecOpen(codec, filename);
	if(ret!=A9_OK) return ret;
	
	// alloc buffer
	size = sizeof(a9WavHeader) + codec->m_info.DataSize();
	buffer = new byte[size];
	if(!buffer) { A9_CodecClose(codec); A9_CodecDestroy(codec); return A9_FAIL; }
	
	// write header and decoded data
	ret = A9_CodecDecodeToWave(codec,buffer);
	if(ret!=A9_OK) { delete [] buffer; A9_CodecClose(codec); A9_CodecDestroy(codec); return A9_FAIL; }

	// close codec
	A9_CodecClose(codec);
	A9_CodecDestroy(codec);
	return A9_OK;		
}

///////////////////////////////////////////////////////////////////////////////////////////////////
