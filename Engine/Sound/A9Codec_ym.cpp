///////////////////////////////////////////////////////////////////////////////////////////////////
// A9Codec_ym.h
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "A9Codec_ym.h"

// make sure the paths to the libs are ok
#ifdef _DEBUG
#pragma comment( lib, "..\\Libs\\StSound\\Debug\\StSoundLibrary.lib" )
#else
#pragma comment( lib, "..\\Libs\\StSound\\Release\\StSoundLibrary.lib" )
#endif

// use our file system
void* ymfopen	( char* filename )							{ return files->OpenFile(filename); }
int	  ymfclose	( void* file )								{ files->FileClose((F9FILE)file); return 0; }
int	  ymfseek	( void* file, int pos, int mode )			{ return static_cast<F9FILE>(file)->Seek(pos, mode); }
int	  ymftell	( void* file )								{ return static_cast<int>(static_cast<F9FILE>(file)->Tell()); }
int	  ymfread	( void* buffer, int size, void* file )		{ return static_cast<int>(static_cast<F9FILE>(file)->Read(buffer,size)); }

///////////////////////////////////////////////////////////////////////////////////////////////////
a9Codec_ym::a9Codec_ym()
{
	m_type	= A9_CODEC_YM;
	m_ym	= NULL;
}

a9Codec_ym::~a9Codec_ym()
{
}

int a9Codec_ym::Init()
{
	ymFileSystem fs;
	fs.m_fopen	= ymfopen;
	fs.m_fclose	= ymfclose;
	fs.m_fseek	= ymfseek;
	fs.m_ftell	= ymftell;
	fs.m_fread	= ymfread;
	ymSetFileSystem( &fs );
	return A9_OK;
}

int a9Codec_ym::Done()
{
	// ymStSoundDone(); // no need
	return A9_OK;
}

int	a9Codec_ym::Open( const std::string & name )
{
	if(m_status!=A9_CODEC_CLOSED) return A9_FAIL;
	m_ym = ymMusicCreate(); if(!m_ym) return A9_FAIL;
	if(!ymMusicLoad(m_ym, name.c_str())) return A9_FAIL;
	
	ymMusicInfo_t yminfo;
	ymMusicGetInfo(m_ym,&yminfo);

	m_info.m_depth		= 16;
	m_info.m_signed		= 1;
	m_info.m_channels	= 1;
	m_info.m_frequency	= 44100;
	m_info.m_size		= (yminfo.musicTimeInMs/100) * m_info.m_frequency / 10;

	m_status = A9_CODEC_OPENED;
	return A9_OK;
}

int	a9Codec_ym::BeginRender( int pos, int loop )
{
	if(m_status!=A9_CODEC_OPENED) return A9_FAIL;
	m_loop = loop;

	ymMusicSetLoopMode(m_ym,loop);
	ymMusicSeek(m_ym, (pos * 1000) / m_info.m_frequency );
	ymMusicPlay(m_ym);

	m_status = A9_CODEC_RENDERING;
	return A9_OK;
}

int	a9Codec_ym::Render( byte* buffer, int size )
{
	if(m_status!=A9_CODEC_RENDERING) return A9_FAIL;
	if(size<=0) return A9_FAIL;
	int ret = ymMusicCompute(m_ym,(ymsample*)buffer,size);
	return size;
}

int	a9Codec_ym::EndRender()
{
	if(m_status!=A9_CODEC_RENDERING) return A9_FAIL;
	ymMusicStop(m_ym);
	m_status = A9_CODEC_OPENED;
	return A9_OK;
}

int	a9Codec_ym::Close()
{
	if(m_status!=A9_CODEC_OPENED) return A9_FAIL;
	ymMusicDestroy(m_ym);
	m_status = A9_CODEC_CLOSED;
	return A9_OK;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
