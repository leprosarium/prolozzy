///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileDisk.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileDisk.h"

f9FileDisk::f9FileDisk()
{
	m_type	= F9_FILE_DISK;
	m_file	= NULL;
}

f9FileDisk::~f9FileDisk()
{
}

int f9FileDisk::Open( const char* name, int mode )
{
	if(IsOpen()) Close();
	if(name==NULL) return F9_FAIL;

	// open
	m_mode = mode;
	char szmode[4][4] = {"rb","wb","r+b","w+b"};
	m_file = fopen(name, szmode[mode & 3]);
	if(!m_file) return F9_FAIL;
	
	m_open = TRUE;
	return F9_OK;
}

int f9FileDisk::Close()
{
	if(!IsOpen()) return F9_FAIL;
	if(m_file) if(fclose(m_file)!=0) return F9_FAIL;
	m_open = FALSE;
	return F9_OK;
}

int f9FileDisk::Seek( int64 offset, int origin )
{
	if(!IsOpen()) return F9_FAIL;
	return (fseek( m_file, (long)offset, origin )==0) ? F9_OK : F9_FAIL;
}

int64 f9FileDisk::Tell()
{
	if(!IsOpen()) return F9_FAIL;
	return ftell( m_file );
}

int64 f9FileDisk::Size()
{
	if(!IsOpen()) return F9_FAIL;
	int64 p = Tell(); 
	if(p!=F9_OK) return F9_FAIL;
	if(Seek(0,SEEK_END)!=F9_OK) return F9_FAIL;
	int64 s = Tell();
	if(Seek(p)!=F9_OK) return F9_FAIL;
	return s;
}

int f9FileDisk::Eof()
{
	if(!IsOpen()) return F9_FAIL;
	return feof( m_file );
}

int64 f9FileDisk::Read( void* data, int64 size )
{
	if(!IsOpen() || data==NULL) return 0;
	dword ret = (dword)fread(data, 1, (sizet)size, m_file);
	return ret;
}

int64 f9FileDisk::Write(void* data, int64 size )
{
	if(!IsOpen() || data==NULL) return 0;
	dword ret = (dword)fwrite(data, 1, (sizet)size, m_file);
	return ret;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
