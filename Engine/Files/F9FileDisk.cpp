///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileDisk.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileDisk.h"

bool f9FileDisk::Open( const std::string & name, int mode )
{
	if(IsOpen()) Close();

	// open
	m_mode = mode;
	char szmode[4][4] = {"rb","wb","r+b","w+b"};
	m_file = fopen(name.c_str(), szmode[mode & 3]);
	if(!m_file) return false;
	
	m_open = true;
	return true;
}

bool f9FileDisk::Close()
{
	if(!IsOpen()) return false;
	if(m_file && fclose(m_file)) return false;
	m_open = false;
	return true;
}

bool f9FileDisk::Seek(int64 offset, int origin)
{
	if(!IsOpen()) return false;
	return !fseek(m_file, (long)offset, origin);
}

int64 f9FileDisk::Tell()
{
	if(!IsOpen()) return F9_FAIL;
	return ftell(m_file);
}

int64 f9FileDisk::Size()
{
	if(!IsOpen()) return F9_FAIL;
	int64 p = Tell(); 
	if(p == F9_FAIL) return F9_FAIL;
	if(!Seek(0, SEEK_END)) return F9_FAIL;
	int64 s = Tell();
	if(!Seek(p)) return F9_FAIL;
	return s;
}

bool f9FileDisk::Eof()
{
	if(!IsOpen()) return true;
	return feof(m_file);
}

int64 f9FileDisk::Read( void* data, int64 size )
{
	if(!IsOpen() || !data) return 0;
	dword ret = (dword)fread(data, 1, (sizet)size, m_file);
	return ret;
}

int64 f9FileDisk::Write(void* data, int64 size )
{
	if(!IsOpen() || !data) return 0;
	dword ret = (dword)fwrite(data, 1, (sizet)size, m_file);
	return ret;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
