///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileDisk.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileDisk.h"

bool f9FileDisk::DoOpen( const std::string & name, int mode )
{
	char szmode[4][4] = {"rb","wb","r+b","w+b"};
	m_file = fopen(name.c_str(), szmode[mode & 3]);
	if(!m_file) return false;
	return true;
}

bool f9FileDisk::DoClose()
{
	if(m_file && fclose(m_file)) return false;
	return true;
}

bool f9FileDisk::DoSeek(int64 offset, int origin)
{
	return !fseek(m_file, (long)offset, origin);
}

int64 f9FileDisk::DoTell() const
{
	return ftell(m_file);
}

int64 f9FileDisk::DoSize()
{
	int64 p = Tell(); 
	if(p == F9_FAIL) return F9_FAIL;
	if(!Seek(0, SEEK_END)) return F9_FAIL;
	int64 s = Tell();
	if(!Seek(p)) return F9_FAIL;
	return s;
}

bool f9FileDisk::DoEof() const
{
	return feof(m_file) != 0;
}

int64 f9FileDisk::DoRead(void * data, int64 size)
{
	return fread(data, 1, static_cast<size_t>(size), m_file);
}

int64 f9FileDisk::DoWrite(void * data, int64 size )
{
	return fwrite(data, 1, static_cast<size_t>(size), m_file);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
