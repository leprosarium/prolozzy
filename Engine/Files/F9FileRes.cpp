///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileRes.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileRes.h"

bool f9FileRes::Open(const std::string & name, int mode)
{
	if(IsOpen()) Close();
	if(!F9_ISREADONLY(mode)) return false; // readonly

	// open
	m_mode = mode;
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;

	// resource
	HRSRC hrsrc = FindResourceEx(NULL, F9_FILERES_GROUP, name.c_str(),  MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL) ); 
	if(!hrsrc) return false;
	m_size = SizeofResource(NULL, hrsrc); if(m_size<=0) return false;
	HGLOBAL hglobal = LoadResource(NULL,hrsrc); if(!hglobal) return false;
	m_addr = (byte*)LockResource(hglobal); if(!m_addr) return false;

	m_open = true;
	return true;
}

bool f9FileRes::Close()
{
	if(!IsOpen()) return false;
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;
	m_open = false;
	return true;
}

bool f9FileRes::Seek( int64 offset, int origin )
{
	if(!IsOpen()) return false;
	if(origin==F9_SEEK_SET && offset>=0 && offset<=m_size) { m_pos = offset; return true; }
	if(origin==F9_SEEK_CUR && m_pos+offset>=0 && m_pos+offset<=m_size) { m_pos += offset; return true; }
	if(origin==F9_SEEK_END && m_size-offset>=0 && m_size-offset<=m_size) { m_pos = m_size-offset; return true; }
	return false;
}

int64 f9FileRes::Tell()
{
	if(!IsOpen()) return F9_FAIL;
	return m_pos;
}

int64 f9FileRes::Size()
{
	if(!IsOpen()) return F9_FAIL;
	return m_size;
}

bool f9FileRes::Eof()
{
	if(!IsOpen()) return true;
	return m_pos == m_size;
}

int64 f9FileRes::Read( void* data, int64 size )
{
	if(!IsOpen() || !data) return 0;
	int64 s = size;
	if(m_pos + s > m_size) s = m_size - m_pos;
	memcpy(data, m_addr + m_pos, (sizet)s);
	m_pos += s;
	return s;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
