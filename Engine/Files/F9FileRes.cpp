///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileRes.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileRes.h"

bool f9FileRes::DoOpen(const std::string & name, int mode)
{
	if(!IsReadOnlyMode(mode)) return false; // readonly

	// open
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;

	// resource
	HRSRC hrsrc = FindResourceEx(NULL, F9_FILERES_GROUP, name.c_str(),  MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL) ); 
	if(!hrsrc) return false;
	m_size = SizeofResource(NULL, hrsrc); if(m_size<=0) return false;
	HGLOBAL hglobal = LoadResource(NULL,hrsrc); if(!hglobal) return false;
	m_addr = (byte*)LockResource(hglobal); if(!m_addr) return false;
	return true;
}

bool f9FileRes::DoClose()
{
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;
	return true;
}

bool f9FileRes::DoSeek( int64 offset, int origin )
{
	if(origin==F9_SEEK_SET && offset>=0 && offset<=m_size) { m_pos = offset; return true; }
	if(origin==F9_SEEK_CUR && m_pos+offset>=0 && m_pos+offset<=m_size) { m_pos += offset; return true; }
	if(origin==F9_SEEK_END && m_size-offset>=0 && m_size-offset<=m_size) { m_pos = m_size-offset; return true; }
	return false;
}

int64 f9FileRes::DoRead( void* data, int64 size )
{
	int64 s = size;
	if(m_pos + s > m_size) s = m_size - m_pos;
	memcpy(data, m_addr + m_pos, (sizet)s);
	m_pos += s;
	return s;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
