///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileRes.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileRes.h"

int f9FileRes::Open(const std::string & name, int mode)
{
	if(IsOpen()) Close();
	if(!F9_ISREADONLY(mode)) return F9_FAIL; // readonly

	// open
	m_mode = mode;
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;

	// resource
	HRSRC hrsrc = FindResourceEx(NULL, F9_FILERES_GROUP, name.c_str(),  MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL) ); 
	if(!hrsrc) return F9_FAIL;
	m_size = SizeofResource(NULL, hrsrc); if(m_size<=0) return F9_FAIL;
	HGLOBAL hglobal = LoadResource(NULL,hrsrc); if(!hglobal) return F9_FAIL;
	m_addr = (byte*)LockResource(hglobal); if(!m_addr) return F9_FAIL;

	m_open = true;
	return F9_OK;
}

int f9FileRes::Close()
{
	if(!IsOpen()) return F9_FAIL;
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;
	m_open = false;
	return F9_OK;
}

int f9FileRes::Seek( int64 offset, int origin )
{
	if(!IsOpen()) return F9_FAIL;
	if(origin==F9_SEEK_SET && offset>=0 && offset<=m_size) { m_pos = offset; return F9_OK; }
	if(origin==F9_SEEK_CUR && m_pos+offset>=0 && m_pos+offset<=m_size) { m_pos += offset; return F9_OK; }
	if(origin==F9_SEEK_END && m_size-offset>=0 && m_size-offset<=m_size) { m_pos = m_size-offset; return F9_OK; }
	return F9_FAIL;
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

int f9FileRes::Eof()
{
	if(!IsOpen()) return F9_FAIL;
	return m_pos == m_size ? 1 : 0;
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
