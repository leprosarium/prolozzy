///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileMem.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileMem.h"


bool f9FileMem::Open(const std::string & name, int mode )
{
	if(IsOpen()) Close();
	if(name.empty()) return false;
	if(name[0] != '#') return false;

	// open
	m_mode = mode;
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;
	if(2!=sscanf(name.c_str(),"#%x#%x#",&m_addr,&m_size)) return false; // bad name format
	if(!m_addr || m_size<0) return false; // bad name format
	
	m_open = true;
	return true;
}

bool f9FileMem::Close()
{
	if(!IsOpen()) return false;
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;
	m_open = false;
	return true;
}

bool f9FileMem::Seek(int64 offset, int origin)
{
	if(!IsOpen()) return false;
	if(origin==F9_SEEK_SET && offset>=0 && offset<=m_size) { m_pos = offset; return true; }
	if(origin==F9_SEEK_CUR && m_pos+offset>=0 && m_pos+offset<=m_size) { m_pos += offset; return true; }
	if(origin==F9_SEEK_END && m_size-offset>=0 && m_size-offset<=m_size) { m_pos = m_size-offset; return true; }
	return false;
}

int64 f9FileMem::Tell()
{
	if(!IsOpen()) return F9_FAIL;
	return m_pos;
}

int64 f9FileMem::Size()
{
	if(!IsOpen()) return F9_FAIL;
	return m_size;
}

bool f9FileMem::Eof()
{
	if(!IsOpen()) return true;
	return (m_pos==m_size);
}

int64 f9FileMem::Read(void * data, int64 size)
{
	if(!IsOpen() || !data) return 0;
	int64 s = size;
	if(m_pos + s > m_size) s = m_size - m_pos;
	memcpy(data, m_addr + m_pos, (sizet)s);
	m_pos += s;
	return s;
}

int64 f9FileMem::Write(void * data, int64 size)
{
	if(!IsOpen() || !data) return 0;
	// @WARNING: this may write in memory if file buffer not big enough !!!
	memcpy(m_addr + m_pos, data, (sizet)size);
	if(m_pos + size > m_size) m_size = m_pos + size;
	return size;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
