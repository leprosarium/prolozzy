///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileMem.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileMem.h"


bool f9FileMem::DoOpen(const std::wstring & name, int mode )
{
	if(name.empty()) return false;
	if(name[0] != L'#') return false;

	// open
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;
	if(2!= swscanf(name.c_str(),L"#%x#%x#", &m_addr, &m_size)) return false; // bad name format
	if(!m_addr || m_size<0) return false; // bad name format
	return true;
}

bool f9FileMem::DoClose()
{
	m_addr = nullptr;
	m_size = 0;
	m_pos  = 0;
	return true;
}

bool f9FileMem::DoSeek(int64 offset, int origin)
{
	if(origin==F9_SEEK_SET && offset>=0 && offset<=m_size) { m_pos = offset; return true; }
	if(origin==F9_SEEK_CUR && m_pos+offset>=0 && m_pos+offset<=m_size) { m_pos += offset; return true; }
	if(origin==F9_SEEK_END && m_size-offset>=0 && m_size-offset<=m_size) { m_pos = m_size-offset; return true; }
	return false;
}

int64 f9FileMem::DoRead(void * data, int64 size)
{
	int64 s = size;
	if(m_pos + s > m_size) s = m_size - m_pos;
	memcpy(data, m_addr + m_pos, (sizet)s);
	m_pos += s;
	return s;
}

int64 f9FileMem::DoWrite(void * data, int64 size)
{
	// @WARNING: this may write in memory if file buffer not big enough !!!
	memcpy(m_addr + m_pos, data, (sizet)size);
	if(m_pos + size > m_size) m_size = m_pos + size;
	return size;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
