///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileMem.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FileMem.h"

f9FileMem::f9FileMem()
{
	m_type	= F9_FILE_MEM;
	m_addr	= NULL;
}

f9FileMem::~f9FileMem()
{
}

int f9FileMem::Open( const char* name, int mode )
{
	if(IsOpen()) Close();
	if(name==NULL) return F9_FAIL;
	if(name[0]!='#') return F9_FAIL;

	// open
	m_mode = mode;
	m_addr = NULL;
	m_size = 0;
	m_pos  = 0;
	if(2!=sscanf(name,"#%x#%x#",&m_addr,&m_size)) return F9_FAIL; // bad name format
	if(m_addr==NULL || m_size<0) return F9_FAIL; // bad name format
	
	m_open = TRUE;
	return F9_OK;
}

int f9FileMem::Close()
{
	if(!IsOpen()) return F9_FAIL;
	m_addr = NULL;
	m_size = 0;
	m_pos  = 0;
	m_open = FALSE;
	return F9_OK;
}

int f9FileMem::Seek( int64 offset, int origin )
{
	if(!IsOpen()) return F9_FAIL;
	if(origin==F9_SEEK_SET && offset>=0 && offset<=m_size) { m_pos = offset; return F9_OK; }
	else
	if(origin==F9_SEEK_CUR && m_pos+offset>=0 && m_pos+offset<=m_size) { m_pos += offset; return F9_OK; }
	else
	if(origin==F9_SEEK_END && m_size-offset>=0 && m_size-offset<=m_size) { m_pos = m_size-offset; return F9_OK; }
	else
	return F9_FAIL;
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

int f9FileMem::Eof()
{
	if(!IsOpen()) return F9_FAIL;
	return (m_pos==m_size) ? 1 : 0;
}

int64 f9FileMem::Read( void* data, int64 size )
{
	if(!IsOpen() || data==NULL) return 0;
	int64 s = size;
	if(m_pos+s>m_size) s=m_size-m_pos;
	memcpy(data,m_addr+m_pos,(sizet)s);
	m_pos+=s;
	return s;
}

int64 f9FileMem::Write(void* data, int64 size )
{
	if(!IsOpen() || data==NULL) return 0;
	// @WARNING: this may write in memory if file buffer not big enough !!!
	memcpy(m_addr+m_pos, data, (sizet)size);
	if(m_pos+size>m_size) m_size = m_pos+size;
	return size;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
