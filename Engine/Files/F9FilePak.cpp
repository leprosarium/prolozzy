//////////////////////////////////////////////////////////////////////////////////////////////////
// F9FilePak.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FilePak.h"
#include "F9ArchivePak.h"

f9FilePak::f9FilePak()
{
	m_type		= F9_FILE_PAK;
	m_fileinfo	= NULL;
	m_arcname	= NULL;
}

f9FilePak::~f9FilePak()
{
}

int f9FilePak::Open( const char* name, int mode )
{
	if(IsOpen()) Close();
	if(name==NULL) return F9_FAIL;
	if(!F9_ISREADONLY(mode)) return F9_FAIL; // readonly

	// archive should set those
	if( !m_arcname || m_fileinfo==NULL ) return F9_FAIL;

	m_mode = mode;

	// arc disk file
	if(m_arcfile.Open( m_arcname, m_mode )!=F9_OK) return F9_FAIL;
	if(m_arcfile.Seek( m_fileinfo->m_offset, F9_SEEK_SET )!=F9_OK) { m_arcfile.Close(); return F9_FAIL; }

	m_open = TRUE;
	return F9_OK;
}

int f9FilePak::Close()
{
	if(!IsOpen()) return F9_FAIL;
	m_arcfile.Close();

	m_arcname	= NULL;
	m_fileinfo	= NULL;

	m_open = FALSE;
	return F9_OK;
}

int f9FilePak::Seek( int64 offset, int origin )
{
	if(!IsOpen()) return F9_FAIL;
	
	// convert to F9_SEEK_SET
	if(origin==F9_SEEK_END)	offset = m_fileinfo->m_size - offset;
	if(origin==F9_SEEK_CUR)	offset = Tell() + offset;
		
	// bounds
	if(offset<0) offset = 0;
	if(offset>m_fileinfo->m_size) offset = m_fileinfo->m_size;

	origin = F9_SEEK_SET;
	offset += m_fileinfo->m_offset;

	return m_arcfile.Seek( offset, origin );
}

int64 f9FilePak::Tell()
{
	if(!IsOpen()) return F9_FAIL;
	int64 pos = m_arcfile.Tell();
	pos -= m_fileinfo->m_offset;
	assert( pos>=0 && pos<=m_fileinfo->m_size );
	return pos;
}

int64 f9FilePak::Size()
{
	if(!IsOpen()) return F9_FAIL;
	return m_fileinfo->m_size;
}

int f9FilePak::Eof()
{
	if(!IsOpen()) return F9_FAIL;
	return (Tell()==m_fileinfo->m_size) ? 1 : 0;
}

int64 f9FilePak::Read( void* data, int64 size )
{
	if(!IsOpen() || data==NULL) return 0;
	
	// bound
	int64 pos = Tell();
	if( pos+size > m_fileinfo->m_size ) size = m_fileinfo->m_size - pos;

	return m_arcfile.Read(data, size);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
