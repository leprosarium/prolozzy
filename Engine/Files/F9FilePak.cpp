//////////////////////////////////////////////////////////////////////////////////////////////////
// F9FilePak.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FilePak.h"
#include "F9ArchivePak.h"

bool f9FilePak::Open(const std::string & name, int mode)
{
	if(IsOpen()) Close();
	if(!F9_ISREADONLY(mode)) return false; // readonly

	// archive should set those
	if( !m_fileinfo) return false;

	m_mode = mode;

	// arc disk file
	if(!m_arcfile.Open( m_arcname, m_mode )) return false;
	if(!m_arcfile.Seek( m_fileinfo->m_offset, F9_SEEK_SET )) { m_arcfile.Close(); return false; }

	m_open = true;
	return true;
}

bool f9FilePak::Close()
{
	if(!IsOpen()) return false;
	m_arcfile.Close();

	m_arcname.clear();
	m_fileinfo	= nullptr;

	m_open = false;
	return true;
}

bool f9FilePak::Seek(int64 offset, int origin)
{
	if(!IsOpen()) return false;
	
	// convert to F9_SEEK_SET
	if(origin==F9_SEEK_END)	offset = m_fileinfo->m_size - offset;
	if(origin==F9_SEEK_CUR)	offset = Tell() + offset;
		
	// bounds
	if(offset < 0) offset = 0;
	if(offset > m_fileinfo->m_size) offset = m_fileinfo->m_size;

	origin = F9_SEEK_SET;
	offset += m_fileinfo->m_offset;

	return m_arcfile.Seek(offset, origin);
}

int64 f9FilePak::Tell()
{
	if(!IsOpen()) return F9_FAIL;
	int64 pos = m_arcfile.Tell();
	pos -= m_fileinfo->m_offset;
	assert(pos >= 0 && pos <= m_fileinfo->m_size );
	return pos;
}

int64 f9FilePak::Size()
{
	if(!IsOpen()) return F9_FAIL;
	return m_fileinfo->m_size;
}

bool f9FilePak::Eof()
{
	if(!IsOpen()) return true;
	return Tell() == m_fileinfo->m_size;
}

int64 f9FilePak::Read(void * data, int64 size)
{
	if(!IsOpen() || !data) return 0;
	
	// bound
	int64 pos = Tell();
	if( pos + size > m_fileinfo->m_size ) size = m_fileinfo->m_size - pos;

	return m_arcfile.Read(data, size);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
