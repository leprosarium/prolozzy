//////////////////////////////////////////////////////////////////////////////////////////////////
// F9FilePak.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9FilePak.h"
#include "F9ArchivePak.h"

bool f9FilePak::DoOpen(const std::string & name, int mode)
{
	if(!IsReadOnlyMode(mode)) return false; // readonly

	// archive should set those
	if( !m_fileinfo) return false;

	// arc disk file
	if(!m_arcfile.Open( m_arcname, mode )) return false;
	if(!m_arcfile.Seek( m_fileinfo->m_offset, F9_SEEK_SET )) { m_arcfile.Close(); return false; }
	return true;
}

bool f9FilePak::DoClose()
{
	m_arcfile.Close();
	m_arcname.clear();
	m_fileinfo	= nullptr;
	return true;
}

bool f9FilePak::DoSeek(int64 offset, int origin)
{
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

int64 f9FilePak::DoTell() const
{
	int64 pos = m_arcfile.Tell();
	pos -= m_fileinfo->m_offset;
	assert(pos >= 0 && pos <= m_fileinfo->m_size );
	return pos;
}

int64 f9FilePak::DoSize()
{
	return m_fileinfo->m_size;
}

bool f9FilePak::DoEof() const
{
	return Tell() == m_fileinfo->m_size;
}

int64 f9FilePak::DoRead(void * data, int64 size)
{
	// bound
	int64 pos = Tell();
	if( pos + size > m_fileinfo->m_size ) size = m_fileinfo->m_size - pos;

	return m_arcfile.Read(data, size);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
