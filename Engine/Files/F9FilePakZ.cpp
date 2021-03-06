///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FilePakZ.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9Math.h"
#include "F9FilePakZ.h"
#include "F9ArchivePak.h"

bool f9FilePakZ::DoOpen(const std::wstring & name, int mode)
{
	if(!IsReadOnlyMode(mode)) return false; // readonly

	// archive should set those
	if( !m_fileinfo) return false;

	m_data	= nullptr;
	m_size	= 0;
	m_pos	= 0;

	// read whole file data and uncompress it
	if(m_fileinfo->m_sizec>0 && m_fileinfo->m_size>0)
	{
		f9FileDisk file;
		if(!file.Open( m_arcname, mode )) return false;
		if(!file.Seek( m_fileinfo->m_offset, F9_SEEK_SET )) { file.Close(); return false; }
		std::auto_ptr<byte> datac(new byte[m_fileinfo->m_sizec]);
		int sizec = (int)file.Read(datac.get(), m_fileinfo->m_sizec);
		file.Close();
		if(sizec != m_fileinfo->m_sizec)
			return false;
		
		// size may be less than sizec !
		dword size = std::max( m_fileinfo->m_size, std::max<dword>(64,m_fileinfo->m_sizec) );

		
		m_data = new byte[size];
		m_size = m_fileinfo->m_size;
		if( !decompress_data( datac.get(), m_fileinfo->m_sizec, m_data, size ) ||
			size != m_fileinfo->m_size )
		{
			delete [] m_data; 
			m_data = nullptr;
			return false;
		}
	}
	return true;
}

bool f9FilePakZ::DoClose()
{
	if(m_data) delete [] m_data;

	m_arcname.clear();
	m_fileinfo	= nullptr;
	m_data		= nullptr;
	m_size		= 0;
	m_pos		= 0;
	return true;
}

bool f9FilePakZ::DoSeek(int64 offset, int origin)
{
	// convert to F9_SEEK_SET
	int64 pos = offset;
	if(origin==F9_SEEK_END)	pos = m_size - offset;
	if(origin==F9_SEEK_CUR)	pos = m_pos + offset;
		
	// bounds
	if(pos<0) pos = 0;
	if(pos>m_size) pos = m_size;

	m_pos = pos;
	return true;
}

int64 f9FilePakZ::DoRead(void* data, int64 size)
{
	if( m_pos + size > m_size ) size = m_size - m_pos; // bound
	if(size > 0) memcpy(data, m_data + m_pos, (sizet)size );
	m_pos += size;
	return size;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
