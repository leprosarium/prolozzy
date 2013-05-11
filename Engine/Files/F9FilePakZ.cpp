///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FilePakZ.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9Math.h"
#include "F9FilePakZ.h"
#include "F9ArchivePak.h"

bool f9FilePakZ::Open(const std::string & name, int mode)
{
	if(IsOpen()) Close();
	if(!F9_ISREADONLY(mode)) return false; // readonly

	// archive should set those
	if( !m_fileinfo) return false;

	m_mode	= mode;
	m_data	= nullptr;
	m_size	= 0;
	m_pos	= 0;

	// read whole file data and uncompress it
	if(m_fileinfo->m_sizec>0 && m_fileinfo->m_size>0)
	{
		f9FileDisk file;
		if(!file.Open( m_arcname, m_mode )) return false;
		if(!file.Seek( m_fileinfo->m_offset, F9_SEEK_SET )) { file.Close(); return false; }
		std::auto_ptr<byte> datac(new byte[m_fileinfo->m_sizec]);
		int sizec = (int)file.Read(datac.get(), m_fileinfo->m_sizec);
		file.Close();
		if(sizec != m_fileinfo->m_sizec)
			return false;
		
		// size may be less than sizec !
		dword size = std::max( m_fileinfo->m_size, std::max<dword>(64,m_fileinfo->m_sizec) );

		
		m_data = (byte*)malloc(size);
		m_size = m_fileinfo->m_size;
		if( !decompress_data( datac.get(), m_fileinfo->m_sizec, m_data, size ) ||
			size != m_fileinfo->m_size )
		{
			free(m_data); 
			m_data = nullptr;
			return false;
		}
	}

	m_open = true;
	return true;
}

bool f9FilePakZ::Close()
{
	if(!IsOpen()) return false;
	if(m_data) free(m_data);

	m_arcname.clear();
	m_fileinfo	= nullptr;
	m_data		= nullptr;
	m_size		= 0;
	m_pos		= 0;

	m_open = false;
	return true;
}

bool f9FilePakZ::Seek(int64 offset, int origin)
{
	if(!IsOpen()) return false;
	
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

int64 f9FilePakZ::Tell()
{
	if(!IsOpen()) return F9_FAIL;
	return m_pos;
}

int64 f9FilePakZ::Size()
{
	if(!IsOpen()) return F9_FAIL;
	return m_size;
}

bool f9FilePakZ::Eof()
{
	if(!IsOpen()) return true;
	return m_pos == m_size;
}

int64 f9FilePakZ::Read(void* data, int64 size)
{
	if(!IsOpen() || !data) return 0;
	if( m_pos + size > m_size ) size = m_size - m_pos; // bound
	if(size > 0) memcpy(data, m_data + m_pos, (sizet)size );
	m_pos += size;
	return size;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
