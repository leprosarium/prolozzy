///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FilePakZ.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9Math.h"
#include "F9FilePakZ.h"
#include "F9ArchivePak.h"

int f9FilePakZ::Open(const char * name, int mode)
{
	if(IsOpen()) Close();
	if(!name) return F9_FAIL;
	if(!F9_ISREADONLY(mode)) return F9_FAIL; // readonly

	// archive should set those
	if( !m_arcname || !m_fileinfo) return F9_FAIL;

	m_mode	= mode;
	m_data	= nullptr;
	m_size	= 0;
	m_pos	= 0;

	// read whole file data and uncompress it
	if(m_fileinfo->m_sizec>0 && m_fileinfo->m_size>0)
	{
		f9FileDisk file;
		if(file.Open( m_arcname, m_mode )!=F9_OK) return F9_FAIL;
		if(file.Seek( m_fileinfo->m_offset, F9_SEEK_SET )!=F9_OK) { file.Close(); return F9_FAIL; }
		std::auto_ptr<byte> datac(new byte[m_fileinfo->m_sizec]);
		int sizec = (int)file.Read(datac.get(), m_fileinfo->m_sizec);
		file.Close();
		if(sizec != m_fileinfo->m_sizec)
			return F9_FAIL;
		
		// size may be less than sizec !
		dword size = std::max( m_fileinfo->m_size, std::max<dword>(64,m_fileinfo->m_sizec) );

		
		m_data = (byte*)malloc(size);
		m_size = m_fileinfo->m_size;
		if( !decompress_data( datac.get(), m_fileinfo->m_sizec, m_data, size ) ||
			size != m_fileinfo->m_size )
		{
			free(m_data); 
			m_data = nullptr;
			return F9_FAIL;
		}
	}

	m_open = true;
	return F9_OK;
}

int f9FilePakZ::Close()
{
	if(!IsOpen()) return F9_FAIL;
	if(m_data) free(m_data);

	m_arcname	= nullptr;
	m_fileinfo	= nullptr;
	m_data		= nullptr;
	m_size		= 0;
	m_pos		= 0;

	m_open = false;
	return F9_OK;
}

int f9FilePakZ::Seek(int64 offset, int origin)
{
	if(!IsOpen()) return F9_FAIL;
	
	// convert to F9_SEEK_SET
	int64 pos = offset;
	if(origin==F9_SEEK_END)	pos = m_size - offset;
	if(origin==F9_SEEK_CUR)	pos = m_pos + offset;
		
	// bounds
	if(pos<0) pos = 0;
	if(pos>m_size) pos = m_size;

	m_pos = pos;
	return F9_OK;
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

int f9FilePakZ::Eof()
{
	if(!IsOpen()) return F9_FAIL;
	return m_pos == m_size ? 1 : 0;
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
