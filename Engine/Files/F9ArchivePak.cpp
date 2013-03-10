///////////////////////////////////////////////////////////////////////////////////////////////////
// F9ArchivePak.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "D9Debug.h"
#include "F9ArchivePak.h"
#include "F9FilePak.h"
#include "F9FilePakZ.h"

#include <algorithm>

f9ArchivePak::f9ArchivePak()
{
	m_type = F9_ARCHIVE_PAK;
	m_header.m_fatfiles	=0;
	m_header.m_fatoffset=sizeof(f9PakHeader);
	m_header.m_fatsizec	=0;
	m_header.m_fatsize  =0;
}

f9ArchivePak::~f9ArchivePak()
{
}

int f9ArchivePak::Open( const char *name, int mode, const char* password )
{
	if(IsOpen()) Close();
	if(name==NULL) return F9_FAIL;
	if(!F9_ISREADONLY(mode)) return F9_FAIL; // readonly

	f9Archive::Open( name, mode, password );

	if( !ReadHeader() ) { Close(); return F9_FAIL; }
	if( !ReadFAT() )	{ Close(); return F9_FAIL; }
	
	return F9_OK;
}

int f9ArchivePak::Close()
{
	if(!IsOpen()) return F9_FAIL;
	index.clear();
	for(InfoList::iterator i = m_fat.begin(), e = m_fat.end(); i != e; ++i) delete *i;
	m_fat.clear();
	f9Archive::Close();	
	return F9_OK;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// files serve
//////////////////////////////////////////////////////////////////////////////////////////////////
f9File* f9ArchivePak::FileOpen( const char* name, int mode )
{
	if(!IsOpen()) return NULL;
	if(name==NULL) return NULL;
	if( (mode & 3) != (m_mode & 3) ) return NULL; // open mode must match

	int i = FileFind( name );
	if( i<0 ) return NULL;

	f9File* file = NULL;
	
	if( m_fat[i]->m_flags & F9_PAK_COMPRESSED )
	{
		file = new f9FilePakZ();
		((f9FilePakZ*)file)->m_arcname = m_name;
		((f9FilePakZ*)file)->m_fileinfo = m_fat[i];
	}
	else
	{
		file = new f9FilePak();
		((f9FilePak*)file)->m_arcname = m_name;
		((f9FilePak*)file)->m_fileinfo = m_fat[i];
	}

	if( file->Open(name, m_mode)!=F9_OK )
	{
		delete file;
		return NULL;
	}
	
	return file;
}

int f9ArchivePak::FileClose(f9File* file)
{
	if(!IsOpen()) return F9_FAIL;
	if(!file) return F9_FAIL;
	if(file->Close()!=F9_OK) return F9_FAIL;
	delete file;
	return F9_OK;
}

int f9ArchivePak::FileFind( const char* name )
{

	std::string nm(name);
	std::transform(nm.begin(), nm.end(), nm.begin(), ::tolower);

	Hash::iterator i = index.find(nm);
	if(i==index.end())
		return -1;
	return i->second;
}

std::string f9ArchivePak::FileGetName( int idx )
{
	if(idx >= 0 && idx < static_cast<int>(m_fat.size()))
		return m_fat[idx]->m_name;	
	return std::string();
}

dword f9ArchivePak::FileGetSize( int idx )
{
	if(idx >= 0 && idx < static_cast<int>(m_fat.size()))
		return m_fat[idx]->m_size;
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// private
//////////////////////////////////////////////////////////////////////////////////////////////////
BOOL f9ArchivePak::ReadHeader()
{
	// read file
	f9FileDisk file;
	if(F9_OK!=file.Open( m_name, F9_READ )) return FALSE;
	int headersize = (int)file.Read( &m_header, sizeof(f9PakHeader) );
	file.Close();
	if( headersize != sizeof(f9PakHeader) ) return FALSE;
	// check id
	dword id = (((dword)F9_PAK_VERSION)<<24) + (((dword)'K')<<16) + (((dword)'A')<<8) + ((dword)'P');
	if(m_header.m_id != id) return FALSE; // wrong pack version
	return TRUE;
}

BOOL f9ArchivePak::ReadFAT()
{
	// header must be all valid
	if(m_header.m_fatfiles==0) return TRUE; // no fat entries
	assert(m_header.m_fatsizec!=0); // check
	assert(m_header.m_fatsize!=0); // check

	// read fat from file
	f9FileDisk file;
	if(F9_OK!=file.Open(m_name, F9_READ )) return FALSE;
	if(F9_OK!=file.Seek(m_header.m_fatoffset,0)) { file.Close(); return FALSE; }
	byte* bufferc = (byte*)malloc(m_header.m_fatsizec);
	int fatsizec = (int)file.Read(bufferc,m_header.m_fatsizec);
	file.Close();
	if(fatsizec!=m_header.m_fatsizec) { free(bufferc); return FALSE; }

	// password
	if(m_password) decrypt_data(bufferc, m_header.m_fatsizec, m_password);

	// check crc
	dword crc=0;
	for(int i=0;i<(int)m_header.m_fatsizec;i++) crc+=bufferc[i];
	if(crc!=m_header.m_fatcrc) { free(bufferc); return FALSE; }

	// uncompress
	dword buffersize = m_header.m_fatsize;
	if(buffersize<m_header.m_fatsizec) buffersize = m_header.m_fatsizec; // safety
	// assert(m_header.m_fatsize>=m_header.m_fatsizec);
	byte* buffer = (byte*)malloc(m_header.m_fatsize);
	dword fatsize = m_header.m_fatsize;
	if(!decompress_data( bufferc, m_header.m_fatsizec, buffer, fatsize )) { free(bufferc); free(buffer); return FALSE; }
	assert(fatsize==m_header.m_fatsize);

	// read fat entries
	int files=0;
	dword pos=0;
	while(pos<fatsize-17) // a fat entry have at least 17 bytes @WARNING !
	{
		f9PakFileInfo* fi = new f9PakFileInfo();
		fi->m_flags	= *(dword*)(buffer+pos+0);
		fi->m_offset= *(dword*)(buffer+pos+4);
		fi->m_sizec	= *(dword*)(buffer+pos+8);
		fi->m_size	= *(dword*)(buffer+pos+12);
		fi->m_name	= (char*)(buffer+pos+16);
		pos += 16 + fi->m_name.size() + 1;
		
		// add file info
		int idx = m_fat.size();
		m_fat.push_back(fi);

		// add to hash
		index.insert(Hash::value_type(fi->m_name, idx));
	}
	assert(files!=m_header.m_fatfiles); // check
	
	free(buffer);
	free(bufferc);
	return TRUE;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
