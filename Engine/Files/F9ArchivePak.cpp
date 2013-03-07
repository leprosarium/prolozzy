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
	guard(f9ArchivePak::f9ArchivePak);
	m_type = F9_ARCHIVE_PAK;
	m_header.m_fatfiles	=0;
	m_header.m_fatoffset=sizeof(f9PakHeader);
	m_header.m_fatsizec	=0;
	m_header.m_fatsize  =0;
	unguard();
}

f9ArchivePak::~f9ArchivePak()
{
	guard(f9ArchivePak::~f9ArchivePak);
	unguard();
}

int f9ArchivePak::Open( const char *name, int mode, const char* password )
{
	guard(f9ArchivePak::Open);
	if(IsOpen()) Close();
	if(name==NULL) return F9_FAIL;
	if(!F9_ISREADONLY(mode)) return F9_FAIL; // readonly

	f9Archive::Open( name, mode, password );

	if( !ReadHeader() ) { Close(); return F9_FAIL; }
	if( !ReadFAT() )	{ Close(); return F9_FAIL; }
	
	return F9_OK;
	unguard();
}

int f9ArchivePak::Close()
{
	guard(f9ArchivePak::Close);
	if(!IsOpen()) return F9_FAIL;
	index.clear();
	for(InfoList::iterator i = m_fat.begin(), e = m_fat.end(); i != e; ++i) sdelete(*i);
	m_fat.clear();
	f9Archive::Close();	
	return F9_OK;
	unguard();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// files serve
//////////////////////////////////////////////////////////////////////////////////////////////////
f9File* f9ArchivePak::FileOpen( const char* name, int mode )
{
	guard(f9ArchivePak::FileOpen);
	if(!IsOpen()) return NULL;
	if(name==NULL) return NULL;
	if( (mode & 3) != (m_mode & 3) ) return NULL; // open mode must match

	int i = FileFind( name );
	if( i<0 ) return NULL;

	f9File* file = NULL;
	
	if( m_fat[i]->m_flags & F9_PAK_COMPRESSED )
	{
		file = snew f9FilePakZ();
		((f9FilePakZ*)file)->m_arcname = m_name;
		((f9FilePakZ*)file)->m_fileinfo = m_fat[i];
	}
	else
	{
		file = snew f9FilePak();
		((f9FilePak*)file)->m_arcname = m_name;
		((f9FilePak*)file)->m_fileinfo = m_fat[i];
	}

	if( file->Open(name, m_mode)!=F9_OK )
	{
		sdelete(file);
		return NULL;
	}
	
	return file;
	unguard();
}

int f9ArchivePak::FileClose(f9File* file)
{
	guard(f9ArchivePak::FileClose);
	if(!IsOpen()) return F9_FAIL;
	if(!file) return F9_FAIL;
	if(file->Close()!=F9_OK) return F9_FAIL;
	sdelete(file);
	return F9_OK;
	unguard();
}

int f9ArchivePak::FileFind( const char* name )
{
	guard(f9ArchivePak::FileFind);

	std::string nm(name);
	std::transform(nm.begin(), nm.end(), nm.begin(), ::tolower);

	Hash::iterator i = index.find(nm);
	if(i==index.end())
		return -1;
	return i->second;
	unguard();
}

char* f9ArchivePak::FileGetName( int idx )
{
	guard(f9ArchivePak::FileGetName);
	if(idx >= 0 && idx < m_fat.size())
		return m_fat[idx]->m_name;	
	return 0;
	unguard();
}

dword f9ArchivePak::FileGetSize( int idx )
{
	guard(f9ArchivePak::FileGetSize);
	if(idx >= 0 && idx < m_fat.size())
		return m_fat[idx]->m_size;
	return 0;
	unguard();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// private
//////////////////////////////////////////////////////////////////////////////////////////////////
BOOL f9ArchivePak::ReadHeader()
{
	guard(f9ArchivePak::ReadHeader);
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
	unguard();
}

BOOL f9ArchivePak::ReadFAT()
{
	guard(f9ArchivePak::ReadFAT);
	// header must be all valid
	if(m_header.m_fatfiles==0) return TRUE; // no fat entries
	sassert(m_header.m_fatsizec!=0); // check
	sassert(m_header.m_fatsize!=0); // check

	// read fat from file
	f9FileDisk file;
	if(F9_OK!=file.Open(m_name, F9_READ )) return FALSE;
	if(F9_OK!=file.Seek(m_header.m_fatoffset,0)) { file.Close(); return FALSE; }
	byte* bufferc = (byte*)smalloc(m_header.m_fatsizec);
	int fatsizec = (int)file.Read(bufferc,m_header.m_fatsizec);
	file.Close();
	if(fatsizec!=m_header.m_fatsizec) { sfree(bufferc); return FALSE; }

	// password
	if(m_password) decrypt_data(bufferc, m_header.m_fatsizec, m_password);

	// check crc
	dword crc=0;
	for(int i=0;i<(int)m_header.m_fatsizec;i++) crc+=bufferc[i];
	if(crc!=m_header.m_fatcrc) { sfree(bufferc); return FALSE; }

	// uncompress
	dword buffersize = m_header.m_fatsize;
	if(buffersize<m_header.m_fatsizec) buffersize = m_header.m_fatsizec; // safety
	// sassert(m_header.m_fatsize>=m_header.m_fatsizec);
	byte* buffer = (byte*)smalloc(m_header.m_fatsize);
	dword fatsize = m_header.m_fatsize;
	if(!decompress_data( bufferc, m_header.m_fatsizec, buffer, fatsize )) { sfree(bufferc); sfree(buffer); return FALSE; }
	sassert(fatsize==m_header.m_fatsize);

	// read fat entries
	int files=0;
	dword pos=0;
	while(pos<fatsize-17) // a fat entry have at least 17 bytes @WARNING !
	{
		f9PakFileInfo* fi = snew f9PakFileInfo();
		fi->m_flags	= *(dword*)(buffer+pos+0);
		fi->m_offset= *(dword*)(buffer+pos+4);
		fi->m_sizec	= *(dword*)(buffer+pos+8);
		fi->m_size	= *(dword*)(buffer+pos+12);
		fi->m_name	= (char*)(buffer+pos+16);
		pos += 16 + fi->m_name.Len() + 1;
		
		// add file info
		int idx = m_fat.size();
		m_fat.push_back(fi);

		// add to hash
		index.insert(Hash::value_type(fi->m_name.m_data, idx));
	}
	sassert(files!=m_header.m_fatfiles); // check
	
	sfree(buffer);
	sfree(bufferc);
	return TRUE;
	unguard();
}

///////////////////////////////////////////////////////////////////////////////////////////////////
