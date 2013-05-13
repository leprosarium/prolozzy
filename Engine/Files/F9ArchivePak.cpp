///////////////////////////////////////////////////////////////////////////////////////////////////
// F9ArchivePak.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9ArchivePak.h"
#include "F9FilePak.h"
#include "F9FilePakZ.h"

#include <algorithm>

f9ArchivePak::f9ArchivePak() : f9Archive(F9_ARCHIVE_PAK)
{
	m_header.m_fatfiles	= 0;
	m_header.m_fatoffset = sizeof(f9PakHeader);
	m_header.m_fatsizec	= 0;
	m_header.m_fatsize  = 0;
}

f9ArchivePak::~f9ArchivePak()
{
	Close();
}

bool f9ArchivePak::Open(const std::string & name, int mode, const std::string & password)
{
	if(IsOpen()) Close();
	if(!f9File::IsReadOnlyMode(mode)) return false; // readonly

	f9Archive::Open(name, mode, password);

	if( !ReadHeader() ) { Close(); return false; }
	if( !ReadFAT() )	{ Close(); return false; }
	
	return true;
}

bool f9ArchivePak::Close()
{
	if(!IsOpen()) return false;
	index.clear();
	for(InfoList::iterator i = m_fat.begin(), e = m_fat.end(); i != e; ++i) delete *i;
	m_fat.clear();
	f9Archive::Close();	
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// files serve
//////////////////////////////////////////////////////////////////////////////////////////////////
f9File * f9ArchivePak::FileOpen(const std::string & name, int mode)
{
	if(!IsOpen()) return nullptr;
	if( (mode & 3) != (m_mode & 3) ) return nullptr; // open mode must match

	int i = FileFind(name);
	if(i < 0) return nullptr;

	f9File * file = nullptr;
	
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

	if(! file->Open(name, m_mode))
	{
		delete file;
		return nullptr;
	}
	
	return file;
}

int f9ArchivePak::FileFind(const std::string & name) const
{
	std::string nm(name);
	std::transform(nm.begin(), nm.end(), nm.begin(), ::tolower);

	auto i = index.find(nm);
	if(i == index.end())
		return -1;
	return i->second;
}

std::string f9ArchivePak::FileGetName(int idx) const
{
	if(idx >= 0 && idx < static_cast<int>(m_fat.size()))
		return m_fat[idx]->m_name;	
	return std::string();
}

dword f9ArchivePak::FileGetSize(int idx) const
{
	if(idx >= 0 && idx < static_cast<int>(m_fat.size()))
		return m_fat[idx]->m_size;
	return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// private
//////////////////////////////////////////////////////////////////////////////////////////////////
bool f9ArchivePak::ReadHeader()
{
	// read file
	f9FileDisk file;
	if(!file.Open( m_name, F9_READ )) return false;
	int headersize = (int)file.Read( &m_header, sizeof(f9PakHeader) );
	file.Close();
	if( headersize != sizeof(f9PakHeader) ) return false;
	// check id
	dword id = (((dword)F9_PAK_VERSION)<<24) + (((dword)'K')<<16) + (((dword)'A')<<8) + ((dword)'P');
	if(m_header.m_id != id) return false; // wrong pack version
	return true;
}

bool f9ArchivePak::ReadFAT()
{
	// header must be all valid
	if(m_header.m_fatfiles==0) return true; // no fat entries
	assert(m_header.m_fatsizec!=0); // check
	assert(m_header.m_fatsize!=0); // check

	// read fat from file
	f9FileDisk file;
	if(!file.Open(m_name, F9_READ )) return false;
	if(!file.Seek(m_header.m_fatoffset,0)) { file.Close(); return false; }
	byte* bufferc = new byte[m_header.m_fatsizec];
	int fatsizec = (int)file.Read(bufferc,m_header.m_fatsizec);
	file.Close();
	if(fatsizec!=m_header.m_fatsizec) { delete [] bufferc; return false; }

	// password
	if(!m_password.empty()) decrypt_data(bufferc, m_header.m_fatsizec, m_password.c_str());

	// check crc
	dword crc=0;
	for(int i=0;i<(int)m_header.m_fatsizec;i++) crc+=bufferc[i];
	if(crc!=m_header.m_fatcrc) { delete [] bufferc; return false; }

	// uncompress
	dword buffersize = m_header.m_fatsize;
	if(buffersize<m_header.m_fatsizec) buffersize = m_header.m_fatsizec; // safety
	// assert(m_header.m_fatsize>=m_header.m_fatsizec);
	byte* buffer = new byte[m_header.m_fatsize];
	dword fatsize = m_header.m_fatsize;
	if(!decompress_data( bufferc, m_header.m_fatsizec, buffer, fatsize )) { delete [] bufferc; delete [] buffer; return false; }
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
	
	delete [] buffer;
	delete [] bufferc;
	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
