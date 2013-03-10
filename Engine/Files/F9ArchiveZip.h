///////////////////////////////////////////////////////////////////////////////////////////////////
// F9ArchiveZip.h
// This file handles .zip archives
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9ARCHIVEZIP_H__
#define __F9ARCHIVEZIP_H__

#include "F9Archive.h"
#include "F9FileDisk.h"
#include "..\\Libs\\zlib\\zlib.h"

#include <hash_map>

struct f9ZipFileInfo
{
	dword	m_offset;
	dword	m_size;
	std::string	m_name;
};

class f9ArchiveZip : public f9Archive
{
public:

						f9ArchiveZip	();
virtual					~f9ArchiveZip	();
						
virtual	int				Open			( const char *name, int mode = F9_READ, const char* password=NULL );
virtual	int				Close			();
						
virtual	f9File*	 		FileOpen		( const char* name, int mode = F9_READ );
virtual	int				FileClose		( f9File* file );
virtual	int				FileCount		()									{ return m_fat.size(); }
virtual	int				FileFind		( const char* name );
virtual	std::string		FileGetName		( int idx );
virtual	dword			FileGetSize		( int idx );
inline	f9ZipFileInfo*	FileGetInfo		( int idx )							{ return idx >= 0 && idx < static_cast<int>(m_fat.size()) ? m_fat[idx] : 0; }

private:
		BOOL			ReadFAT			();
		typedef std::vector<f9ZipFileInfo *> InfoList;
		InfoList m_fat;		// file allocation table
		typedef std::hash_map<std::string, int> Hash;
		Hash					index;		// hash for FAT (name,idx)
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
