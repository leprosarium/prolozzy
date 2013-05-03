///////////////////////////////////////////////////////////////////////////////////////////////////
// F9Files.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9Files.h"
#include "F9FileMem.h"
#include "F9FileRes.h"
#include "F9ArchiveZip.h"
#include "F9ArchivePak.h"
#include "D9Log.h"


void f9Files::Done()
{
	for(int i=0 ,e = static_cast<int>(m_archives.size());i<e;i++) ArchiveClose(i);
	m_archives.clear();
}

///////////////////////////////////////////////////////////////////////////////////////////////////
f9Archive * f9Files::ArchiveOpen( const char* name, int mode, const char* password )
{

	// check if already opened (in list)
	f9Archive * arc = ArchiveFind( name );
	if(arc) return arc;

	// create archive
	#ifndef EXCLUDE_FILEZIP
	if( mode & F9_ARCHIVE_ZIP )	arc = new f9ArchiveZip(); else
	#endif
	#ifndef	EXCLUDE_FILEPAK
	if( mode & F9_ARCHIVE_PAK )	arc = new f9ArchivePak();
	#endif
	if(!arc) return nullptr;
	mode &= 255; // remove archive type from the mode

	if( arc->Open( name, mode, password )!=F9_OK )
	{
		delete arc;
		return nullptr;
	}

	m_archives.push_back(arc);
	return arc;
}

void f9Files::ArchiveClose( int idx )
{
	if(idx < 0 || idx >= static_cast<int>(m_archives.size())) return;
	Array::iterator it = m_archives.begin() + idx;
	Array::value_type arc = *it;
	if(!arc) return;
	arc->Close();
	delete arc;
	m_archives.erase(it);
}

f9Archive* f9Files::ArchiveGet( int idx )
{
	if(idx < 0 || idx >= static_cast<int>(m_archives.size())) return 0;
	return m_archives[idx];
}

f9Archive * f9Files::ArchiveFind( const char* name )
{
	for(f9Archive * a: m_archives)
		if(a && 0==stricmp(a->m_name, name) ) 
			return a;
	return nullptr;
}

f9Archive * f9Files::ArchiveFindContaining(const char* filename)
{
	for(f9Archive * a: m_archives)
		if( a && a->FileFind( filename ) != -1 )	
			return a;
	return nullptr;
}

f9Archive * f9Files::ArchiveFindContainingEx(const char * path)
{
	if(m_archives.size()==0) return nullptr;

	int j,k;
	k = (int)strlen(path);
	for(f9Archive * a: m_archives)
	{
		if(!a) continue;
		char* arcname = a->m_name;	assert(arcname);
		const char* sz = file_path2file( arcname );		assert(sz);
		int ap =(int)(sz - arcname);
		if( ap >= k ) continue; // file path is smaller than archive dir !
		
		for( j=0; j<ap; j++)
			if( tolower(path[j]) != tolower(arcname[j]) ) break;

		if( j<ap ) continue; // archive dir not at the begining of file dir !

		if(a->FileFind( path+ap ) != -1 ) return a;
	}

	return nullptr;
}

int	f9Files::ArchiveGetFileCount( int idx )
{
	if(idx < 0 || idx >= static_cast<int>(m_archives.size())) return 0;
	if(f9Archive* archive = m_archives[idx])
		return archive->FileCount();
	return 0;
}

std::string f9Files::ArchiveGetFileName( int idx, int fileidx )
{
	if(idx < 0 || idx >= static_cast<int>(m_archives.size())) return std::string();
	f9Archive* archive = m_archives[idx];		
	if(!archive) return std::string();
	if(idx<0 || idx>=archive->FileCount()) return std::string();
	return archive->FileGetName(fileidx);
}


///////////////////////////////////////////////////////////////////////////////////////////////////
f9File* f9Files::FileOpen( const char* name, int mode )
{
	if(!name) return nullptr;
	BOOL readonly = F9_ISREADONLY(mode);

	// ARCHIVE FILE
	if(readonly)
	{
		if(f9Archive * arc = ArchiveFindContainingEx(name))
		{
			const char* sz = file_path2file(arc->m_name); assert(sz);
			int ap = (int)( sz - arc->m_name );
			return arc->FileOpen( name+ap, mode ); // we open the path from inside the archive
		}
	}

	f9File* file;

	// MEMORY FILE
	if( name[0]=='#' )
	{
		file = new f9FileMem();
		if(file->Open(name,mode)==F9_OK) return file;
		delete file;
	}
	
	// RESOURCES FILES
	if( readonly && _searchInResources )
	{
		file = new f9FileRes();
		if(file->Open(name,mode)==F9_OK) return file;
		delete file;
	}
	
	//FILE DISK
	file = new f9FileDisk();
	if(file->Open(name,mode)==F9_OK) return file;
	delete file;

	return nullptr;
}

int f9Files::FileClose( f9File* file )
{
	if(file==NULL) return F9_FAIL;
	if(file->Close()!=F9_OK) return F9_FAIL;
	delete file;
	return F9_OK;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////
f9Files* f9_files = nullptr;

bool F9_Init()
{
	if(f9_files) return true;
	dlog(LOGFIL, L"Files init.\n");
	f9_files = new f9Files();
	f9_files->Init();
	return true;
}

void F9_Done()
{
	if(!f9_files) return;
	f9_files->Done();
	delete  f9_files ;
	f9_files = nullptr;
	dlog(LOGFIL, L"Files done.\n");
}


///////////////////////////////////////////////////////////////////////////////////////////////////
