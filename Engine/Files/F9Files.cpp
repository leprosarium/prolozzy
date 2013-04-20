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

f9Files::f9Files()
{
	m_resources = 0;
}

f9Files::~f9Files()
{
}

void f9Files::Init()
{
}

void f9Files::Done()
{
	for(int i=0 ,e = static_cast<int>(m_archives.size());i<e;i++) ArchiveClose(i);
	m_archives.clear();
}

///////////////////////////////////////////////////////////////////////////////////////////////////
int f9Files::ArchiveOpen( const char* name, int mode, const char* password )
{

	// check if already opened (in list)
	int idx = ArchiveFind( name );
	if( idx!=-1 ) return idx;

	// create archive
	f9Archive* arc = NULL;
	#ifndef EXCLUDE_FILEZIP
	if( mode & F9_ARCHIVE_ZIP )	arc = new f9ArchiveZip(); else
	#endif
	#ifndef	EXCLUDE_FILEPAK
	if( mode & F9_ARCHIVE_PAK )	arc = new f9ArchivePak();
	#endif
	if( arc==NULL ) return -1;
	mode &= 255; // remove archive type from the mode

	if( arc->Open( name, mode, password )!=F9_OK )
	{
		delete arc;
		return -1;
	}

	idx = m_archives.size();
	m_archives.push_back(arc);
	return idx;
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

int f9Files::ArchiveFind( const char* name )
{
	for(int i=0,e=static_cast<int>(m_archives.size()); i<e; i++)
		if(m_archives[i] && 0==stricmp(m_archives[i]->m_name, name) ) 
			return i;
	return -1;
}

int f9Files::ArchiveFindContaining( const char* filename )
{
	for(int i=0, e=static_cast<int>(m_archives.size()); i<e; i++)
		if( m_archives[i] && m_archives[i]->FileFind( filename ) != -1 )	
			return i;
	return -1;
}

int f9Files::ArchiveFindContainingEx( const char* path )
{
	if(m_archives.size()==0) return -1;

	int j,k;
	k = (int)strlen(path);
	for(int i=0, e=static_cast<int>(m_archives.size()); i<e; i++)
	{
		if(!m_archives[i]) continue;
		char* arcname = m_archives[i]->m_name;	assert(arcname);
		const char* sz = file_path2file( arcname );		assert(sz);
		int ap =(int)(sz - arcname);
		if( ap >= k ) continue; // file path is smaller than archive dir !
		
		for( j=0; j<ap; j++)
			if( tolower(path[j]) != tolower(arcname[j]) ) break;

		if( j<ap ) continue; // archive dir not at the begining of file dir !

		if( m_archives[i]->FileFind( path+ap ) != -1 )	return i;
	}

	return -1;
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
	if(idx < 0 || idx >= static_cast<int>(m_archives.size())) return 0;
	f9Archive* archive = m_archives[idx];		
	if(!archive) return NULL;
	if(idx<0 || idx>=archive->FileCount()) return NULL;
	return archive->FileGetName(fileidx);
}


///////////////////////////////////////////////////////////////////////////////////////////////////
f9File* f9Files::FileOpen( const char* name, int mode )
{
	if( name==NULL ) return NULL;
	BOOL readonly = F9_ISREADONLY(mode);

	// ARCHIVE FILE
	if(readonly)
	{
		int idx = ArchiveFindContainingEx( name );
		if(idx!=-1)
		{
			f9Archive* arc = ArchiveGet(idx); assert(arc);
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
	if( readonly && m_resources )
	{
		file = new f9FileRes();
		if(file->Open(name,mode)==F9_OK) return file;
		delete file;
	}
	
	//FILE DISK
	file = new f9FileDisk();
	if(file->Open(name,mode)==F9_OK) return file;
	delete file;

	return NULL;
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
f9Files* f9_files = NULL;

BOOL F9_Init()
{
	if(f9_files) return TRUE;
	dlog(LOGFIL, L"Files init.\n");
	f9_files = new f9Files();
	f9_files->Init();
	return TRUE;
}

void F9_Done()
{
	if(!f9_files) return;
	f9_files->Done();
	delete  f9_files ;
	f9_files = NULL;
	dlog(LOGFIL, L"Files done.\n");
}

/*
///////////////////////////////////////////////////////////////////////////////////////////////////
// User funcions (can be used as callbacks)
///////////////////////////////////////////////////////////////////////////////////////////////////
void*	file_open		( char* name, int mode )					{ return f9Files::FileOpen(name, mode); }
int		file_close		( void* file )								{ return f9Files::FileClose((cFile*)file); }
int		file_read		( void* buffer, int size, void* file )		{ return ((cFile*)file)->Read( buffer, size ); }
int		file_write		( void* buffer, int size, void* file )		{ return ((cFile*)file)->Write( buffer, size ); }
int		file_seek		( void* file, int offset, int origin )		{ return ((cFile*)file)->Seek( offset, origin ); }
int		file_tell		( void* file )								{ return ((cFile*)file)->Tell(); }
int		file_eof		( void* file )								{ return ((cFile*)file)->Eof(); }
int		file_size		( void* file )								{ return ((cFile*)file)->Size(); }
*/

///////////////////////////////////////////////////////////////////////////////////////////////////
