///////////////////////////////////////////////////////////////////////////////////////////////////
// F9Files.h
// order for opening files:
// 1. check archives if readonly
// 2. check resources if resources opened and readonly and IFile initialized
// 3. check if memory file ( name[0]=='#' as for "#hexaddr#hexsize#name.ext" )
// 4. open from disk
// Interface:
// F9FILE
// F9_Init, F9_Done, F9_IsReady
// F9_ArchiveOpen, F9_ArchiveClose, F9_ArchiveGetFileCount, F9_ArchiveGetFileName
// F9_ResourcesOpen, F9_ResourcesClose	
// F9_FileOpen, F9_FileClose, F9_FileRead, F9_FileWrite, F9_FileSeek, F9_FileTell, F9_FileEof, F9_FileSize
// F9_MakeFileName
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILES_H__
#define __F9FILES_H__

#include <vector>

#include "F9Archive.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// f9Files class
///////////////////////////////////////////////////////////////////////////////////////////////////
class f9Files
{
public:
					f9Files			();
virtual				~f9Files		();	
		void		Init			();
		void		Done			();

// archives serve
		int			ArchiveOpen				( const char* name, int mode = F9_READ, const char* password=NULL );	// returning archive index
		void		ArchiveClose			( int idx );												// close an archive
		f9Archive*	ArchiveGet				( int idx );												// get archive pointer
		int			ArchiveFind				( const char* name );										// return archive index or -1 if not found
		int			ArchiveFindContaining	( const char* filename );									// strictly look for the file name
		int			ArchiveFindContainingEx	( const char* path );										// try to fit path with the archive path and look for the rest
		int			ArchiveGetFileCount		( int idx );												// return number of files in an archive
		std::string	ArchiveGetFileName		( int idx, int fileidx );									// return a file's name in an archive

// resource serve
inline	void		ResourcesOpen			()										{ m_resources=1; } // open resources (files will be searched in resources)
inline	void		ResourcesClose			()										{ m_resources=0; } // close resources (files will not be searched in resources)

// files serve
		f9File*		FileOpen		( const char* name, int mode = F9_READ );		// open a file; with search in archives and other tricks
		int			FileClose		( f9File* file );								// closes a file
	
private:
		typedef std::vector<f9Archive *> Array;
		Array	m_archives;
		int					m_resources;	// 1 to search resources
};

///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////
extern	f9Files*	f9_files;			// global instance

		BOOL	F9_Init();
		void	F9_Done();
inline	BOOL	F9_IsReady()																	{ return f9_files!=NULL; }
inline	int		F9_ArchiveOpen			( const char* name, int mode=F9_READ, const char* password=NULL )	{ 	assert(f9_files); return f9_files->ArchiveOpen(name,mode,password);  }
inline	void	F9_ArchiveClose			( int idx )												{ 	assert(f9_files); f9_files->ArchiveClose(idx);  }
inline	int		F9_ArchiveGetFileCount	( int idx )												{ 	assert(f9_files); return f9_files->ArchiveGetFileCount(idx);  }
inline	std::string	F9_ArchiveGetFileName	( int idx, int fileidx )							{ 	assert(f9_files); return f9_files->ArchiveGetFileName(idx,fileidx);  }
inline	void	F9_ResourcesOpen		()														{ 	assert(f9_files); f9_files->ResourcesOpen();  }
inline	void	F9_ResourcesClose		()														{ 	assert(f9_files); f9_files->ResourcesClose();  }
inline	F9FILE	F9_FileOpen				( const char* name, int mode=F9_READ )					{ 	assert(f9_files); return f9_files->FileOpen(name, mode);  }
inline	int		F9_FileClose			( F9FILE file )											{ 	assert(f9_files); return f9_files->FileClose(file);  }
inline	int		F9_FileRead				( void* buffer, int size, F9FILE file )					{ 	assert(file); return (int)file->Read( buffer, size );  }
inline	int		F9_FileWrite			( void* buffer, int size, F9FILE file )					{ 	assert(file); return (int)file->Write( buffer, size );  }
inline	int		F9_FileSeek				( F9FILE file, int offset, int origin=F9_SEEK_SET )		{ 	assert(file); return file->Seek( offset, origin );  }
inline	int		F9_FileTell				( F9FILE file )											{ 	assert(file); return (int)file->Tell();  }
inline	int		F9_FileEof				( F9FILE file )											{ 	assert(file); return file->Eof();  }
inline	int		F9_FileSize				( F9FILE file )											{ 	assert(file); return (int)file->Size();  }

// helper for memory files ext must not exceed 5 characters but may be NULL - uses sprint!
inline	char*	F9_MakeFileName		( const char* name, void* addr, int size )						{ return sprint("#%x#%x#%s",(dwordptr)addr,size,name?name:""); }

/*
///////////////////////////////////////////////////////////////////////////////////////////////////
// User funcions (can be used as callbacks)
///////////////////////////////////////////////////////////////////////////////////////////////////
void*	file_open		( char* name, int mode=FS_READ );
int		file_close		( void* file );
int		file_read		( void* buffer, int size, void* file );
int		file_write		( void* buffer, int size, void* file );
int		file_seek		( void* file, int offset, int origin=SEEK_SET );
int		file_tell		( void* file );
int		file_eof		( void* file );
int		file_size		( void* file );
*/

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
