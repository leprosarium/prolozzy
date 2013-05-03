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
#include <assert.h>

#include "F9Archive.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// f9Files class
///////////////////////////////////////////////////////////////////////////////////////////////////
class f9Files
{
public:
	f9Files() : _searchInResources() {}
	virtual	~f9Files() {}
	void Init() {}
	void Done();

// archives serve
	f9Archive * ArchiveOpen(const char * name, int mode = F9_READ, const char* password=NULL);	// returning archive index
		void		ArchiveClose			( int idx );												// close an archive
		f9Archive*	ArchiveGet				( int idx );												// get archive pointer
	f9Archive * ArchiveFind(const char * name);									// return archive index or -1 if not found
	f9Archive * ArchiveFindContaining(const char * filename);					// strictly look for the file name
	f9Archive * ArchiveFindContainingEx(const char * path);						// try to fit path with the archive path and look for the rest
		int			ArchiveGetFileCount		( int idx );												// return number of files in an archive
		std::string	ArchiveGetFileName		( int idx, int fileidx );									// return a file's name in an archive

// resource serve
	void searchInResources(bool s) { _searchInResources = s; }

// files serve
		f9File*		FileOpen		( const char* name, int mode = F9_READ );		// open a file; with search in archives and other tricks
		int			FileClose		( f9File* file );								// closes a file
	
private:
		typedef std::vector<f9Archive *> Array;
		Array	m_archives;
	bool _searchInResources;	// 1 to search resources
};

///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////
extern	f9Files*	f9_files;			// global instance

bool F9_Init();
void F9_Done();

inline bool	F9_IsReady() { return f9_files != nullptr; }
inline f9Archive * F9_ArchiveOpen(const char* name, int mode=F9_READ, const char* password=NULL) { assert(f9_files); return f9_files->ArchiveOpen(name,mode,password); }
inline void F9_ArchiveClose(int idx) { assert(f9_files); f9_files->ArchiveClose(idx); }
inline int F9_ArchiveGetFileCount(int idx) { assert(f9_files); return f9_files->ArchiveGetFileCount(idx); }
inline std::string F9_ArchiveGetFileName(int idx, int fileidx) { assert(f9_files); return f9_files->ArchiveGetFileName(idx,fileidx); }
inline void F9_ResourcesSearch(bool s) { assert(f9_files); f9_files->searchInResources(s); }
inline F9FILE F9_FileOpen(const char* name, int mode=F9_READ) { assert(f9_files); return f9_files->FileOpen(name, mode); }
inline int F9_FileClose(F9FILE file) { assert(f9_files); return f9_files->FileClose(file); }

// helper for memory files ext must not exceed 5 characters but may be NULL - uses sprint!
inline char * F9_MakeFileName(const char * name, void * addr, int size) { return sprint("#%x#%x#%s",(dwordptr)addr,size,name?name:""); }

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
