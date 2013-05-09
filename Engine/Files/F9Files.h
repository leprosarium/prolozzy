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
// F9_FileOpen, files->FileClose, F9_FileRead, F9_FileWrite, F9_FileSeek, F9_FileTell, F9_FileEof, F9_FileSize
// F9_MakeFileName
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILES_H__
#define __F9FILES_H__

#include <vector>
#include <map>
#include <assert.h>
#include <sstream>

#include "F9Archive.h"


bool F9_Init();
void F9_Done();

// helper for memory files ext must not exceed 5 characters but may be NULL - uses sprint!
inline std::string F9_MakeFileName(const std::string & name, void * addr, int size) 
{ 
	std::ostringstream o;
	o << "#" << std::hex << (dwordptr)addr << "#" << size << "#";
	if(!name.empty())
		o << name;
	return o.str();
}

class Files
{
	bool _searchInResources;
	typedef std::vector<f9Archive *> Array;
	Array	Archives;
	typedef std::map<std::string, f9Archive *> Map;
	Map Index;
	template<class Arc> void DoIndex(const std::string & file) { DoIndex(new Arc(), file); }
	void DoIndex(f9Archive * arc, const std::string & file);
public:
	Files() : _searchInResources() {}
	~Files() { for(f9Archive *a: Archives) delete a; }
	void MakeIndex(const std::string & path);
	void FindFiles(const std::string & path, std::function<void(const std::string &)>);
	f9File * OpenFile(const std::string & file);
	template<class FileType>
	static f9File * OpenFile(const std::string & name, int mode = F9_READ)
	{
		f9File * file = new FileType();
		if(file->Open(name, mode) == F9_OK)
			return file;
		delete file;
		return 0;
	}
	void FileClose(f9File * file);
	void searchInResources(bool s) { _searchInResources = s; }
};

extern	Files *	files;

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
