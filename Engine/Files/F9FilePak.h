///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FilePak.h
// Uncompressed file from Pak archive
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILEPAK_H__
#define __F9FILEPAK_H__

#include "f9File.h"
#include "f9FileDisk.h"

struct f9PakFileInfo;

class f9FilePak : public f9File
{	
	virtual bool DoOpen(const std::string & name, int mode);
	virtual bool DoClose();
	virtual int64 DoRead(void* data, int64 size);
	virtual bool DoSeek(int64 offset, int origin = F9_SEEK_SET);
	virtual int64 DoTell() const;
	virtual int64 DoSize();
	virtual bool DoEof() const;
public:
	f9FilePak () : m_fileinfo(nullptr) {}

protected:
	f9PakFileInfo * m_fileinfo;		// info from pak
	std::string m_arcname;			// just a pointer to zip archive name
	f9FileDisk m_arcfile;			// archive disk file, opened on Open

	friend	class f9ArchivePak;
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
