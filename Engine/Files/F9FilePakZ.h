///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FilePakZ.h
// Compressed file from Pak archive
// This caches the whole zipped file at open time, and decompress it
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILEPAKZ_H__
#define __F9FILEPAKZ_H__

#include "F9File.h"
#include "F9FileDisk.h"

struct f9PakFileInfo;

class f9FilePakZ : public f9File
{
public:
	f9FilePakZ() : f9File(F9_FILE_PAKZ), m_fileinfo(nullptr), m_data(nullptr) {}
	virtual bool Open(const std::string & name, int mode);
	virtual bool Close();
	virtual int64 Read(void* data, int64 size);
	virtual bool Seek(int64 offset, int origin = F9_SEEK_SET);
	virtual int64 Tell();
	virtual int64 Size();
	virtual bool Eof();

protected:
	f9PakFileInfo * m_fileinfo;		// info from pak
	std::string  m_arcname;				// just a pointer to zip archive name
	byte * m_data;					// unpacked data

	friend	class f9ArchivePak;
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
