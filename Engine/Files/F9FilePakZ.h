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
	virtual bool DoOpen(const std::wstring & name, int mode);
	virtual bool DoClose();
	virtual int64 DoRead(void* data, int64 size);
	virtual bool DoSeek(int64 offset, int origin = F9_SEEK_SET);

public:
	f9FilePakZ() : m_fileinfo(nullptr), m_data(nullptr) {}


protected:
	f9PakFileInfo * m_fileinfo;		// info from pak
	std::wstring  m_arcname;		// zip archive name
	byte * m_data;					// unpacked data

	friend	class f9ArchivePak;
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
