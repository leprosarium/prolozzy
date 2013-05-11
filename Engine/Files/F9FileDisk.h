///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileDisk.h
// This is the standard FILE* wrapper
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILEDISK_H__
#define __F9FILEDISK_H__

#include "F9File.h"

class f9FileDisk : public f9File
{
public:
	f9FileDisk() : m_file(nullptr) {}
	virtual ~f9FileDisk() { Close(); }
	virtual bool Open(const std::string & name, int mode);
	virtual	bool Close();
	virtual	int64 Read(void* data, int64 size);
	virtual	int64 Write(void* data, int64 size);
	virtual	bool Seek(int64 offset, int origin = F9_SEEK_SET);
	virtual	int64 Tell();
	virtual	int64 Size();
	virtual	bool Eof();

	FILE * GetFILE() const { return m_file; }

protected:
		FILE*	m_file;		// file handler
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
