///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileDisk.h
// This is the standard FILE* wrapper
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILEDISK_H__
#define __F9FILEDISK_H__

#include "F9File.h"

class f9FileDisk : public f9File
{
	virtual bool DoOpen(const std::string & name, int mode);
	virtual	bool DoClose();
	virtual	int64 DoRead(void* data, int64 size);
	virtual	int64 DoWrite(void* data, int64 size);
	virtual	bool DoSeek(int64 offset, int origin = F9_SEEK_SET);
	virtual	int64 DoTell() const;
	virtual	int64 DoSize();
	virtual	bool DoEof() const;
public:
	f9FileDisk() : m_file(nullptr) {}
	virtual ~f9FileDisk() { Close(); }


	FILE * GetFILE() const { return m_file; }

protected:
		FILE*	m_file;		// file handler
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
