///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileMem.h
// This handles files from memory
// They use a special naming format "#hexaddr#hexsize#name.ext"
// Obs: handle write calls with great care, because if they exceeds real buffer space...
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILEMEM_H__
#define __F9FILEMEM_H__

#include "F9File.h"

class f9FileMem : public f9File
{
	virtual bool DoOpen(const std::wstring & name, int mode);
	virtual	bool DoClose();
	virtual int64 DoRead(void * data, int64 size);
	virtual	int64 DoWrite(void * data, int64 size);
	virtual bool DoSeek(int64 offset, int origin = F9_SEEK_SET);

public:
	f9FileMem() : m_addr(nullptr) {}
protected:
	byte * m_addr;
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
