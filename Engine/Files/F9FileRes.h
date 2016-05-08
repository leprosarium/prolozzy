///////////////////////////////////////////////////////////////////////////////////////////////////
// F9FileRes.h
// This handles files from win32 resources (READONLY)
// User resources must be in "FILE" group on neutral(!?) language. 
// File name is the resource name
// Reads the resource in memory when opened and then work on the memory buffer
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILERES_H__
#define __F9FILERES_H__

#include "F9File.h"

#define F9_FILERES_GROUP L"FILES"

class f9FileRes : public f9File
{
	virtual bool DoOpen(const std::wstring & name, int mode);
	virtual bool DoClose();
	virtual int64 DoRead(void* data, int64 size);
	virtual bool DoSeek(int64 offset, int origin = F9_SEEK_SET);
public:
	f9FileRes() : m_addr(nullptr) {} 
protected:
	byte * m_addr;
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
