///////////////////////////////////////////////////////////////////////////////////////////////////
// F9File.h
// This is the base class for the files used in the file system
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9FILE_H__
#define __F9FILE_H__

#include "E9System.h"
#include "E9String.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// Defines
///////////////////////////////////////////////////////////////////////////////////////////////////
#define F9_FAIL				-1

// open modes
#define F9_READ				0	// "rb"
#define F9_WRITE			1	// "wb"
#define F9_READWRITE		2	// "r+b"
#define F9_WRITEREAD		3	// "w+b"

// seek modes ( better to match original values: 0,1,2 )
#define F9_SEEK_SET			SEEK_SET
#define F9_SEEK_CUR			SEEK_CUR
#define F9_SEEK_END			SEEK_END

///////////////////////////////////////////////////////////////////////////////////////////////////
// f9File class
///////////////////////////////////////////////////////////////////////////////////////////////////
class f9File
{
	virtual bool DoOpen(const std::string & name, int mode) = 0;
	virtual bool DoClose() = 0;
	virtual	int64 DoRead(void * data, int64 size) = 0;
	virtual	int64 DoWrite(void * data, int64 size) { return 0; }
	virtual	bool DoSeek(int64 offset, int origin = F9_SEEK_SET) = 0;
	virtual	int64 DoTell() const { 	return m_pos; } 
	virtual	int64 DoSize() { return m_size; }
	virtual	bool DoEof() const  { return m_pos >= m_size; }
public:
	f9File() : m_mode(F9_READ), m_open(), m_pos(0), m_size(0) {}
	virtual ~f9File() {}
	bool Open(const std::string & name, int mode) 
	{ 
		if(IsOpen()) Close(); 
		if(DoOpen(name, mode)) 
		{ 
			m_mode = mode; 
			m_open = true; 
		}
		return m_open;
	}
	bool Close() { 	if(!IsOpen()) return false; if(DoClose()) m_open = false; return !m_open; }
	int64 Read(void * data, int64 size) { if(!IsOpen() || !data) return 0; return DoRead(data, size); }
	int64 Write(void * data, int64 size) { 	if(!IsOpen() || !data) return 0; return DoWrite(data, size); }
	bool Seek(int64 offset, int origin = F9_SEEK_SET) { if(!IsOpen()) return false; return DoSeek(offset, origin); }
	int64 Tell() const {  if(!IsOpen()) return F9_FAIL; return DoTell(); }
	int64 Size() { if(!IsOpen()) return F9_FAIL; return DoSize(); }
	bool Eof() const { 	if(!IsOpen()) return true; return DoEof(); }

	bool IsOpen() const	{ return m_open; }

	static bool IsReadOnlyMode(int mode) { return (mode & F9_WRITEREAD)==0; }
protected:
	int m_mode;		// open mode
	bool m_open;	// if opened
	int64 m_pos;	// current pos
	int64 m_size;	// file size
};

typedef f9File* F9FILE;

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
