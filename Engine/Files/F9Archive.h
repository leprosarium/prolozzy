//////////////////////////////////////////////////////////////////////////////////////////////////
// F9Archive.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9ARCHIVE_H__
#define __F9ARCHIVE_H__

#include "F9File.h"

#define F9_ARCHIVE_NONE		(0<<8)
#define F9_ARCHIVE_ZIP		(1<<8)
#define F9_ARCHIVE_PAK		(2<<8)

class f9Archive
{
public:
	f9Archive(int type);
	virtual ~f9Archive();
						
	virtual int Open(const std::string & name , int mode = F9_READ, const std::string & password = std::string() );
	virtual int Close();
	inline bool IsOpen() const { return m_open; }
						
	virtual	f9File * FileOpen(const std::string & name, int mode = F9_READ) { return nullptr; }
	virtual int FileCount() const { return 0; }
	virtual	int FileFind(const std::string & name) const { return -1; }		// get arc file's index in the archive
	virtual	std::string FileGetName(int idx) const = 0;						// get arc file's name
	virtual	dword FileGetSize(int idx) const { return 0; }					// get arc file's size

public:
	int type;				// archive type
	int m_mode;				// open mode
	bool m_open;			// if opened
	std::string m_name;		// archive name
	std::string m_password;	// archive password
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
