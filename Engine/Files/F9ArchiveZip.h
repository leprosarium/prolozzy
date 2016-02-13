///////////////////////////////////////////////////////////////////////////////////////////////////
// F9ArchiveZip.h
// This file handles .zip archives
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __F9ARCHIVEZIP_H__
#define __F9ARCHIVEZIP_H__

#include "F9Archive.h"
#include "F9FileDisk.h"
#include "..\\Libs\\zlib\\zlib.h"

#include <string>
#include <unordered_map>

struct f9ZipFileInfo
{
	dword	m_offset;
	dword	m_size;
	std::string	m_name;
};

class f9ArchiveZip : public f9Archive
{
public:
	f9ArchiveZip();
	virtual ~f9ArchiveZip();
						
	virtual	bool Open(const std::string & name, int mode = F9_READ, const std::string & password = std::string() );
	virtual	bool Close();
						
	virtual	f9File * FileOpen(const std::string & name, int mode = F9_READ);
	virtual	int FileCount() const { return m_fat.size(); }
	virtual	int FileFind( const std::string & name ) const;
	virtual	std::string	FileGetName(int idx) const;
	virtual	dword FileGetSize(int idx) const;
	inline	f9ZipFileInfo *	FileGetInfo(int idx) const { return idx >= 0 && idx < static_cast<int>(m_fat.size()) ? m_fat[idx] : 0; }

private:
	bool ReadFAT();
	typedef std::vector<f9ZipFileInfo *> InfoList;
	InfoList m_fat;
	typedef std::unordered_map<std::string, int> Hash;
	Hash index;
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
