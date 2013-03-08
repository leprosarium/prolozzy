///////////////////////////////////////////////////////////////////////////////////////////////////
// F9ArchiveZip.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9Math.h"
#include "F9ArchiveZip.h"
#include "F9FileZip.h"
#include <algorithm>

f9ArchiveZip::f9ArchiveZip()
{
	m_type = F9_ARCHIVE_ZIP;
}

f9ArchiveZip::~f9ArchiveZip()
{
}

int f9ArchiveZip::Open( const char *name, int mode, const char* password )
{
	if( IsOpen() ) Close();
	if( name == NULL ) return F9_FAIL;
	if(!F9_ISREADONLY(mode)) return F9_FAIL; // readonly

	f9Archive::Open( name, mode, password );
	if( !ReadFAT() ) { Close(); return F9_FAIL; }
	
	return F9_OK;
}

int f9ArchiveZip::Close()
{
	if(!IsOpen()) return F9_FAIL;
	index.clear();
	for(InfoList::iterator i = m_fat.begin(), e = m_fat.end(); i != e; ++i) delete *i;
	m_fat.clear();
	f9Archive::Close();	
	return F9_OK;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// files serve
//////////////////////////////////////////////////////////////////////////////////////////////////
f9File* f9ArchiveZip::FileOpen( const char* name, int mode )
{
	if( !IsOpen() ) return NULL;
	if( name == NULL ) return NULL;
	if( (mode & 3) != (m_mode & 3) ) return NULL; // open mode must match

	int i = FileFind( name );
	if( i<0 ) return NULL;

	f9FileZip* fzip	= new f9FileZip();
	fzip->m_offset	= m_fat[i]->m_offset;
	fzip->m_arcname	= m_name;

	if( fzip->Open(name, m_mode)!=F9_OK )
	{
		delete fzip;
		return NULL;
	}
	
	return fzip;
}

int f9ArchiveZip::FileClose(f9File* file)
{
	if(!IsOpen()) return F9_FAIL;
	if(!file) return F9_FAIL;
	if(file->Close()!=F9_OK) return F9_FAIL;
	delete file;
	return F9_OK;
}

int f9ArchiveZip::FileFind( const char* name )
{

	std::string nm(name);
	std::transform(nm.begin(), nm.end(), nm.begin(), ::tolower);

	Hash::iterator i = index.find(nm);
	if (i == index.end())
		return -1;
	return i->second;
}

std::string f9ArchiveZip::FileGetName( int idx )
{
	if(idx >= 0 && idx < m_fat.size()) 
		return m_fat[idx]->m_name;
	return std::string();
}

dword f9ArchiveZip::FileGetSize( int idx )
{
	if(idx >= 0 && idx < m_fat.size())
		return m_fat[idx]->m_size;
	return 0;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// private
///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL f9ArchiveZip::ReadFAT()
{

	int				i;
	f9FileDisk		file;
	int				filesize;
	
	char*			cebuff;		// big buffer to search for the central dir end
	int				cebuffsize;	// size of the cebuff

	zipCentralEnd*	centralend;		// pointer ro central dir end (in cebuff)
	char*			central;		// central dir buffer
	zipFileHeader*	fh;				// pointer to file header (in central)

	f9ZipFileInfo*	fi;
	
	// open archive file
	if( file.Open(m_name, F9_READ ) ) return FALSE;
	filesize = (int)file.Size();

	// read central dir buffer
	cebuffsize =  MIN( filesize, ZIP_CENTRAL_END_BUFFER_SIZE );
	cebuff = (char*)malloc( cebuffsize ); 
	if(!cebuff) { file.Close(); return FALSE; }
	file.Seek( filesize-cebuffsize );
	if( cebuffsize!=file.Read( cebuff, cebuffsize ) ) 
	{ free(cebuff); file.Close(); return FALSE; }

	// find central directory end
	for(i = cebuffsize-4; i >= 0; i--)
	{
		if( *(dword*)(cebuff+i) == ZIP_CENTRAL_END_SIGN ) break;
	}		
	if( i<0 ) { free(cebuff); file.Close(); return FALSE; }
	int centralendpos = i; // filesize - cebuffsize + i // if we used to seek and reread centralend from the file

	// read central directory
	centralend = (zipCentralEnd*) (cebuff + centralendpos);
	central = (char *)malloc( centralend->size );
	file.Seek( centralend->offset );
	file.Read( central, centralend->size );
	file.Close();

	int	offset = 0;
	for(i = 0; i < centralend->entries; i++)
	{	
		fh = (zipFileHeader*)( central + i*sizeof(zipFileHeader) + offset );
		
		if( fh->sign != ZIP_FILE_HEADER_SIGN)
		{
			free(cebuff); free(central); return FALSE;
		}
		
		char fname[MAX_PATH];
		int sizename = MIN(fh->sizename, MAX_PATH - 1);
		strncpy( fname, central + (i+1)*sizeof(zipFileHeader) + offset, sizename );
		fname[sizename] = 0;

		offset += fh->sizename;
		offset += fh->sizecomment + fh->sizextra;			

		// process fname to lower and '\'
		for(int j=0; j<sizename; j++)
		{
			if(fname[j]=='/') fname[j]='\\';
			fname[j] = tolower(fname[j]);
		}
		
		// create file info entry - filters may be applied here
		fi = new f9ZipFileInfo();
		fi->m_name		= fname;
		fi->m_offset	= fh->offset;
		fi->m_size		= fh->sizeuncomp;
		int idx = m_fat.size();
		m_fat.push_back(fi);

		// add to hash
		index.insert(Hash::value_type(fi->m_name, idx));
	}
		
	free( cebuff );
	free( central );
	
	return TRUE;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
