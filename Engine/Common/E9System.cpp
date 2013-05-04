///////////////////////////////////////////////////////////////////////////////////////////////////
// E9System.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"

#include <sstream>

#include "E9System.h"
#include "E9Engine.h"



#ifdef E9_ENABLE_ZIP
// make sure the paths to the libs are ok
#include "zlib.h"
#ifdef _DEBUG
#pragma comment( lib, "..\\Libs\\ZLib\\zlib_d.lib" )
#else
#pragma comment( lib, "..\\Libs\\ZLib\\zlib.lib" )
#endif
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////
// System
///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL sys_senddata( HWND fromhwnd, HWND tohwnd, int cmd, int slot, int size, char* data )
{
	if(!tohwnd) return FALSE;
	COPYDATASTRUCT copystruct;
	copystruct.dwData = MAKEWORD(cmd,slot); // low,high
	copystruct.cbData = size;
	copystruct.lpData = data;
	LRESULT hr = SendMessage( tohwnd, WM_COPYDATA, (WPARAM)fromhwnd, (LPARAM)&copystruct );
	return (!FAILED(hr));
}


///////////////////////////////////////////////////////////////////////////////////////////////////
// Ini Tools
///////////////////////////////////////////////////////////////////////////////////////////////////

std::istringstream ini_get(const std::string & file, const std::string & group, const std::string & key)
{
	char sz[64]; sz[0]=0;
	std::istringstream o;
	if(GetPrivateProfileString(group.c_str(), key.c_str(), "", sz, 64, file.c_str()))
		o.str(sz);
	else
		o.setstate(std::ios::failbit);
	return o;
}

template<>
void ini_set<std::string>(const std::string & file, const std::string & group, const std::string & key, const std::string & value)
{
	WritePrivateProfileString(group.c_str(), key.c_str(), value.c_str(), file.c_str());	
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Files
///////////////////////////////////////////////////////////////////////////////////////////////////


//std::string	file_getfullpath(const std::string & file)
//{
//	static char path[MAX_PATH];
//	if(GetFullPathName(file.c_str(), MAX_PATH , path, nullptr) != 0) return path;
//	return std::string();
//}

const char* file_getfullpath( const char* file )
{
	static char path[256];
	if(GetFullPathName( file, 255, path, NULL )!=0) return path;
	return NULL;
}

const char* file_path2file(const char* path)
{
	const char* p = NULL;
	if(!path) return NULL;
	if(!(p = strrchr(path, '\\')))	return path;
	if(strlen((p + 1)) > 0)			return (p + 1);
	return NULL;
}

int	file_path2dir(const char* path)
{
	const char *p = NULL;
	if(!path || !(p = file_path2file(path))) return 0;
	return (int)(p - path);
}

const char* file_path2ext(const char* path)
{
	const char *p = NULL;
	if(!path) return NULL;
	if(!(p = strrchr(path, '.')))	return NULL;
	if(strlen((p + 1)) > 0)			return  (p + 1);
	return NULL;
}

char file_path2drive(const char* path)
{
	if( !path || path[0]==0 || path[1] != ':') return 0;
	return path[0];
}

void file_pathsplit( const char* path, char* drive, char* dir, char* fname, char* ext )
{
	_splitpath( path, drive, dir, fname, ext );
}

void file_findfiles( const char* path, file_ffcallback ffcallback, dword flags )
{
	char fpath[256];
	char spath[256];
	if(path) strcpy(spath,path); else spath[0]=0;
	strcat(spath,"*.*");

	WIN32_FIND_DATA data;
	HANDLE h = FindFirstFile( spath, &data );
	if(h==INVALID_HANDLE_VALUE) return;
	do
	{	// check
		if(data.dwFileAttributes & FILE_ATTRIBUTE_SYSTEM ) continue;

		// directory
		if( data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ) 
		{
			if( 0==strcmp(data.cFileName,"." ) ) continue;
			if( 0==strcmp(data.cFileName,".." ) ) continue;

			if(path) strcpy(fpath,path); else fpath[0]=0;
			strcat(fpath,data.cFileName);
			_strlwr(fpath);
			if( flags & FILE_FINDDIR )	ffcallback( fpath, 1 );
			strcat(fpath,"\\");
			if( flags & FILE_FINDREC )	file_findfiles( fpath, ffcallback, flags );
		}
		else // file
		{
			if(path) strcpy(fpath,path); else fpath[0]=0;
			strcat(fpath,data.cFileName);
			_strlwr(fpath);
			ffcallback( fpath, 0 );
		}

	}while(FindNextFile(h,&data));
	
	FindClose(h);
}

void file_delete( const char* path )
{
	DeleteFile(path);
}


///////////////////////////////////////////////////////////////////////////////////////////////////
// Encription
///////////////////////////////////////////////////////////////////////////////////////////////////
void encrypt_data( void* data, dword size, int key )
{
	if(data==NULL || size==0) return;
	for(dword i=0;i<size;i++)
		*((byte*)data+i) ^= key;
}

void decrypt_data( void* data, dword size, int key )
{
	if(data==NULL || size==0) return;
	encrypt_data( data, size, key ); // we use a simple xor method
}

void encrypt_data( void* data, dword size, const char* key )
{
	if(data==NULL || size==0 || key==NULL) return;
	int len = (int)strlen(key);
	for(dword i=0;i<size;i++)
		*((byte*)data+i) ^= (byte)i + (byte)key[i%len];
}

void decrypt_data( void* data, dword size, const char* key )
{
	if(data==NULL || size==0 || key==NULL) return;
	encrypt_data( data, size, key ); // we use a simple xor method
}


///////////////////////////////////////////////////////////////////////////////////////////////////
// Compression
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef E9_ENABLE_ZIP

BOOL compress_data( void* srcdata, dword srcsize, void* dstdata, dword &dstsize )
{
	assert(srcdata!=NULL && srcsize>0);
	assert(dstdata!=NULL && dstsize>=srcsize+srcsize/1000+32);
	unsigned long ss = srcsize;
	unsigned long ds = dstsize;
	if(Z_OK!=compress( (unsigned char *)dstdata, &ds, (unsigned char *)srcdata, ss ))  return FALSE;
	dstsize = ds;
	return TRUE;
}

BOOL decompress_data( void* srcdata, dword srcsize, void* dstdata, dword &dstsize )
{
	assert(srcdata!=NULL && srcsize>0);
	assert(dstdata!=NULL);
	unsigned long ss = srcsize;
	unsigned long ds = dstsize;
	if(Z_OK!=uncompress( (unsigned char *)dstdata, &ds, (unsigned char *)srcdata, ss )) return FALSE;
	dstsize = ds;
	return TRUE;
}

#else

BOOL compress_data( void* srcdata, dword srcsize, void* dstdata, dword &dstsize ) { return FALSE; }
BOOL decompress_data( void* srcdata, dword srcsize, void* dstdata, dword &dstsize ) { return FALSE; }

#endif


///////////////////////////////////////////////////////////////////////////////////////////////////
