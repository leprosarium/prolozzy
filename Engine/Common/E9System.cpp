///////////////////////////////////////////////////////////////////////////////////////////////////
// E9System.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"

#include <sstream>

#include "E9System.h"

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
	char sz[1024]; sz[0]=0;
	std::istringstream o;
	if(GetPrivateProfileString(group.c_str(), key.c_str(), "", sz, 1024, file.c_str()))
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


std::string	file_getfullpath(const std::string & file)
{
	static char path[MAX_PATH];
	if(GetFullPathName(file.c_str(), MAX_PATH , path, nullptr) != 0) return path;
	return std::string();
}

std::string file_path2ext(const std::string	& path)
{
	std::string::size_type p = path.rfind('.');
	if(p == std::string::npos)
		return std::string();
	return path.substr(p + 1);
}

std::string file_path2name(const std::string & path)
{
	std::string nm = file_path2file(path);
	std::string::size_type p = nm.rfind('.');
	if(p == std::string::npos)
		return nm;
	return nm.substr(0, p);
}

std::string file_path2file(const std::string & path)
{
	std::string::size_type p = path.rfind('\\');
	if(p == std::string::npos)
		return path;
	return path.substr(p + 1);
}

void file_findfiles( const std::string & path, const std::string & mask, std::function<void(const std::string &, bool)> ffcallback, dword flags)
{
	std::string query = path + mask;
	WIN32_FIND_DATA data;
	HANDLE h = FindFirstFile(query.c_str(), &data);
	if(h == INVALID_HANDLE_VALUE) return;
	do
	{	
		if(data.dwFileAttributes & FILE_ATTRIBUTE_SYSTEM) continue;
		if(data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) 
		{
			if(data.cFileName[0] == '.' && (!data.cFileName[1] || data.cFileName[1] == '.' && !data.cFileName[2])) continue;

			std::string dir = path + data.cFileName;
			std::transform(dir.begin(), dir.end(), dir.begin(), ::tolower);
			if(flags & FILE_FINDDIR)
				ffcallback(dir, true);
			if(flags & FILE_FINDREC)
				file_findfiles(dir + "\\", mask, ffcallback, flags);
		}
		else
		{
			std::string file = path + data.cFileName;
			std::transform(file.begin(), file.end(), file.begin(), ::tolower);
			ffcallback(file, false);
		}

	} while(FindNextFile(h, &data));
	FindClose(h);
}


void file_delete(const std::string & path)
{
	DeleteFile(path.c_str());
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
