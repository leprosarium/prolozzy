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

std::wistringstream ini_get(const std::wstring & file, const std::wstring & group, const std::wstring & key)
{
	wchar_t sz[1024]; sz[0]=0;
	std::wistringstream o;
	if(GetPrivateProfileStringW(group.c_str(), key.c_str(), L"", sz, 1024, file.c_str()))
		o.str(sz);
	else
		o.setstate(std::ios::failbit);
	return o;
}

template<>
void ini_set(const std::wstring & file, const std::wstring & group, const std::wstring & key, const std::wstring & value)
{
	WritePrivateProfileStringW(group.c_str(), key.c_str(), value.c_str(), file.c_str());	
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Files
///////////////////////////////////////////////////////////////////////////////////////////////////


std::wstring file_getfullpath(const std::wstring & file)
{
	static wchar_t path[MAX_PATH];
	if(GetFullPathNameW(file.c_str(), MAX_PATH , path, nullptr) != 0) return path;
	return std::wstring();
}

std::wstring file_path2ext(const std::wstring & path)
{
	std::wstring::size_type p = path.rfind('.');
	if(p == std::wstring::npos)
		return std::wstring();
	return path.substr(p + 1);
}

std::wstring file_path2name(const std::wstring & path)
{
	std::wstring nm = file_path2file(path);
	std::wstring::size_type p = nm.rfind('.');
	if(p == std::wstring::npos)
		return nm;
	return nm.substr(0, p);
}

std::wstring file_path2file(const std::wstring & path)
{
	std::wstring::size_type p = path.rfind(L'\\');
	if(p == std::wstring::npos)
		return path;
	return path.substr(p + 1);
}

void file_findfiles( const std::wstring & path, const std::wstring & mask, std::function<void(const std::wstring &, bool)> ffcallback, dword flags)
{
	std::wstring query = path + mask;
	WIN32_FIND_DATAW data;
	HANDLE h = FindFirstFileW(query.c_str(), &data);
	if(h == INVALID_HANDLE_VALUE) return;
	do
	{	
		if(data.dwFileAttributes & FILE_ATTRIBUTE_SYSTEM) continue;
		if(data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) 
		{
			if(data.cFileName[0] == '.' && (!data.cFileName[1] || data.cFileName[1] == '.' && !data.cFileName[2])) continue;

			std::wstring dir = path + data.cFileName;
			std::transform(dir.begin(), dir.end(), dir.begin(), ::tolower);
			if(flags & FILE_FINDDIR)
				ffcallback(dir, true);
			if(flags & FILE_FINDREC)
				file_findfiles(dir + L"\\", mask, ffcallback, flags);
		}
		else
		{
			std::wstring file = path + data.cFileName;
			std::transform(file.begin(), file.end(), file.begin(), ::tolower);
			ffcallback(file, false);
		}

	} while(FindNextFileW(h, &data));
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
	if(!data || !size) return;
	for(dword i=0;i<size;i++)
		*((byte*)data+i) ^= key;
}

void decrypt_data( void* data, dword size, int key )
{
	encrypt_data( data, size, key ); // we use a simple xor method
}

void encrypt_data( void* data, dword size, const std::string & key )
{
	if(!data || !size || key.empty()) return;
	int len = key.size();
	for(dword i=0;i<size;i++)
		*((byte*)data+i) ^= (byte)i + (byte)key[i%len];
}

void decrypt_data( void* data, dword size, const std::string & key )
{
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
