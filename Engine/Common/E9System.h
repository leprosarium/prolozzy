///////////////////////////////////////////////////////////////////////////////////////////////////
// E9System.h
// Wrapper for operating system specific types, defines and functions
// Each os/platform may have to change imlementations here
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __E9SYSTEM_H__
#define __E9SYSTEM_H__

#include <functional>

///////////////////////////////////////////////////////////////////////////////////////////////////
// basic types
///////////////////////////////////////////////////////////////////////////////////////////////////
typedef signed char			int8;		// 8  bit	signed integer
typedef signed short int	int16;		// 16 bit	signed integer
typedef signed 		 int	int32;		// 32 bit	signed integer
typedef __int64				int64;		// 64 bit   signed integer
typedef unsigned char		byte;		// 8  bit unsigned integer
typedef unsigned short		word;		// 16 bit unsigned integer
typedef unsigned int		dword;		// 32 bit unsigned integer
typedef unsigned __int64	qword;		// 64 bit unsigned integer
typedef int					BOOL;		// boolean data type
typedef	SIZE_T				sizet;		// size of a pointer

typedef	INT_PTR				intptr;		// pointer int
typedef	UINT_PTR			uintptr;	// pointer unsigned int
typedef	UINT_PTR			dwordptr;	// pointer dword
typedef	LONG_PTR			longptr;	// pointer long
typedef ULONG_PTR			ulongptr;	// pointer unsigned long

#ifdef	FALSE
#undef	FALSE
#endif
#define FALSE				( 0 )		// false

#ifdef	TRUE
#undef	TRUE
#endif
#define TRUE				( 1 )		// true

#ifndef HIBYTE
#define HIBYTE(w)			((byte)(((word)(w) >> 8) & 0xFF))
#endif

#ifndef LOBYTE
#define LOBYTE(w)			((byte)(w))
#endif

#ifndef HIWORD
#define HIWORD(l)			((word)(((dword)(l) >> 16) & 0xFFFF))
#endif

#ifndef LOWORD
#define LOWORD(l)			((word)(l))
#endif

#ifndef MAKEWORD
#define MAKEWORD(a,b)		((word)(((byte)(a)) | ((word)((byte)(b))) << 8))
#endif

#ifndef MAKELONG 
#define MAKELONG(a, b)		((dword)(((word)(a)) | ((dword)((word)(b))) << 16))
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////
// System
///////////////////////////////////////////////////////////////////////////////////////////////////
inline	dword		sys_gettickcount()												{ return GetTickCount(); }
inline	void		sys_sleep( dword miliseconds )									{ Sleep(miliseconds); }
inline	int			sys_msgbox( HWND hwnd, LPCWSTR text, LPCWSTR caption, dword type = MB_OK|MB_ICONEXCLAMATION ) { return MessageBoxW(IsWindow(hwnd)?hwnd:NULL,text,caption,type); }
inline	int			sys_desktopwidth()												{ return GetSystemMetrics(SM_CXSCREEN); }
inline	int			sys_desktopheight()												{ return GetSystemMetrics(SM_CYSCREEN); }
		BOOL		sys_senddata( HWND fromhwnd, HWND tohwnd, int cmd, int slot, int size, char* data );
inline	void		sys_outputdebugstring( LPCWSTR msg )						{ OutputDebugStringW( msg ); }

typedef HANDLE HSEMAPHORE;
inline	HSEMAPHORE	sys_createsemaphore( const char* name=NULL )					{ return (HSEMAPHORE)CreateSemaphore(NULL, 1, 1, name); }
inline	void		sys_destroysemaphore( HSEMAPHORE semaphore )					{ if(semaphore) CloseHandle(semaphore); }
inline	BOOL		sys_waitsemaphore( HSEMAPHORE semaphore, dword msec=INFINITE )	{ return WaitForSingleObject(semaphore,msec)==WAIT_OBJECT_0; }
inline	void		sys_releasesemaphore( HSEMAPHORE semaphore )					{ ReleaseSemaphore(semaphore,1,NULL); }


///////////////////////////////////////////////////////////////////////////////////////////////////
// Ini Tools
///////////////////////////////////////////////////////////////////////////////////////////////////

std::istringstream ini_get(const std::string & file, const std::string & group, const std::string & key);

template<class T>
void ini_set(const std::string & file, const std::string & group, const std::string & key, const T & value)
{
	std::ostringstream o;
	o << value;
	ini_set(file, group, key, o.str());
}

template<>
void ini_set<std::string>(const std::string & file, const std::string & group, const std::string & key, const std::string & value);


///////////////////////////////////////////////////////////////////////////////////////////////////
// Files
///////////////////////////////////////////////////////////////////////////////////////////////////
#define	FILE_FINDDIR		(1<<0)								// request directories list
#define	FILE_FINDREC		(1<<1)								// request to go recursive on found directories
typedef std::function<void(const char *, BOOL)> file_ffcallback;		// filepath=found file including requested path, dir=1 if filepath is a directory

const char*	file_path2file		( const char* path );
void		file_findfiles		( const char* path, file_ffcallback ffcallback, dword flags );		// path must include terminal '\'; use FINDFILE flags


std::string	file_getfullpath(const std::string & file);

std::string	file_path2name(const std::string & path);
std::string	file_path2ext(const std::string & path);

void file_delete(const std::string & path);






///////////////////////////////////////////////////////////////////////////////////////////////////
// Encription
///////////////////////////////////////////////////////////////////////////////////////////////////
void encrypt_data( void* data, dword size, int key );	// encrypt
void decrypt_data( void* data, dword size, int key );	// decrypt
void encrypt_data( void* data, dword size, const char* key );	// encrypt
void decrypt_data( void* data, dword size, const char* key );	// decrypt


///////////////////////////////////////////////////////////////////////////////////////////////////
// Compression
///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL compress_data	( void* srcdata, dword srcsize, void* dstdata, dword& dstsize ); // compress; dstsize>=srcsize+srcsize/1000+32
BOOL decompress_data( void* srcdata, dword srcsize, void* dstdata, dword& dstsize ); // decompress; dstsize must be big enought!


///////////////////////////////////////////////////////////////////////////////////////////////////
// dword colors
///////////////////////////////////////////////////////////////////////////////////////////////////
#define DWORD_BLACK			0xff000000
#define DWORD_DGREY			0xff404040
#define DWORD_GREY			0xff707070
#define DWORD_LGREY			0xffa0a0a0
#define DWORD_DRED			0xff800000
#define DWORD_RED			0xffff0000
#define DWORD_LRED			0xffff4000
#define DWORD_DGREEN		0xff005000
#define DWORD_GREEN			0xff00a000
#define DWORD_LGREEN		0xff00ff00
#define DWORD_DBLUE			0xff000080
#define DWORD_BLUE			0xff0000ff
#define DWORD_LBLUE			0xff0040ff
#define DWORD_YELLOW		0xffff0000
#define DWORD_MAGENTA		0xffff00ff
#define DWORD_CYAN			0xff00ffff
#define DWORD_ORANGE		0xffff8000
#define DWORD_WHITE			0xffffffff


///////////////////////////////////////////////////////////////////////////////////////////////////
// Compatibility tricks
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef IDC_HAND	// missing on VC6
#define IDC_HAND	MAKEINTRESOURCE(32649)
#endif

#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL 0x020A
#endif

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////