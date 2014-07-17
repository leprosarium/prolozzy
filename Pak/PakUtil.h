//////////////////////////////////////////////////////////////////////////////////////////////////
// PakUtil.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __PAKUTIL_H__
#define __PAKUTIL_H__

//////////////////////////////////////////////////////////////////////////////////////////////////
// defines
//////////////////////////////////////////////////////////////////////////////////////////////////

typedef unsigned char		byte;		// 8  bit unsigned integer
typedef unsigned short		word;		// 16 bit unsigned integer
typedef unsigned int		dword;		// 32 bit unsigned integer
typedef int					BOOL;		// boolean data type

#ifndef	FALSE
#define FALSE 0
#endif
#ifndef	TRUE
#define TRUE 1
#endif


char*	file_path2ext		( char* path );

//////////////////////////////////////////////////////////////////////////////////////////////////
// LOG and PROGRESS
//////////////////////////////////////////////////////////////////////////////////////////////////
#define LOG( text )				Log(g_hwndlog, text)
#define PROGRESSINIT( range )	ProgressInit(g_hwndprogress, range)
#define PROGRESSSTEP()			ProgressStep(g_hwndprogress);

void Log( HWND hwnd, char* text );
void CenterDialog( HWND hwnd );
void ProgressInit( HWND hwnd, int range );
void ProgressStep( HWND hwnd );

//////////////////////////////////////////////////////////////////////////////////////////////////
// InputBox
// read input text (max 255 chars); ret FALSE if closed
//////////////////////////////////////////////////////////////////////////////////////////////////
BOOL InputBox( HINSTANCE hinstance, HWND hwnd, char* title, char* msg, char* content );

//////////////////////////////////////////////////////////////////////////////////////////////////
// PACK and UNPACK
//////////////////////////////////////////////////////////////////////////////////////////////////
int		PackProceed( char* input, char* output, char* extentions, int options=1, char* password=NULL );
int		UnpackProceed( char* input, int options=1 );
void*	ReadPackedFile( char* input, char* file, dword& size );

//////////////////////////////////////////////////////////////////////////////////////////////////
// PROTECTION
//////////////////////////////////////////////////////////////////////////////////////////////////
dword	GetPasswordCode( char* password );

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////