///////////////////////////////////////////////////////////////////////////////////////////////////
// E9String.h
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __E9STRING_H__
#define __E9STRING_H__

#include "E9System.h"
#include "D9Debug.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers
///////////////////////////////////////////////////////////////////////////////////////////////////
inline char* sstrdup( const char* sz )
{
	if(sz==NULL) return NULL;
	char* szdest = (char*)malloc( (int)strlen(sz)+1 ); 
	if(!szdest) return NULL;
	strcpy(szdest,sz);
	return szdest;
}

char* sprint(char* szFormat, ...);
LPWSTR swprint(LPCWSTR szFormat, ...);

std::string WideStringToMultiByte(const wchar_t* wszSrc);  
std::wstring    MultiByteToWideString(const char* szSrc);

///////////////////////////////////////////////////////////////////////////////////////////////////
// Parsing utils
///////////////////////////////////////////////////////////////////////////////////////////////////
const char*	parser_skipchar( const char* buffer, const char* charlist, int& parsedsize );		// skip any of those chars
const char*	parser_skiptochar( const char* buffer, const char* charlist, int& parsedsize );		// skip until one of those chars
const char*	parser_skipline( const char* buffer, int& parsedsize );
const char*	parser_skipspace( const char* buffer, int& parsedsize );
const char*	parser_skiptotoken( const char* buffer, const char* token, int& parsedsize );		// skip all until token find
BOOL	parser_readtoken( const char* buffer, const char* token, int& parsedsize );
BOOL	parser_readword( const char* buffer, char* value, int valuesize, int& parsedsize );
BOOL	parser_readline( const char* buffer, char* value, int valuesize, int& parsedsize );
BOOL	parser_readvarstr( const char* buffer, const char* name, char* value, int valuesize, int& parsedsize );
BOOL	parser_readvarint( const char* buffer, const char* name, int* value, int& parsedsize );
BOOL	parser_readvarfloat( const char* buffer, const char* name, float* value, int& parsedsize );
void	parser_trimbackspace( char* buffer, int& pos );							// cuts end spaces, and updates pos (pos=strlen(buffer))

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
