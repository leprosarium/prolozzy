///////////////////////////////////////////////////////////////////////////////////////////////////
// E9String.h
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __E9STRING_H__
#define __E9STRING_H__

#include "E9System.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers
///////////////////////////////////////////////////////////////////////////////////////////////////

std::string WideStringToMultiByte(LPCWSTR wszSrc);  
std::wstring MultiByteToWideString(LPCSTR szSrc);

inline char* sstrdup( const char* sz )
{
	if(sz==NULL) return NULL;
	char* szdest = new char[(int)strlen(sz)+1]; 
	if(!szdest) return NULL;
	strcpy(szdest,sz);
	return szdest;
}

const std::string WHITESPACE = " \n\r\t";
inline std::string ltrim(const std::string & s)
{
    auto start = s.find_first_not_of(WHITESPACE);
    return start == std::string::npos ? "" : s.substr(start);
}

inline std::string rtrim(const std::string & s)
{
    auto end = s.find_last_not_of(WHITESPACE);
    return end == std::string::npos ? "" : s.substr(0, end + 1);
}
inline std::string trim(const std::string & s)
{
    return rtrim(ltrim(s));
}

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
