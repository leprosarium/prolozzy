///////////////////////////////////////////////////////////////////////////////////////////////////
// E9String.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9String.h"

std::wstring    MultiByteToWideString(LPCSTR szSrc)  
{
	unsigned int iSizeOfStr = MultiByteToWideChar(CP_ACP, 0, szSrc, -1, NULL, 0);  
	wchar_t* wszTgt = new wchar_t[iSizeOfStr];  
	if(!wszTgt)    assert(0);  
	MultiByteToWideChar(CP_ACP, 0, szSrc, -1, wszTgt, iSizeOfStr);  
	std::wstring wstr(wszTgt);  
	delete wszTgt;  
	return wstr;  
}

std::string WideStringToMultiByte(LPCWSTR wszSrc)  
{  
    int iSizeOfStr = WideCharToMultiByte(CP_ACP, 0, wszSrc, -1, NULL, 0, NULL, NULL);  
    char* szTgt = new char[iSizeOfStr];  
    if(!szTgt)  assert(0);  
    WideCharToMultiByte(CP_ACP, 0, wszSrc, -1, szTgt, iSizeOfStr, NULL, NULL);  
    std::string str(szTgt);  
    delete szTgt ;  
    return str ;  
}  
