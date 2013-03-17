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
delete(wszTgt);  
return(wstr);  
}

std::string WideStringToMultiByte(LPCWSTR wszSrc)  
{  
    int iSizeOfStr = WideCharToMultiByte(CP_ACP, 0, wszSrc, -1, NULL, 0, NULL, NULL);  
    char* szTgt = new char[iSizeOfStr];  
    if(!szTgt)  return(NULL);  
    WideCharToMultiByte(CP_ACP, 0, wszSrc, -1, szTgt, iSizeOfStr, NULL, NULL);  
    std::string str(szTgt);  
    delete(szTgt);  
    return(str);  
}  

////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////////////////////////
char* sprint(char* szFormat, ...)
{
	static char		szString[1024];
	va_list			vaArg;
	
	va_start (vaArg, szFormat);
	vsprintf (szString, szFormat, vaArg);
	va_end (vaArg);

	return szString;
}

LPWSTR swprint(LPCWSTR szFormat, ...)
{
	static WCHAR		szString[1024];
	va_list			vaArg;
	
	va_start (vaArg, szFormat);
	vswprintf (szString, szFormat, vaArg);
	va_end (vaArg);

	return szString;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Parsing utils
///////////////////////////////////////////////////////////////////////////////////////////////////
const char* parser_skipchar( const char* buffer, const char* charlist, int& parsedsize )
{
	parsedsize = 0;
	while(*buffer!=0)
	{
		int i=0;
		while(charlist[i]!=0)
		{
			if(*buffer==charlist[i])
				break;
			i++;
		}
		if(charlist[i]==0) // none matched
			return buffer;
		buffer++;
		parsedsize++;
	}
	return buffer;
}

const char* parser_skiptochar( const char* buffer, const char* charlist, int& parsedsize )
{
	parsedsize = 0;
	while(*buffer!=0)
	{
		int i=0;
		while(charlist[i]!=0)
		{
			if(*buffer==charlist[i])
				break;
			i++;
		}
		if(charlist[i]!=0) // one matched
			return buffer;
		buffer++;
		parsedsize++;
	}
	return buffer;
}

const char* parser_skipline( const char* buffer, int& parsedsize )
{
	const char* bufferstart = buffer;
	buffer = parser_skiptochar(buffer,"\n\r", parsedsize);
	buffer = parser_skipchar(buffer,"\n\r", parsedsize);
	parsedsize = (int)(intptr)(buffer-bufferstart);
	return buffer;
}


const char* parser_skipspace( const char* buffer, int& parsedsize )
{
	return parser_skipchar(buffer," \t\r\n",parsedsize);
}

const char* parser_skiptotoken( const char* buffer, const char* token, int& parsedsize )
{
	parsedsize=0;
	while(*buffer!=0)
	{
		int i=0;
		while(buffer[i]==token[i] && buffer[i]!=0 && token[i]!=0)
			i++;
		if(token[i]==0) return buffer; // got it !
		buffer++;
		parsedsize++;
	}
	return buffer;
}

BOOL parser_readtoken( const char* buffer, const char* token, int& parsedsize )
{
	parsedsize=0;
	while(*buffer==*token)
	{
		buffer++;
		token++;
		parsedsize++;
	}
	return *token==0;
}

BOOL parser_readword( const char* buffer, char* value, int valuesize, int& parsedsize )
{
	parsedsize=0;
	while(*buffer!=0 && *buffer!=' ' && *buffer!='\t' && *buffer!='\n' && *buffer!='\r' && parsedsize<valuesize-1)
	{
		*value = *buffer;
		buffer++;
		value++;
		parsedsize++;
	}
	*value=0;
	return parsedsize>0;
}

BOOL parser_readline( const char* buffer, char* value, int valuesize, int& parsedsize )
{
	int read=0;
	parsedsize=0;
	while(*buffer!=0 && *buffer!='\n' && *buffer!='\r' && read<valuesize-1)
	{
		*value=*buffer;
		buffer++;
		value++;
		read++;
		parsedsize++;
	}
	if(*buffer=='\r')
	{
		buffer++;
		parsedsize++;
	}
	if(*buffer=='\n')
	{
		buffer++;
		parsedsize++;
	}
	*value = 0;
	return parsedsize>0;
}

BOOL parser_readvarstr( const char* buffer, const char* name, char* value, int valuesize, int& parsedsize )
{
	const char* bufferstart = buffer;
	
	buffer=parser_skipspace(buffer,parsedsize);

	if(!parser_readtoken(buffer,name,parsedsize)) return FALSE;
	buffer += parsedsize;

	buffer=parser_skipspace(buffer,parsedsize);

	if(!parser_readtoken(buffer,"=",parsedsize)) return FALSE;
	buffer += parsedsize;

	buffer=parser_skipspace(buffer,parsedsize);

	parser_readline(buffer,value,valuesize,parsedsize);
	buffer += parsedsize;

	parsedsize = (int)(intptr)(buffer-bufferstart);
	return TRUE;
}

BOOL parser_readvarint( const char* buffer, const char* name, int* value, int& parsedsize )
{
	const char* bufferstart = buffer;
	
	buffer=parser_skipspace(buffer,parsedsize);

	if(!parser_readtoken(buffer,name,parsedsize)) return FALSE;
	buffer += parsedsize;

	buffer=parser_skipspace(buffer,parsedsize);

	if(!parser_readtoken(buffer,"=",parsedsize)) return FALSE;
	buffer += parsedsize;

	buffer=parser_skipspace(buffer,parsedsize);

	char sztmp[64];	sztmp[0]=0;
	parser_readword(buffer,sztmp,64,parsedsize);
	buffer += parsedsize;
	buffer=parser_skipline(buffer,parsedsize);

	int ret = sscanf(sztmp,"%i",value);
	if(ret!=1) return FALSE;

	parsedsize = (int)(intptr)(buffer-bufferstart);
	return TRUE;
}

BOOL parser_readvarfloat( const char* buffer, const char* name, float* value, int& parsedsize )
{
	const char* bufferstart = buffer;
	
	buffer=parser_skipspace(buffer,parsedsize);

	if(!parser_readtoken(buffer,name,parsedsize)) return FALSE;
	buffer += parsedsize;

	buffer=parser_skipspace(buffer,parsedsize);

	if(!parser_readtoken(buffer,"=",parsedsize)) return FALSE;
	buffer += parsedsize;

	buffer=parser_skipspace(buffer,parsedsize);

	char sztmp[64];	sztmp[0]=0;
	parser_readword(buffer,sztmp,64,parsedsize);
	buffer += parsedsize;
	buffer=parser_skipline(buffer,parsedsize);

	int ret = sscanf(sztmp,"%f",value);
	if(ret!=1) return FALSE;

	parsedsize = (int)(intptr)(buffer-bufferstart);
	return TRUE;
}

void parser_trimbackspace( char* buffer, int& pos )
{
	while(pos>0 && (buffer[pos-1]==' ' || buffer[pos-1]=='\t' || buffer[pos-1]=='\r' || buffer[pos-1]=='\n') )
		pos--;
	buffer[pos]=0;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
