///////////////////////////////////////////////////////////////////////////////////////////////////
// F9Archive.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9Archive.h"

f9Archive::f9Archive(int type) : type(type), m_mode(F9_READ), m_open()
{
}

f9Archive::~f9Archive()
{
	Close();
}

bool f9Archive::Open( const std::wstring & name, int mode, const std::wstring & password)
{
	if( IsOpen() ) Close();

	m_name = name;
	m_password = password;
	m_mode = mode;
	m_open = true;
	return true;
}

bool f9Archive::Close() 
{
	if( !IsOpen() ) return false;
	m_open = false;
	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
