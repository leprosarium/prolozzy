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

int f9Archive::Open( const std::string & name, int mode, const std::string & password)
{
	if( IsOpen() ) Close();

	m_name = name;
	m_password = password;
	m_mode = mode;
	m_open = true;
	return F9_OK;
}

int f9Archive::Close() 
{
	if( !IsOpen() ) return F9_FAIL;
	m_open = false;
	return F9_OK;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
