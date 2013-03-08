///////////////////////////////////////////////////////////////////////////////////////////////////
// F9Archive.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "F9Archive.h"

f9Archive::f9Archive()
{
	m_type		= F9_ARCHIVE_NONE;
	m_mode		= F9_READ;
	m_open		= FALSE;
	m_name		= NULL;
	m_password	= NULL;
}

f9Archive::~f9Archive()
{
}

int f9Archive::Open( const char* name, int mode, const char* password )
{
	if( IsOpen() ) Close();
	if( name == NULL ) return F9_FAIL;

	m_name = sstrdup( name );
	m_password = sstrdup( password );
	m_mode = mode;
	m_open = TRUE;
	return F9_OK;
}

int f9Archive::Close() 
{
	if( !IsOpen() ) return F9_FAIL;
	if(m_name) { free(m_name); m_name=NULL; }
	if(m_password) { free(m_password); m_password=NULL; }
	m_open = FALSE;
	return F9_OK;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
