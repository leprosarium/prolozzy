///////////////////////////////////////////////////////////////////////////////////////////////////
// R9Font.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "D9Debug.h"
#include "R9Font.h"
#include "R9Render.h"
#include "F9Files.h"

#ifndef TAB_SIZE
#define TAB_SIZE	4
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////
// construction
///////////////////////////////////////////////////////////////////////////////////////////////////
r9Font::r9Font()
{
	m_chrw		= 0;
	m_chrh		= 0;
	m_ofsx		= 0;
	m_ofsy		= 0;
	m_texw		= 0;
	m_texh		= 0;
	m_scale		= 1.0f;
	m_aspect	= 1.0f;
	m_italic	= 0;
	m_color		= 0xffffffff;
	m_blend		= Blend::AlphaRep;
	m_tex		= NULL;
	memset( &m_char, 0, sizeof(m_char) );
}

r9Font::~r9Font()
{
}

BOOL r9Font::Create( int chrw, int chrh, int cols, int start, int count )
{
	m_chrw		= chrw;
	m_chrh		= chrh;
	m_ofsx		= 0;
	m_ofsy		= 0;
	m_texw		= 0; // unused
	m_texh		= 0; // unused
	m_scale		= 1.0f;
	m_aspect	= 1.0f;
	m_italic	= 0;
	int col=0;
	int row=0;
	for(int i=start;i<start+count;i++)
	{
		byte ci			= (byte)i;
		m_char[ci].x	= col*chrw;
		m_char[ci].y	= row*chrh;
		m_char[ci].w	= chrw;
		col++;
		if(col>=cols) { col=0; row++; }
	}
	return TRUE;
}

BOOL r9Font::Create( const char* filename )
{
	
	byte* buffer;
	F9FILE file = F9_FileOpen( filename );
	if(!file) return FALSE;
	int	size = F9_FileSize(file);
	if(size==0) { F9_FileClose(file); return FALSE; }
	buffer = (byte*)malloc( size );
	F9_FileRead( buffer, size, file );
	F9_FileClose(file);

	byte* buffer0 = buffer;

	// HEADER (24 bytes)

	if( !(buffer[0]=='F' && buffer[1]=='N' && buffer[2]=='T' && buffer[3]=='0') ) { free(buffer); return FALSE; }
	buffer += 4;

	m_chrw		= *((word*)buffer);			buffer += sizeof(word);
	m_chrh		= *((word*)buffer);			buffer += sizeof(word);
	m_ofsx		= *((short int*)buffer);	buffer += sizeof(short int);
	m_ofsy		= *((short int*)buffer);	buffer += sizeof(short int);
	m_texw		= *((word*)buffer);			buffer += sizeof(word);
	m_texh		= *((word*)buffer);			buffer += sizeof(word);
	m_scale		= *((word*)buffer);			buffer += sizeof(word); m_scale /= 100.0f;
	m_aspect	= *((word*)buffer);			buffer += sizeof(word);	m_aspect /= 100.0f;
	m_italic	= *((word*)buffer);			buffer += sizeof(word);

	buffer += 2; // empty up to pos 24
	
	// DATA
	int charsize = 1+2+2+1; // char data size

	while( buffer-buffer0 <= size - charsize )
	{
		byte ci			= *((byte*)buffer); buffer += sizeof(byte);
		m_char[ci].x	= *((word*)buffer); buffer += sizeof(word);
		m_char[ci].y	= *((word*)buffer); buffer += sizeof(word);
		m_char[ci].w	= *((byte*)buffer); buffer += sizeof(byte);
	}
	
	free(buffer0);
	return TRUE;
}

void r9Font::Destroy()
{
	// let the user deal with the texture...
	// if(m_tex) R9_TextureDestroy((R9TEXTURE)m_tex);		
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// sizes
///////////////////////////////////////////////////////////////////////////////////////////////////
float r9Font::GetCharWidth( char c )
{
	if(!IsValid(c)) return 0.0f;
	return m_scale * m_aspect * (float)m_char[(byte)c].w;
}

float r9Font::GetTextWidth( const char* text )
{
	if( text==NULL ) return 0.0f;
	return GetTextWidth( text, (int)strlen(text) );
}

float r9Font::GetTextWidth( const char* text, int size )
{
	if( text==NULL ) return 0.0f;
	int i;
	float w = 0.0f;
	for( i=0; i<size; i++ )
	{
		w += GetCharWidth(text[i]) + GetOfsX();
	}
	return w + GetItalic();
}

void r9Font::GetTextBox( const char* text, float& w, float& h )
{
	w=0; h=0;
	if( text==NULL || text[0]==0) return;
	int size = (int)strlen(text);
	h = m_chrh;
	float wi=0;
	for( int i=0; i<size; i++ )
	{
		if(text[i]=='\n')
		{
			if(wi>w) w=wi;
			wi=0; h+=m_chrh;
		}
		else
			wi += GetCharWidth( text[i] ) + GetOfsX();
	}
	if(wi>w) w=wi;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// draw
///////////////////////////////////////////////////////////////////////////////////////////////////

void r9Font::Char( const fV2 & p, char c )
{
	assert(R9_IsReady());
	if( !IsValid(c) ) return;
	if( !m_tex ) return;
	R9TEXTURE tex = (R9TEXTURE)m_tex;

	if(c==32) return; // don't draw space !

	byte ci = (byte)c;

	fRect dst;
	dst.x1 = p.x;
	dst.y1 = p.y;
	dst.x2 = p.x + GetCharWidth(c);
	dst.y2 = p.y + GetSize();
	
	fRect src;
	src.x1 = (float)m_char[ci].x;
	src.y1 = (float)m_char[ci].y;
	src.x2 = (float)(m_char[ci].x + m_char[ci].w);
	src.y2 = (float)(m_char[ci].y + m_chrh);

	// clipping
	R9_ClipQuad( dst, src );
	if(dst.x2<=dst.x1 || dst.y2<=dst.y1) return;

	// mapping
	float tw = (float)R9_TextureGetRealWidth(tex);
	float th = (float)R9_TextureGetRealHeight(tex);
	src.x1 /= tw;
	src.x2 /= tw;
	src.y1 /= th;
	src.y2 /= th;

	R9_DrawQuad(dst, src, tex, m_color );

}

void r9Font::Print( float x, float y, const char* text )
{
	fV2 p(x, y);
	if( text==NULL ) return;

	while( *text!=0 )
	{
		char c = *text;
		if( c=='\n' )
		{
			p.x = x;
			p.y += GetSize() + GetOfsY();
		}
		else 
		if( c=='\r')
		{
			p.x = x;
		}
		else
		if( c=='\t')
		{
			p.x += (GetCharWidth(32) + GetOfsX()) * TAB_SIZE;
		}
		else
		if( IsValid(c) ) 
		{
			Char(p, c);
			p.x += GetCharWidth(c) + GetOfsX();
		}
		text++;
	}

}

void r9Font::Printn( float x, float y, char* text, int size )
{
	if( text==NULL ) return;
	char c = text[size];
	text[size] = 0;
	Print(x,y,text);
	text[size] = c;
}

void r9Font::Printf( float x, float y, char* format, ... )
{
	if( format==NULL ) return;

	va_list		va;
	char		msg[1024]; // @WARNING!!!!
	va_start(va, format);
	vsprintf(msg, format, va);
	va_end(va);

	Print(x, y, msg);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// utils
///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL r9Font::Check( const char* text, float size, int& len )
{

	float textsize = 0;
	int sppos = -1; // space positioning
	len = 0;
	size -= GetItalic(); // use the italic factor

	while(text[len]!=0)
	{
		if(text[len]==' ') sppos = len;
		textsize += GetCharWidth(text[len])+GetOfsX();
		if(textsize>size) 
		{
			if(sppos!=-1) len=sppos; // break at last space
			return FALSE;
		}
		len++;
	}

	return TRUE;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
