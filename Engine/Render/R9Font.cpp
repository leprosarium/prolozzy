///////////////////////////////////////////////////////////////////////////////////////////////////
// R9Font.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
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

bool r9Font::Create( int chrw, int chrh, int cols, int start, int count )
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
	return true;
}

bool r9Font::Create( const std::string & filename )
{
	
	F9FILE file = files->OpenFile(filename);
	if(!file) return false;
	int size = static_cast<int>(file->Size());
	if(size==0) { files->FileClose(file); return false; }
	byte * buffer = new byte[size];
	file->Read( buffer, size);
	files->FileClose(file);

	byte * buffer0 = buffer;

	// HEADER (24 bytes)

	if( !(buffer[0]=='F' && buffer[1]=='N' && buffer[2]=='T' && buffer[3]=='0') ) { delete [] buffer; return false; }
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
	
	delete [] buffer0;
	return true;
}

void r9Font::Destroy()
{
	// let the user deal with the texture...
	// if(m_tex) R9_TextureDestroy((R9TEXTURE)m_tex);		
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// sizes
///////////////////////////////////////////////////////////////////////////////////////////////////
float r9Font::GetCharWidth( char c ) const
{
	if(!IsValid(c)) return 0.0f;
	return m_scale * m_aspect * (float)m_char[(byte)c].w;
}

float r9Font::GetTextWidth( const std::string & text ) const
{
	if(text.empty()) return 0.0f;
	return GetTextWidth(text, text.size());
}

float r9Font::GetTextWidth( const std::string & text, int size ) const
{
	if(text.empty()) return 0.0f;
	return std::accumulate(text.begin(), text.begin() + size, GetItalic() + size * GetOfsX(),
		[this](float w, char c) { return w + GetCharWidth(c); });
}

fV2 r9Font::GetTextBox( const std::string & text) const
{
	fV2 sz;
	if( text.empty()) return sz;
	sz.y = m_chrh;
	float w = 0;
	for(auto c: text)
	{
		if(c=='\n')
		{
			if(w > sz.x) sz.x = w;
			w = 0;
			sz.y += m_chrh;
		}
		else
			w += GetCharWidth( c ) + GetOfsX();
	}
	if(w > sz.x) sz.x = w;
	return sz;
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

	fRect dst(p, p + fV2(GetCharWidth(c), GetSize()));
	
	fRect src((float)m_char[ci].x, (float)m_char[ci].y, (float)(m_char[ci].x + m_char[ci].w), (float)(m_char[ci].y + m_chrh));

	// clipping
	R9_ClipQuad( dst, src );
	if(!dst.Ordered()) return;

	// mapping
	fV2 tt = tex->realSize();
	src.p1 /= tt;
	src.p2 /= tt;

	R9_DrawQuad(dst, src, tex, m_color );

}

void r9Font::Print(const fV2 & start, const std::string & text )
{
	fV2 p(start);
	for(auto c: text)
		if( c=='\n' )
		{
			p.x = start.x;
			p.y += GetSize() + GetOfsY();
		}
		else 
		if( c=='\r')
		{
			p.x = start.x;
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
}

///////////////////////////////////////////////////////////////////////////////////////////////////
