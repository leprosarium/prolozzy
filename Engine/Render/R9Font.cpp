///////////////////////////////////////////////////////////////////////////////////////////////////
// R9Font.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "R9Font.h"
#include "R9Render.h"
#include "F9Files.h"

const int TAB_SIZE = 4;

///////////////////////////////////////////////////////////////////////////////////////////////////
// construction
///////////////////////////////////////////////////////////////////////////////////////////////////
r9Font::r9Font() :
	m_chrw(),
	m_chrh(),
	m_ofsx(),
	m_ofsy(),
	m_scale(1.0f),
	m_aspect(1.0f),
	m_italic(),
	m_color(0xffffffff),
	m_blend(Blend::AlphaRep),
	m_tex()
{
	memset( &m_char, 0, sizeof(m_char) );
}

bool r9Font::Create( int chrw, int chrh, int cols, int start, int count )
{
	m_chrw		= chrw;
	m_chrh		= chrh;
	m_ofsx		= 0;
	m_ofsy		= 0;
	m_scale		= 1.0f;
	m_aspect	= 1.0f;
	m_italic	= 0;
	int col=0;
	int row=0;
	for(int i=start;i<start+count;i++)
	{
		word ci			= static_cast<word>(i);
		m_char[ci].x	= col*chrw;
		m_char[ci].y	= row*chrh;
		m_char[ci].w	= chrw;
		col++;
		if(col>=cols) { col=0; row++; }
	}
	return true;
}

template<class T>
T get(byte * & buf) 
{
	T t(*(reinterpret_cast<T*>(buf)));
	buf += sizeof(T);
	return t;
}

bool r9Font::Create( const std::wstring & filename )
{
	std::unique_ptr<f9File, std::function<void(F9FILE)>> file(files->OpenFile(filename), [](f9File * f) { files->FileClose(f);});
	if(!file) return false;
	int size = static_cast<int>(file->Size());
	if(!size) return false;
	std::unique_ptr<byte> buffer0(new byte[size]);
	file->Read(buffer0.get(), size);
	file.reset();

	byte * buffer = buffer0.get();

	// HEADER (24 bytes)

	if(get<dword>(buffer) != 0x30544e46u) return false; // "FNT0"

	m_chrw		= get<word>(buffer);
	m_chrh		= get<word>(buffer);
	m_ofsx		= get<short int>(buffer);
	m_ofsy		= get<short int>(buffer);
	get<word>(buffer);  //skip  unused
	get<word>(buffer);
	m_scale		= get<word>(buffer) / 100.0f;
	m_aspect	= get<word>(buffer) / 100.0f;
	m_italic	= get<word>(buffer);

	buffer += 2; // empty up to pos 24
	
	// DATA

	while( buffer-buffer0.get() < size)
	{
		word ci			= get<byte>(buffer);
		m_char[ci].x	= get<word>(buffer);
		m_char[ci].y	= get<word>(buffer);
		m_char[ci].w	= get<byte>(buffer);
	}
	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// sizes
///////////////////////////////////////////////////////////////////////////////////////////////////
float r9Font::GetCharWidth( wchar_t c ) const
{
	if(!IsValid(c)) return 0.0f;
	return GetScaleW() * m_char[c].w;
}

float r9Font::GetTextWidth( const std::wstring & text ) const
{
	if(text.empty()) return 0.0f;
	return GetTextWidth(text, text.size());
}

float r9Font::GetTextWidth( const std::wstring & text, int size ) const
{
	if(text.empty()) return 0.0f;
	return std::accumulate(text.begin(), text.begin() + size, GetItalic() + size * GetOfsX(),
		[this](float w, wchar_t c) { return w + GetCharWidth(c); });
}

fV2 r9Font::GetTextBox( const std::wstring & text) const
{
	fV2 sz;
	if( text.empty()) return sz;
	sz.y = m_chrh;
	float w = 0;
	for(auto c: text)
	{
		if(c==L'\n')
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

void r9Font::Char( const fV2 & p, wchar_t c )
{
	assert(R9_IsReady());
	if( !IsValid(c) ) return;
	if( !m_tex ) return;
	R9TEXTURE tex = (R9TEXTURE)m_tex;

	if(c == L' ') return; // don't draw space !

	word ci = c;

	fRect dst(p, p + fV2(GetCharWidth(c), GetSize()));
	
	fRect src(m_char[ci].x, m_char[ci].y, m_char[ci].x + m_char[ci].w, m_char[ci].y + m_chrh);

	// clipping
	R9_ClipQuad( dst, src );
	if(!dst.Ordered()) return;

	// mapping
	fV2 tt = tex->realSize();
	src.p1 /= tt;
	src.p2 /= tt;

	R9_DrawQuad(dst, src, tex, m_color );

}

void r9Font::Print(const fV2 & start, const std::wstring & text )
{
	fV2 p(start);
	for(auto c: text)
		if( c==L'\n' )
		{
			p.x = start.x;
			p.y += GetSize() + GetOfsY();
		}
		else 
		if( c==L'\r')
		{
			p.x = start.x;
		}
		else
		if( c==L'\t')
		{
			p.x += (GetCharWidth(L' ') + GetOfsX()) * TAB_SIZE;
		}
		else
		if( IsValid(c) ) 
		{
			Char(p, c);
			p.x += GetCharWidth(c) + GetOfsX();
		}
}

///////////////////////////////////////////////////////////////////////////////////////////////////
