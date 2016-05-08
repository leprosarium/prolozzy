///////////////////////////////////////////////////////////////////////////////////////////////////
// R9Font.h
//
// file format: FNT0 - 040723
//
// size		member		value / meening
//
// HEADER	(pos 0, 24 bytes)
// 4		ID			FNT0
// 2		chrw		character max width
// 2		chrh		character max height (font size)
// 2		ofsx		character offset on x (added between characters) (0)
// 2		ofsy		character offset on y (added between rows) (0)
// 2		texw		texture width (NOT NEEDED)
// 2		texh		texture height (NOT NEEDED)
// 2		scale		scale percentage % (100)
// 2		aspect		aspect percentage % (widths are multiplied by this) (100)
// 2		italic		italic shift width (0)
// 2		empty		---
//
// BODY		( pos 24, size to the end of file, each entry is 6 bytes )
// 1		char		character [0..255]
// 2		x			cel x position on texture
// 2		y			cel y position on texture
// 1		w			character real width
//
// Obs: characters above 128 may have problems because of the char sign if used >> should work!
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __R9FONT_H__
#define __R9FONT_H__

#include "E9System.h"
#include "R9Render.h"

///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
class r9Font  
{	
	struct Char
	{
		word x,y;		// cel pos
		word w;			// cel width
	};

	word		m_chrw;			// character max width
	word		m_chrh;			// character max height (font size)
	short int	m_ofsx;			// character offset on x (added between characters) (0)
	short int	m_ofsy;			// character offset on y (added between rows) (0)
	float		m_scale;		// general scale [0..1]
	float		m_aspect;		// aspect (widths are multiplied by this) [0..1]
	word		m_italic;		// italic shift width (0)
	dword		m_color;		// color (0xffffffff)
	Blend		m_blend;		// blend state
	R9TEXTURE	m_tex;			// texture
	Char		m_char[65536];	// chars mapping
public:
	r9Font();
// load
	bool Create( int chrw, int chrh, int cols=16, int start=32, int count=96 ); // create from a table with given cel size and columns (fixed char size)
	bool Create( const std::wstring & fontfile );				// create from a .fnt file

// config
	void SetTexture(R9TEXTURE texture) { m_tex = texture; }
	R9TEXTURE GetTexture() const { return m_tex; }
	void SetBlend( Blend blend ) { m_blend = blend; }
	Blend GetBlend() const { return m_blend; }
	void SetColor( dword color ) { m_color = color; }
	void SetSize( float size ) { m_scale = size / m_chrh; }
	void SetSpace( int width ) { m_char[32].w = width; }
	void SetScale(float s) { m_scale = s; }
	bool IsValid( wchar_t c ) const { return ( m_char[c].w > 0 ); }

	dword GetColor() const { return m_color; }
	float GetScale() const { return m_scale; }
	float GetScaleW() const { return GetScale() * m_aspect; }
	float GetSize() const { return GetScale() * m_chrh; }
	float GetOfsX() const { return GetScaleW() * m_ofsx; }
	float GetOfsY() const { return GetScale() * m_ofsy; }
	float GetItalic() const { return GetScaleW() * m_italic; }

// sizes
	float GetCharWidth() const { return GetScaleW() * m_chrw; }
	float GetCharWidth(wchar_t c) const;								// gets the current width of a char (in pixels) - italic not included
	float GetTextWidth(const std::wstring & text) const;				// gets the current width of a string (in pixels) - italic included, newlines ignored
	float GetTextWidth(const std::wstring & text, int size ) const;	// gets the current width of a string (in pixels) - italic included, newlines ignored
	fV2 GetTextBox(const std::wstring & text) const;				// gets the box sizes the text fits in; italic and newlines included

// draw
	void Char(const fV2 & p, wchar_t c);						// draw a single char at point p
	void Print(const fV2 & p, const std::wstring & text);	// draw a text at (x,y)
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
