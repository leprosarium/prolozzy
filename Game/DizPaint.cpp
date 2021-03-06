//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPaint.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizPaint.h"
#include "DizGame.h"
#include "DizDebug.h"
#include "R9ImgLoader.h"

DizPaint g_paint;

PREDICATE_M(tile, find, 2)
{
	return A2 = g_paint.tiles.Find(A1); 
}

PREDICATE_M(tile, count, 1)
{
	return A1 = static_cast<int>(g_paint.tiles.size());
}

PREDICATE_M(tile, load, 1)
{
	return g_paint.tiles.Load(A1);
}

PREDICATE_M(tile, unload, 0)
{
	g_paint.tiles.clear();
	return true;
}

PREDICATE_M(tile, id, 2) 
{
	if(Tile* tile = g_paint.tiles.Get(A1))
		return A2 = tile->id;
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(tile, width, 2) 
{
	if(Tile* tile = g_paint.tiles.Get(A1))
		return A2 = tile->GetWidth();
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(tile, height, 2) 
{
	if(Tile* tile = g_paint.tiles.Get(A1))
		return A2 = tile->GetHeight();
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(tile, frames, 2) 
{
	if(Tile* tile = g_paint.tiles.Get(A1))
		return A2 = tile->frames;
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(tile, name, 2) 
{
	if(Tile* tile = g_paint.tiles.Get(A1))
		return A2 = tile->name;
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(font, load, 1)
{
	return g_paint.fonts.Load(A1);
}

PREDICATE_M(font, unload, 0)
{
	g_paint.fonts.clear();
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// HUD
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(hud, drawTile, 5)
{
	PlTerm r1 = A2;
	PlTerm r2 = A3;
	iV2 p1(r1[1], r1[2]);
	iV2 p2(r2[1], r2[2]);
	g_paint.hud.DrawTile(A1,
		iRect(p1, p1 + iV2(r1[3], r1[4])),
		iRect(p2, p2 + iV2(r2[3], r2[4])),
		static_cast<int>(A4), A5 );
	return true;
}

PREDICATE_M(hud, drawText, 4)
{
	PlTerm r = A2;
	iV2 p(r[1], r[2]);
	g_paint.hud.DrawText( A1, iRect(p, p + iV2(r[3], r[4])), A3, A4 );
	return true;
}

PREDICATE_M(hud, getTextWidth, 2)
{
	int w,h,c,r;
	g_paint.hud.GetTextSize(A1, w, h, c, r);
	return A2 = w;
}


PREDICATE_M(hud, getTextHeight, 2)
{
	int w,h,c,r;
	g_paint.hud.GetTextSize(A1, w, h, c, r);
	return A2 = h;
}

PREDICATE_M(hud, getTextColumns, 2)
{
	int w,h,c,r;
	g_paint.hud.GetTextSize(A1, w, h, c, r);
	return A2 = c;
}

PREDICATE_M(hud, getTextRows, 2)
{
	int w,h,c,r;
	g_paint.hud.GetTextSize(A1, w, h, c, r);
	return A2 = r;
}

PREDICATE_M(hud, clipping, 1)
{
	PlTerm r = A1;	
	iRect dst( r[0], r[1], r[2], r[3] );
	dst.p2 += dst.p1;
	g_paint.hud.SetClipping( dst );
	return true;
}

PREDICATE_M(hud, font, 1)
{
	g_paint.hud.font = A1;
	return true;
}

PREDICATE_M(hud, shader, 1)
{
	g_paint.hud.shader = BlendAtom().Get(A1);
	return true;
}

PREDICATE_M(hud, color, 1)
{
	int64 v;
	if(!PL_get_int64(A1, &v))
		return false;
	g_paint.hud.color = static_cast<dword>(v);
	return true;
}

bool Tiles::Reacquire()
{
	elog::app() << "Paint reaquire." << std::endl;
	bool ok=true;
	for(Tile & tile: *this) 
	{
		tile.tex = R9_TextureLoad(tile.name);
		if(!tile.tex)
		{
			elog::sys() << "error reacquireing tile " << tile.name.c_str() << std::endl;
			ok = false;
		}
	}
	return ok;
}

void Tiles::Unacquire()
{
	for(Tile & t: *this) t.Destroy();
}
void DizPaint::Unacquire()
{
	elog::app() << "Paint unaquire." << std::endl;
	tiles.Unacquire();
	fonts.Unacquire();
}


void DizPaint::Layout()
{
	scale = g_cfg.m_scale;
	if(!scale)	scale = std::max(0, std::min(R9_GetWidth()/g_game.screenSize.x, R9_GetHeight()/g_game.screenSize.y));
	scrOffs = g_dizdebug.visible() ? iV2() : (iV2(R9_GetWidth(), R9_GetHeight()) - g_game.screenSize * scale ) / 2;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// TILES
//////////////////////////////////////////////////////////////////////////////////////////////////
bool Tiles::LoadFile( const std::wstring & filepath, size_t & total, size_t & fail, size_t & duplicates)
{
	
	// check file type (not counted if unaccepted); only TGA and PNG files accepted
	std::wstring ext = file_path2ext(filepath);
	if(ext != L"tga" && ext != L"png") return false;
	std::wistringstream name(file_path2name(filepath));
	
	total++;
	int id;
	if(!(name >> id))
	{
		fail++; 
		elog::sys() << "! " << filepath << " (bad name)" << std::endl; 
		return false; 
	}

	std::wstring szt;
	int frames	= 1;
	name >> szt >> frames;

	int fpl;
	if(! (name >> fpl))
		fpl = frames;
	
	// check unique id
	if(Find(id) != -1)
	{
		fail++;
		duplicates++;
		elog::sys() << "! " << filepath << " (duplicate id)" << std::endl;
		return false;
	}

	// load image rgba
	r9Img img;
	if(!R9_ImgLoadFile(filepath, &img))
	{
		fail++;
		elog::sys() << "! " << filepath << " (load failed)" << std::endl;
		return false;
	}

	// create img alpha
	r9Img imga;
	imga.m_pf = R9_PF_A;
	imga.m_width = img.m_width;
	imga.m_height = img.m_height;
	if( !R9_ImgCreate(&imga) ||
		!R9_ImgBitBltSafe(&img,0,0,img.m_width,img.m_height,&imga,0,0) ) 
	{
		fail++;
		elog::sys() << "! " << filepath << " (alpha failed)" << std::endl;
		R9_ImgDestroy(&img);
		return false;
	}

	R9TEXTURE tex = R9_TextureCreate(&img);
	if(!tex)
	{
		fail++;
		elog::sys() << "! " << filepath << " (texture failed)" << std::endl;
		return false;
	}
	Tile tile(id);
	tile.tex = tex;
	tile.frames = frames;
	tile.fx = fpl;
	tile.fy = frames / fpl;
	if(frames % fpl)
		tile.fy += 1;
	tile.img = std::move(imga);
	tile.name = filepath;

	Add(id, std::move(tile));

	R9_ImgDestroy(&img);

	if(g_dizdebug.active()) // log for developers
		elog::app() << "  " << filepath << " [" << frames << "]" << std::endl;
	
	return true;
}



bool Tiles::Load(const std::wstring & path)
{
	elog::app() << "Loading tiles from \"" << path  << "\"" << std::endl;


	// init
	size_t total = 0;
	size_t fail = 0;
	size_t duplicates = 0;

	files->FindFiles(path, [this, &total, &fail, &duplicates](const std::wstring & filepath) { LoadFile(filepath, total, fail, duplicates); } );

	// report
	elog::app() << "Tiles report: total=" << total << ", failed=" << fail << " (duplicates=" << duplicates << ")" << std::endl << std::endl;

	return true;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// Draw functions
//////////////////////////////////////////////////////////////////////////////////////////////////

void DizPaint::DrawTile( int idx,const iV2 & p, const iRect & map, dword color, int flip, int frame, Blend blend, float sc ) const
{
	if(auto tile = tiles.Get(idx))
	{
		R9_SetBlend(blend);
		R9_DrawSprite( scrPos(p), tile->FrameRect(tile->ComputeFrameLoop(frame), map), tile->tex, color, flip, scale * sc);
	}
}
	
void DizPaint::DrawTile( int idx, const iV2 & p, dword color, int flip, int frame, Blend blend, float sc ) const
{
	if(auto tile = tiles.Get(idx))
	{
		R9_SetBlend(blend);
		R9_DrawSprite( scrPos(p), tile->FrameRect(tile->ComputeFrameLoop(frame)), tile->tex, color, flip, scale * sc);
	}
}

void DizPaint::DrawChar( int fontidx, const iV2 & p, wchar_t c, dword color ) const
{
	if(auto f = fonts.Get(fontidx))
		if(auto font = f->font)
		{
			float tsize = font->GetSize();
			font->SetSize( tsize * scale );
			font->SetColor( color );
			font->Char(scrPos(p), c);
			font->SetSize(tsize);
		}
}

DizPaint::DrawMethod DizPaint::selectDrawMethod(const Brush & brush, int idx, int frame) const
{
	iRect map = brush.map;
	dword color = brush.color;
	int flip = brush.flip;
	Blend blend = brush.shader;
	float ms = brush.mapScale();
	if(!drawtilesoft())
		return [this, idx, map, color, flip, frame, blend, ms](const iV2 & p) { DrawTile(idx, p, map, color, flip, frame, blend, ms); };
	if(Is<Flip::R>(flip) || (brush.scale != 0 && brush.scale != 100))
		return [this, idx, map, color, flip, frame, blend, ms](const iV2 & p) { DrawTileSoft2(idx, p, map, color, flip, frame, blend, ms); };
	return [this, idx, map, color, flip, frame, blend, ms](const iV2 & p) { DrawTileSoft(idx, p, map, color, flip, frame, blend, ms); };
}

void DizPaint::DrawBrush( const Brush & brush, const iV2 & p0, int frame ) const
{
	int idx = tiles.Find(brush.tile);
	if(idx==-1) return;

	iV2 msz = brush.mapSize();
	if( msz == 0) return;
	iV2 sz = brush.size;

	fRect oldclip = R9_GetClipping();
	iV2 p1 = scrPos(p0);
	R9_AddClipping(fRect(p1, p1 + scale * sz));
	if(R9_IsClipping())
	{
		g_game.visible_brushes++;
		iV2 c = (sz - 1) / msz;
		iV2 p = p0;
		auto draw = selectDrawMethod(brush, idx, frame);
		for(int i = 0; i <= c.y; ++i, p.y+=msz.y)
		{
			p.x = p0.x;
			for(int j = 0;j<=c.x;j++, p.x+=msz.x)
				draw(p);
		}
	}

	R9_SetClipping(oldclip);
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// TILE MATERIAL DRAW (SOFTWARE)
// color and shader ignored
// use imgtarget, drawtilemat and correct clipping
// rotated and scalled sprites go through DrawTileSoft2
//////////////////////////////////////////////////////////////////////////////////////////////////

void DizPaint::BeginSoftwareRendering(const iV2 & size, dword cap, byte * data)
{
	rollback.scale = scale;
	rollback.offs = scrOffs;
	rollback.clip = R9_GetClipping(); 

	scrOffs = 0;
	scale = 1;
	_drawtilesoft = true;
	_imgtarget.m_pf = R9_PF_A;
	_imgtarget.m_width = size.x;
	_imgtarget.m_height = size.y;
	_imgtarget.m_size = cap;
	_imgtarget.m_data = data;
}

void DizPaint::EndSoftwareRendering()
{
	_drawtilesoft = false;
	scrOffs = rollback.offs;
	scale = rollback.scale;
	R9_SetClipping(rollback.clip);
}


void DizPaint::DrawTileSoft( int idx, const iV2 & p, const iRect & map, dword color, int flip, int frame, Blend blend, float scale ) const
{
	auto tile = tiles.Get(idx); 
	if(tile == nullptr) return;
	iV2 sz = tile->GetSize();
	// assert(frame==0);
	frame = tile->ComputeFrameLoop(frame);
	iV2 f = tile->GetF(frame);
	// source rectangle safe
	iRect rsrc = map;
	rsrc.Offset(f * sz).Clip(iRect(iV2(), tile->TexSize()));

	// destination rectangle
	iRect rdst(p, p + rsrc.Size());

	// clip by clipping (clipping must be already clipped by imgtarget)
	fRect frdst = rdst;
	fRect frsrc = rsrc;
	R9_ClipSprite(frdst,frsrc,flip); // must always clip !!!
	rdst = frdst;
	rsrc = frsrc;

	if(!rdst.Ordered()) return; // out-clipped

	// prepare blit
	int dstw = _imgtarget.m_width;
	int dsth = _imgtarget.m_height;
	byte* dst = _imgtarget.m_data; assert(dst!=NULL);
	dst += rdst.p1.x + rdst.p1.y * dstw; // start

	int rw = rdst.Width();
	int rh = rdst.Height();
	int srcw = tile->img.m_width;
	int srch = tile->img.m_height;
	byte* src = tile->img.m_data; assert(src!=NULL);
	src += rsrc.p1.x + rsrc.p1.y * srcw; // start
	int srcsx = 1; // step x;
	if(Is<Flip::X>(flip))
	{
		srcsx = -1;
		src += rw-1;
	}
	int srcsy = 1; // step y;
	if(Is<Flip::Y>(flip))
	{
		srcsy = -1;
		src += (rh-1) * srcw;
	}

	// draw loops
	for(int iy=0;iy<rh;iy++)
	{
		byte* src0 = src;
		for(int ix=0;ix<rw;ix++)
		{
			if(*src>128) *dst = drawtilemat;
			dst++; src+=srcsx;
		}
		dst += dstw - rw;
		src = src0 + srcw*srcsy;
	}

}

void DizPaint::DrawTileSoft2( int idx, const iV2 & p, const iRect & map, dword color, int flip, int frame, Blend blend, float scale ) const
{
	auto tile = tiles.Get(idx); 
	if(tile == nullptr) return;
	iV2 sz = tile->GetSize();
	// assert(frame==0);
	frame = tile->ComputeFrameLoop(frame);
	iV2 f = tile->GetF(frame);
	// source rectangle safe
	iRect rsrc = map;
	rsrc.Offset(f* sz).Clip(iRect(iV2(), tile->TexSize()));

	// DRAW SPRITE SOFTWARE
	bool rotated = Is<Flip::R>(flip);

	fV2 pf(p);
	fRect dst(pf, pf + fV2(rotated ? rsrc.Size().Tran() : rsrc.Size() ) * scale);
	
	fRect src(rsrc);
	if(Is<Flip::X>(flip))	{ src.p1.x=(float)rsrc.p2.x; src.p2.x=(float)rsrc.p1.x; }
	if(Is<Flip::Y>(flip))	{ src.p1.y=(float)rsrc.p2.y; src.p2.y=(float)rsrc.p1.y; }
	if(rotated)		{ fRect src1 = src; src.p1.x=src1.p2.y; src.p1.y=src1.p1.x; src.p2.x=src1.p1.y; src.p2.y=src1.p2.x; }

	R9_ClipQuad(dst,src);
	if(!dst.Ordered()) return;

	int dstw = _imgtarget.m_width;
	int dsth = _imgtarget.m_height;
	int dw = (int)dst.Width();
	int dh = (int)dst.Height();
	byte* dstdata = _imgtarget.m_data;
	dstdata += (int)dst.p1.x + (int)dst.p1.y * dstw; // start
	float sw = src.Width();
	float sh = src.Height();
	int srcw = tile->img.m_width;
	int srch = tile->img.m_height;
	byte* srcdata = tile->img.m_data;
	// draw
	for(int iy=0;iy<dh;iy++)
	{
		float ty = (float)iy / (float)dh + 0.001f;
		int my = (int)(src.p1.y + sh*ty);
		for(int ix=0;ix<dw;ix++)
		{
			float tx = (float)ix / (float)dw + 0.001f;
			int mx = (int)(src.p1.x + sw*tx);
			if(rotated)
			{
				if(srcdata[mx*srcw+my]>128) *dstdata = drawtilemat;
			}
			else
			{
				if(srcdata[my*srcw+mx]>128) *dstdata = drawtilemat;
			}
			dstdata++;
		}
		dstdata += dstw - dw;
	}

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// HUD draw functions
//////////////////////////////////////////////////////////////////////////////////////////////////

// command format can be: {a:l} or {a:left}, {a:c}, {a:r}, {c:ff0080}, {f:0}, {f:1}, {t:10 16 18}
HUD::Cmd HUD::ScanText(std::wstring::const_iterator start, std::wstring::const_iterator end, std::wstring::const_iterator & res, int* data )
{
	res = start;
	data[0]=0;

	// search for close sign
	while(res != end && *res != L'}') ++res;
	if(end<start+4 || *res != L'}' || *(start + 2) != L':') return Cmd::None; // invalid command

	// copy data string
	std::wstring szdata(start+3, res);
	switch(*(start+1))
	{
		case L'a': // align
		case L'A': // align
		{
			data[0] = (szdata[0]==L'l') ? -1 : (szdata[0]==L'r') ? 1 : 0;
			return Cmd::Align;
		}
		case L'c': // color
		case L'C': // color
		{
			std::wistringstream i(szdata);
			i >> std::hex;
			dword t;
			if(i >> t)
			{
				data[0] = static_cast<int>(t);
				return Cmd::Color;
			}
		}
		case L'f': // focus
		case L'F': // focus
		{
			data[0] = (szdata[0]==L'1') ? 1 : 0;
			return Cmd::Focus;
		}
		case L't': // tile
		case L'T': // tile
		{
			std::wistringstream i(szdata);
			if(i >> data[0] >> data[1] >> data[2]) // id, x, y (in client)
				return Cmd::Tile;
		}
	}
	return Cmd::None;
}

void HUD::GetTextSize(const std::wstring & text, int& w, int& h, int& c, int& rowcount )
{
	w = h = c = rowcount = 0;
	if (text.empty()) return;
	const Font* font = g_paint.fonts.Get(g_paint.fonts.Find(this->font));
	if(!font) return;

	auto m = text.begin();
	int data[4];
	int chrcount = 0;
	int linesize = 0;
	bool newline = true;
	const iV2 fontOfs = font->GetOfs();
	while(m != text.end())
	{
		// escape command
		decltype(m) m2;
		Cmd cmd = Cmd::None;
		if( *m == L'{')	cmd = ScanText(m, text.end(), m2, data); // read command
		if(cmd != Cmd::None)
			m = m2 + 1; // step over it
		else 
		{
			newline = *m == L'\n';
			if(newline)
			{
				if(chrcount) linesize -= fontOfs.x;
				h += font->GetSize() + fontOfs.y;
				if(linesize > w) w = linesize;
				if(chrcount > c) c = chrcount;
				chrcount = 0;
				linesize = 0;
				rowcount++;
			}
			else
			{
				linesize += font->GetCharWidth(*m) + fontOfs.x;
				chrcount++;
			}
			m++;
		}
	}
	// last row, if not empty, or after a \n
	if(chrcount || newline) 
	{
		if(chrcount) linesize -= fontOfs.x;
		h += font->GetSize();
		if(linesize > w) w = linesize;
		if(chrcount > c) c = chrcount;
		rowcount++;
	}
}

void HUD::DrawText( int tileid, const iRect & dst, const std::wstring & text, int m_align )
{
	if(!visible) return;
	if( text.empty() ) return;
	int tileidx = g_paint.tiles.Find(tileid);
	if (g_paint.tiles.InvalidIdx(tileidx)) return;
	Tile * tile = g_paint.tiles.Get(tileidx); 
	int fontidx = g_paint.fonts.Find(font); // find font
	if (g_paint.fonts.InvalidIdx(fontidx)) return;
	Font * font = g_paint.fonts.Get(fontidx);

	// overwrite font's texture and shader
	font->font->SetTexture(tile->tex);
	font->font->SetBlend(shader);

	// draw process
	iV2 p(dst.p1);
	iV2 sz = dst.Size();

	int linecount = 0;				// line count
	
	auto lnstart = text.begin();	// start of current line (chr scan pos)
	auto lnend = text.end();						// end of current line (chr scan pos)
	int align = m_align;			// lign align mode
	int focus = 0;					// focus mode 1/0
	dword clr = color;			// color
	dword colorfocus;				// color when focus=1
	dword focuscolors[8] = { 0xffc80000, 0xffc800c8, 0xff00c800, 0xff00c8c8, 0xffc8c800,  0xff00c8c8, 0xff00c800, 0xffc800c8 };
//	dword focuscolors[6] = { 0xffff8000, 0xffffa000, 0xffffc000, 0xffffe000, 0xffffc000, 0xffffa000 }; // some orange version
	colorfocus = focuscolors[ (GetTickCount()/30) % 8 ];

	// scan text
	while(lnstart != text.end())
	{
		// Current Line Processing
		p.x = dst.p1.x;

		Cmd cmd;			// escape command
		int data[4];		// escape command data

		// PASS1: scan current line for size
		auto m = lnstart;	// scan cursor
		int chrcount = 0;	// printable characters to scan cursor
		int linesize = 0;	// line size in pixels
		const iV2 fontOfs = font->GetOfs();
		while(m != text.end() && *m != L'\n')
		{
			// escape command
			decltype(m) m2;
			cmd = Cmd::None;
			if( *m == L'{') cmd = ScanText(m,text.end(), m2, data); // read command
			if(cmd != Cmd::None) // only if command found and valid
			{
				if(cmd == Cmd::Align) align = data[0];
				m = m2 + 1; // step over it
			}
			else
			{
				linesize += font->GetCharWidth(*m) + fontOfs.x;
				m++;
				chrcount++;
			}
		}
		if(chrcount>0) linesize -= fontOfs.x;

		lnend = m;

		// compute aligniament
		if(align == 0)
			p.x += ((sz.x-linesize)/2);
		else if(align == 1)
			p.x += (sz.x-linesize);

		// PASS2: print characters to the end of the line
		m = lnstart;
		while(m < lnend)
		{
			if(m == text.end()) break; // safety+

			// escape command
			decltype(m) m2;
			cmd = Cmd::None;
			if( *m == L'{')	cmd = ScanText(m, text.end(), m2, data); // read command
			if(cmd != Cmd::None) // only if command found and valid
			{
				if(cmd == Cmd::Color) clr = data[0];
				else if(cmd == Cmd::Focus) focus = data[0];
				else if(cmd == Cmd::Tile) g_paint.DrawTile( g_paint.tiles.Find(data[0]), p + iV2(data[1], data[2]), focus ? colorfocus : clr, 0, 0);
				m = m2 + 1; // step over it
			}
			else
			{
				// print character
				g_paint.DrawChar( fontidx, p, *m, focus ? colorfocus : clr );
				p.x += font->GetCharWidth(*m) + fontOfs.x;
				chrcount--;
				m++;
			}
		}

		if(m != text.end() && *m == L'\n') m++; // step over new line character

		// new Line
		p.y += font->GetSize() + fontOfs.y;
		lnstart = m;
		linecount++;
	}
}


void HUD::DrawTile( int tileid, const iRect & dst, const iRect & src, dword flags, int frame )
{
	if(!visible) return;
	int tileidx = g_paint.tiles.Find(tileid);
	if(g_paint.tiles.InvalidIdx(tileidx)) return;
	iV2 sz = src.Size();
	if( sz.x == 0 || sz.y == 0 ) return;
	fRect oldclip = R9_GetClipping();
	fRect newclip = fRect(g_paint.scrPos(dst.p1), g_paint.scrPos(dst.p2));
	R9_AddClipping(newclip);
	if(R9_IsClipping())
	{
		iV2 c = (dst.Size() + sz - 1) / sz;
		iV2 p(dst.p1);
		for(int i = 0; i < c.y; i++)
		{
			p.x = dst.p1.x;
			for(int j = 0; j < c.x; j++)
			{
				g_paint.DrawTile( tileidx, p, src, color, flags, frame, shader );
				p.x += sz.x;
			}
			p.y += sz.y;
		}
	}

	R9_SetClipping(oldclip);	
}

void HUD::SetClipping( const iRect & dst )
{
	if(visible)
		if(dst.Ordered())
			R9_SetClipping(fRect(g_paint.scrPos(dst.p1), g_paint.scrPos(dst.p2)));
		else
			R9_ResetClipping();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Fonts
//////////////////////////////////////////////////////////////////////////////////////////////////
bool Fonts::LoadFile(const std::wstring & filepath, size_t & total, size_t & fail, size_t & duplicates)
{
	if(file_path2ext(filepath) != L"fnt") return false;
	std::wistringstream name(file_path2name(filepath));
	
	total++;

	int id;
	if(!(name >> id))
	{
		fail++; 
		elog::sys() << "! " << filepath << " (bad name)" << std::endl; 
		return false; 
	}

	std::wstring szt;
	name >> szt;

	if(Find(id) !=-1)
	{
		fail++;
		duplicates++;
		elog::sys() << "! " << filepath << " (duplicate id)" << std::endl;
		return false;
	}

	// add to list
	Font font(id, new r9Font());
	font.font->Create(8,8,8,32,128);
	if(!font.font->Create(filepath))
	{
		fail++;
		elog::sys() << "! " << filepath << " (failed to load)" << std::endl;
		return false;
	}
	Add(id, std::move(font));

	if(g_dizdebug.active()) // log for developers
		elog::app() << "  " << filepath << std::endl;
	return true;
}

bool Fonts::Load(const std::wstring & path)
{
	elog::app() << "Loading fonts from \"" << path << "\"" << std::endl;

	size_t total = 0;
	size_t fail = 0;
	size_t duplicates = 0;

	files->FindFiles(path, [this, &total, &fail, &duplicates](const std::wstring & filepath) { LoadFile(filepath, total, fail, duplicates); } );

	// report
	elog::app() << "Fonts report: total=" << total << ", failed=" << fail << " (duplicates=" << duplicates << ")" << std::endl << std::endl;
	return true;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
