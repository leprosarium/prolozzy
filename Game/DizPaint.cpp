//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPaint.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizPaint.h"
#include "DizGame.h"
#include "DizDebug.h"

cDizPaint g_paint;

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
	return g_paint.tiles.Load(A1, 0);
}

PREDICATE_M(tile, load, 2)
{
	return g_paint.tiles.Load(A1, A2);
}

PREDICATE_M(tile, unload, 0)
{
	g_paint.tiles.Unload(0);
	return true;
}

PREDICATE_M(tile, unload, 1)
{
	g_paint.tiles.Unload(A1);
	return true;
}

PREDICATE_M(tile, id, 2) 
{
	if(cTile* tile = g_paint.tiles.Get(A1))
		return A2 = tile->id;
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(tile, width, 2) 
{
	if(cTile* tile = g_paint.tiles.Get(A1))
		return A2 = R9_TextureGetWidth(tile->tex);
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(tile, height, 2) 
{
	if(cTile* tile = g_paint.tiles.Get(A1))
		return A2 = R9_TextureGetHeight(tile->tex);
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(tile, frames, 2) 
{
	if(cTile* tile = g_paint.tiles.Get(A1))
		return A2 = tile->frames;
	throw PlDomainError("invalid tile index", A1); 
}

PREDICATE_M(tile, name, 2) 
{
	if(cTile* tile = g_paint.tiles.Get(A1))
		return A2 = tile->name.c_str();
	throw PlDomainError("invalid tile index", A1); 
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////

cDizPaint::cDizPaint()
{
	m_scale = 1;
	m_drawtilesoft = false;
	m_drawtilemat = 0;
	m_hudfont = 0;
	m_hudshader = ShaderBlend;
	m_hudcolor = 0xffffffff;
	m_huddraw = false;
}

bool Tiles::Reacquire()
{
	dlog(LOGAPP, L"Paint reaquire.\n");
	bool ok=true;
	for(auto i = begin(), e = end(); i != e; ++i) 
	{
		cTile & tile = *i;
		tile.tex = R9_TextureLoad(tile.name.c_str());
		if(tile.tex==NULL)
		{
			dlog(LOGSYS, L"error reacquireing tile %S.\n",tile.name.c_str());
			ok = false;
		}
	}
	return ok;
}

void Tiles::Unacquire()
{
	std::for_each(begin(), end(), [](cTile & t) { t.Destroy(); });
}
void cDizPaint::Unacquire()
{
	dlog(LOGAPP, L"Paint unaquire.\n");
	tiles.Unacquire();
	fonts.Unacquire();
}


void cDizPaint::Layout()
{
	m_scale = g_cfg.m_scale;
	if(m_scale==0)	m_scale = std::max( 0, std::min( R9_GetWidth()/g_game.screenSize.x, R9_GetHeight()/g_game.screenSize.y ) );
	scrOffs = g_dizdebug.visible() ? iV2() : (iV2(R9_GetWidth(), R9_GetHeight()) - g_game.screenSize * m_scale ) / 2;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// TILES
//////////////////////////////////////////////////////////////////////////////////////////////////

int		gstile_total;			// status report on total tiles declared (load+failed)
int		gstile_fail;			// status report on tiles failed to load
int		gstile_duplicates;		// status report on id duplicates
int		gstile_group;			// current loading group

bool Tiles::LoadFile( const char* filepath, int group )
{
	
	// check file type (not counted if unaccepted); only TGA and PNG files accepted
	const char* ext = file_path2ext(filepath); if(!ext) return false;
	if( 0!=stricmp(ext,"tga") && 0!=strcmp(ext,"png") ) return false;
	const char* name = file_path2file(filepath); if(!name) return false;
	
	gstile_total++;

	// check name format
	char szt[128];
	int id		= -1;
	int frames	= 1;
	int fpl = -1;
	int ret = sscanf(name,"%i %s %i %i",&id,szt,&frames,&fpl);
	if(ret==0 || id==-1) 
	{ 
		gstile_fail++; 
		dlog(LOGSYS, L"! %S (bad name)\n", filepath); 
		return false; 
	}
	if(frames<1) frames=1;
	if(fpl < 0) fpl = frames;
	
	// check unique id
	int idx = Find(id);
	if( idx!=-1 )
	{
		gstile_fail++;
		gstile_duplicates++;
		dlog(LOGSYS, L"! %S (duplicate id)\n", filepath, id);
		return false;
	}

	// load image rgba
	r9Img img;
	if(!R9_ImgLoadFile(filepath,&img))
	{
		gstile_fail++;
		dlog(LOGSYS, L"! %S (load failed)\n", filepath);
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
		gstile_fail++;
		dlog(LOGSYS, L"! %S (alpha failed)\n", filepath);
		R9_ImgDestroy(&img);
		return false;
	}

	cTile *tile;
	if(R9TEXTURE tex = R9_TextureCreate(&img))
	{
		// create new tile
		idx = Add(id);
		tile = Get(idx);
		tile->tex = tex;
	}
	else 
	{
		gstile_fail++;
		dlog(LOGSYS, L"! %S (texture failed)\n", filepath);
		return false;
	}

	// set current options
	tile->group	= group;
	tile->frames	= frames;
	tile->fx = fpl;
	tile->fy = frames / fpl;
	if(frames % fpl)
		tile->fy += 1;
	tile->img = std::move(imga);
	tile->name	= sstrdup(filepath);

	R9_ImgDestroy(&img);

	if(g_dizdebug.active()) // log for developers
		dlog(LOGAPP, L"  %S [%i]\n", filepath, frames );
	
	return true;
}

void FFCallback_Tile( const char* filepath, BOOL dir )
{
	if(dir) return;
	bool ret = g_paint.tiles.LoadFile(filepath,gstile_group);
}

bool Tiles::Load( char* path, int group )
{
	if(!path || !path[0]) return false; // invalid path
	int szlen = (int)strlen(path);
	if(path[szlen-1]!='\\') strcat(path,"\\");
	_strlwr(path);
	dlog(LOGAPP, L"Loading tiles from \"%S\" (group=%i)\n", path, group);

	// init
	gstile_total		= 0;
	gstile_fail			= 0;
	gstile_duplicates	= 0;
	gstile_group		= group;

	// find files on disk
	int archivefiles = F9_ArchiveGetFileCount(0);
	if(archivefiles==0) // if no archive found then open from disk
	{
		file_findfiles( path, FFCallback_Tile, FILE_FINDREC );
	}
	else // if archive opened, load from it
	{
		for(int i=0;i<archivefiles;i++)
		{
			std::string filename = F9_ArchiveGetFileName(0,i);
			if(strstr(filename.c_str(),path)==filename)
			{
				FFCallback_Tile(filename.c_str(),false);
			}
		}
	}

	// report
	dlog(LOGAPP, L"Tiles report: total=%i, failed=%i (duplicates=%i)\n\n", gstile_total, gstile_fail, gstile_duplicates );

	return true;
}

void Tiles::Unload( int group )
{
	for(size_t i=0;i<size();)
		if((*this)[i].group == group)
			Del(i);
		else
			++i;
}

int Tiles::Add( int id )
{
	if(id<0) return -1; // negative ids not accepted
	if(Find(id)!=-1) return -1; // duplicate id
	push_back(cTile(id));
	int idx = size() - 1;

	Index.insert(IntIndex::value_type(id, idx));
	return idx;
}

void Tiles::Del( int idx )
{
	if(!InvalidIdx(idx))
		erase(begin() + idx);
}



//////////////////////////////////////////////////////////////////////////////////////////////////
// Draw functions
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizPaint::DrawTile( int idx,const iV2 & p, const iRect & map, dword color, int flip, int frame, int blend, float scale )
{
	if(cTile* tile = tiles.Get(idx))
	{
		R9_SetState(R9_STATE_BLEND,blend);
		R9_DrawSprite( scrPos(p), tile->FrameRect(tile->ComputeFrameLoop(frame), map), tile->tex, color, flip, static_cast<float>(m_scale * scale));
	}
}
	
void cDizPaint::DrawTile( int idx, const iV2 & p, dword color, int flip, int frame, int blend, float scale )
{
	if(cTile* tile = tiles.Get(idx))
	{
		R9_SetState(R9_STATE_BLEND,blend);
		R9_DrawSprite( scrPos(p), tile->FrameRect(tile->ComputeFrameLoop(frame)), tile->tex, color, flip, static_cast<float>(m_scale * scale));
	}
}

void cDizPaint::DrawChar( int fontidx, const iV2 & p, char c, dword color )
{
	if(cFont * f = fonts.Get(fontidx))
		if(r9Font* font = f->font)
		{
			float tsize = font->GetSize();
			font->SetSize( tsize * m_scale );
			font->SetColor( color );
			font->Char(scrPos(p), c);
			font->SetSize(tsize);
		}
}

void cDizPaint::DrawBrush( const tBrush & brush, const iV2 & p0, int frame )
{
	int idx = tiles.Find(brush.Get(BRUSH_TILE));
	if(idx==-1) return;

	iRect map = brush.map();
	iV2 msz = brush.mapSize();
	if( msz == 0) return;
	iV2 sz = brush.size();
	float ms = brush.mapScale();

	fRect oldclip = R9_GetClipping();
	iV2 p1 = scrPos(p0);
	fRect newclip(p1, p1 + m_scale * sz); 
	R9_AddClipping(newclip);
	if(R9_IsClipping())
	{
		g_game.m_visible_brushes++;
		iV2 c = (sz + msz - 1) / msz;
		int blend = brush.Get(BRUSH_SHADER);
		int bs = brush.Get(BRUSH_SCALE);
		int color = brush.Get(BRUSH_COLOR);
		int flip = brush.Get(BRUSH_FLIP);
		bool soft2 = (flip & R9_FLIPR) || (bs != 0 && bs != 100);

		iV2 p = p0;
		std::function<void()> draw;
		if(m_drawtilesoft)
			if(soft2)
				draw = [this, idx, &p, map, color, flip, frame, blend, ms]() { DrawTileSoft2(idx, p, map, color, flip, frame, blend, ms); };
			else
				draw = [this, idx, &p, map, color, flip, frame, blend, ms]() { DrawTileSoft(idx, p, map, color, flip, frame, blend, ms); };
		else 
			draw = [this, idx, &p, map, color, flip, frame, blend, ms]() { DrawTile(idx, p, map, color, flip, frame, blend, ms); };

		for(int i=0;i<c.y;i++)
		{
			p.x = p0.x;
			for(int j=0;j<c.x;j++)
			{
				draw();
				p.x+=msz.x;
			}
			p.y+=msz.y;
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
void cDizPaint::DrawTileSoft( int idx, const iV2 & p, const iRect & map, dword color, int flip, int frame, int blend, float scale )
{
	cTile* tile = tiles.Get(idx); 
	if(tile==NULL) return;
	iV2 sz = tile->GetSize();
	// assert(frame==0);
	frame = tile->ComputeFrameLoop(frame);
	bool rotated = (flip & R9_FLIPR) != FALSE;
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

	if(!(rdst.x1<rdst.x2 && rdst.y1<rdst.y2)) return; // out-clipped

	// prepare blit
	int dstw = m_imgtarget.m_width;
	int dsth = m_imgtarget.m_height;
	byte* dst = m_imgtarget.m_data; assert(dst!=NULL);
	dst += rdst.x1 + rdst.y1 * dstw; // start

	int rw = rdst.x2-rdst.x1;
	int rh = rdst.y2-rdst.y1;
	int srcw = tile->img.m_width;
	int srch = tile->img.m_height;
	byte* src = tile->img.m_data; assert(src!=NULL);
	src += rsrc.x1 + rsrc.y1 * srcw; // start
	int srcsx = 1; // step x;
	if(flip & 1)
	{
		srcsx = -1;
		src += rw-1;
	}
	int srcsy = 1; // step y;
	if(flip & 2)
	{
		srcsy = -1;
		src += (rh-1) * srcw;
	}

	// draw loops
	byte mat = m_drawtilemat;
	for(int iy=0;iy<rh;iy++)
	{
		byte* src0 = src;
		for(int ix=0;ix<rw;ix++)
		{
			if(*src>128) *dst=mat;
			dst++; src+=srcsx;
		}
		dst += dstw - rw;
		src = src0 + srcw*srcsy;
	}

}

void cDizPaint::DrawTileSoft2( int idx, const iV2 & p, const iRect & map, dword color, int flip, int frame, int blend, float scale )
{
	cTile* tile = tiles.Get(idx); 
	if(tile==NULL) return;
	iV2 sz = tile->GetSize();
	// assert(frame==0);
	frame = tile->ComputeFrameLoop(frame);
	iV2 f = tile->GetF(frame);
	// source rectangle safe
	iRect rsrc = map;
	rsrc.Offset(f* sz).Clip(iRect(iV2(), tile->TexSize()));

	// DRAW SPRITE SOFTWARE
	bool rotated = (flip & R9_FLIPR) != 0;

	fV2 pf(p);
	fRect dst(pf, pf + fV2(rotated ? rsrc.Size().Tran() : rsrc.Size() ) * scale);
	
	fRect src(rsrc);
	if(flip & R9_FLIPX)	{ src.x1=(float)rsrc.x2; src.x2=(float)rsrc.x1; }
	if(flip & R9_FLIPY)	{ src.y1=(float)rsrc.y2; src.y2=(float)rsrc.y1; }
	if(rotated)		{ fRect src1 = src; src.x1=src1.y2; src.y1=src1.x1; src.x2=src1.y1; src.y2=src1.x2; }

	R9_ClipQuad(dst,src);
	if(dst.x2<=dst.x1 || dst.y2<=dst.y1) return;

	int dstw = m_imgtarget.m_width;
	int dsth = m_imgtarget.m_height;
	int dw = (int)dst.Width();
	int dh = (int)dst.Height();
	byte* dstdata = m_imgtarget.m_data;
	dstdata += (int)dst.x1 + (int)dst.y1 * dstw; // start
	float sw = src.Width();
	float sh = src.Height();
	int srcw = tile->img.m_width;
	int srch = tile->img.m_height;
	byte* srcdata = tile->img.m_data;
	byte mat = m_drawtilemat;

	// draw
	for(int iy=0;iy<dh;iy++)
	{
		float ty = (float)iy / (float)dh + 0.001f;
		int my = (int)(src.y1 + sh*ty);
		for(int ix=0;ix<dw;ix++)
		{
			float tx = (float)ix / (float)dw + 0.001f;
			int mx = (int)(src.x1 + sw*tx);
			if(rotated)
			{
				if(srcdata[mx*srcw+my]>128) *dstdata=mat;
			}
			else
			{
				if(srcdata[my*srcw+mx]>128) *dstdata=mat;
			}
			dstdata++;
		}
		dstdata += dstw - dw;
	}

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// HUD draw functions
//////////////////////////////////////////////////////////////////////////////////////////////////
#define DLGCMD_NONE		0
#define DLGCMD_ALIGN	1
#define DLGCMD_COLOR	2
#define DLGCMD_FOCUS	3
#define DLGCMD_TILE		4

// command format can be: {a:l} or {a:left}, {a:c}, {a:r}, {c:ff0080}, {f:0}, {f:1}, {t:10 16 18}
int	cDizPaint::HUDScanText( char* text, int start, int& end, int* data )
{
	end = start;
	data[0]=0;

	// search for close sign
	while(text[end]!=0 && text[end]!='}') end++;
	if(end<start+4 || text[end]!='}' || text[start+2]!=':') return DLGCMD_NONE; // invalid command

	// copy data string
	char szdata[64];
	int szdatalen = std::min(63,end-start-3);
	strncpy( szdata, text+start+3, szdatalen );
	szdata[szdatalen]=0;
	
	switch(text[start+1])
	{
		case 'a': // align
		case 'A': // align
		{
			data[0] = (szdata[0]=='l') ? -1 : (szdata[0]=='r') ? 1 : 0;
			return DLGCMD_ALIGN;
		}
		case 'c': // color
		case 'C': // color
		{
			if(1==sscanf(szdata,"%x",&data[0]))
				return DLGCMD_COLOR;
		}
		case 'f': // focus
		case 'F': // focus
		{
			data[0] = (szdata[0]=='1') ? 1 : 0;
			return DLGCMD_FOCUS;
		}
		case 't': // tile
		case 'T': // tile
		{
			if(3==sscanf(szdata,"%i %i %i",&data[0],&data[1],&data[2])) // id, x, y (in client)
				return DLGCMD_TILE;
		}
	}

	return DLGCMD_NONE;
}

void cDizPaint::HUDGetTextSize( char* text, int& w, int& h, int& c, int& r )
{
	w = h = c = r = 0;
	if(text==NULL) return; // invalid text
	int fontidx = fonts.Find(m_hudfont); // find font
	cFont* font = fonts.Get(fontidx);
	if(!font) return; // no font

	int m=0;
	int data[4];
	h = 0;
	int chrcount = 0;
	int rowcount = 0;
	int linesize = 0;
	bool newline=true;
	while(text[m]!=0)
	{
		// escape command
		int m2;
		int cmd=DLGCMD_NONE;
		if( text[m]=='{')	cmd = HUDScanText(text,m,m2,data); // read command
		if(cmd!=DLGCMD_NONE) m=m2+1; // step over it
		else 
		{
			if(text[m]=='\n')
			{
				if(chrcount) linesize -= font->GetOfsX();
				h += font->GetSize() + font->GetOfsY();
				if(linesize>w) w=linesize;
				if(chrcount>c) c=chrcount;
				chrcount = 0;
				linesize = 0;
				rowcount++;
				newline = true;
			}
			else
			{
				newline = false;
				linesize += font->GetCharWidth(text[m]) + font->GetOfsX();
				chrcount++;
			}
			m++;
		}
	}
	// last row, if not empty, or after a \n
	if(chrcount || newline) 
	{
		if(chrcount) linesize -= font->GetOfsX();
		h += font->GetSize();
		if(linesize>w) w=linesize;
		if(chrcount>c) c=chrcount;
		rowcount++;
	}

	r = rowcount;
}

void cDizPaint::HUDDrawText( int tileid, const iRect & dst, char* text, int m_align )
{
	if( m_huddraw==0 ) return; // not in draw
	if( m_hudshader<0 || m_hudshader>=ShaderMax ) return; // invalid shader
	if( text==NULL ) return; // invalid text
	int tileidx = tiles.Find(tileid);
	cTile* tile = tiles.Get(tileidx); 
	if(tile==NULL) return; // invalid tile
	int fontidx = fonts.Find(m_hudfont); // find font
	cFont* font = fonts.Get(fontidx);
	if(!font) return; // no font

	// overwrite font's texture and shader
	font->font->SetTexture(tile->tex);
	font->font->SetBlend(m_hudshader);

	// draw process
	iV2 p(dst.x1, dst.y1);
	iV2 sz = dst.Size();

	int linecount = 0;				// line count
	int lnstart=0;					// start of current line (chr scan pos)
	int lnend;						// end of current line (chr scan pos)
	int size = (int)strlen(text);	// text size
	int align = m_align;			// lign align mode
	int focus = 0;					// focus mode 1/0
	dword color = m_hudcolor;		// color
	dword colorfocus;				// color when focus=1
	dword focuscolors[8] = { 0xffc80000, 0xffc800c8, 0xff00c800, 0xff00c8c8, 0xffc8c800,  0xff00c8c8, 0xff00c800, 0xffc800c8 };
//	dword focuscolors[6] = { 0xffff8000, 0xffffa000, 0xffffc000, 0xffffe000, 0xffffc000, 0xffffa000 }; // some orange version
	colorfocus = focuscolors[ (GetTickCount()/30) % 8 ];

	// scan text
	while(lnstart<size)
	{
		// Current Line Processing
		p.x = dst.x1;

		int cmd;			// escape command
		int data[4];		// escape command data

		// PASS1: scan current line for size
		int	m=lnstart;		// scan cursor
		int chrcount=0;		// printable characters to scan cursor
		int linesize=0;		// line size in pixels
		
		while(m<size && text[m]!='\n')
		{
			// escape command
			int m2;
			cmd=DLGCMD_NONE;
			if( text[m]=='{') cmd = HUDScanText(text,m,m2,data); // read command
			if(cmd!=DLGCMD_NONE) // only if command found and valid
			{
				if(cmd==DLGCMD_ALIGN) align=data[0];
				m=m2+1; // step over it
			}
			else
			{
				linesize+=font->GetCharWidth(text[m])+font->GetOfsX();
				m++;
				chrcount++;
			}
		}
		if(chrcount>0) linesize-=font->GetOfsX();

		lnend = m;

		// compute aligniament
		if(align==0) p.x+=((sz.x-linesize)/2); else
		if(align==1) p.x+=(sz.x-linesize);

		// PASS2: print characters to the end of the line
		m = lnstart;
		while(m<lnend)
		{
			if(text[m]==0) break; // safety+

			// escape command
			int m2;
			cmd=DLGCMD_NONE;
			if( text[m]=='{')	cmd = HUDScanText(text,m,m2,data); // read command
			if(cmd!=DLGCMD_NONE) // only if command found and valid
			{
				if(cmd==DLGCMD_COLOR) 	color=0xff000000 | data[0];
				else
				if(cmd==DLGCMD_FOCUS)	focus=data[0];
				else
				if(cmd==DLGCMD_TILE)	DrawTile( tiles.Find(data[0]), p.x+data[1], p.y+data[2], focus?colorfocus:color, 0, 0 );
				m=m2+1; // step over it
			}
			else
			{
				// print character
				DrawChar( fontidx, p, text[m], focus?colorfocus:color );
				p.x+=font->GetCharWidth(text[m])+font->GetOfsX();
				chrcount--;
				m++;
			}
		}

		if(text[m]=='\n') m++; // step over new line character

		// new Line
		p.y += font->GetSize() + font->GetOfsY();
		lnstart = m;
		linecount++;
	}

}


void cDizPaint::HUDDrawTile( int tileid, const iRect & dst, const iRect & src, dword flags, int frame )
{
	if( m_huddraw==0 ) return; // not in draw
	int tileidx = tiles.Find(tileid);
	if(tileidx==-1) return;
	iV2 sz = src.Size();
	if( sz.x==0 || sz.y==0 ) return;
	if(m_hudshader<0 || m_hudshader>=ShaderMax) return;

	fRect oldclip = R9_GetClipping();
	fRect newclip = fRect(scrPos(iV2(dst.x1, dst.y1)), scrPos(iV2(dst.x2, dst.y2)));
	R9_AddClipping(newclip);
	if(R9_IsClipping())
	{
		iV2 c = (dst.Size() + sz - 1) / sz;
		int blend = m_hudshader;
		iV2 p(dst.x1, dst.y1);
		for(int i=0;i<c.y;i++)
		{
			p.x = dst.x1;
			for(int j=0;j<c.x;j++)
			{
				DrawTile( tileidx, p, src, m_hudcolor, flags, frame, blend );
				p.x+=sz.x;
			}
			p.y+=sz.y;
		}
	}

	R9_SetClipping(oldclip);	
}

void cDizPaint::HudClipping( const iRect & dst )
{
	if( m_huddraw )
		if(dst.x2<dst.x1 || dst.y2<dst.y1)
			R9_ResetClipping();
		else
			R9_SetClipping(fRect(scrPos(iV2(dst.x1, dst.y1)), scrPos(iV2(dst.x2, dst.y2))));	
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Fonts
//////////////////////////////////////////////////////////////////////////////////////////////////

int		gsfont_total;			// status report on total fonts declared (load+failed)
int		gsfont_fail;			// status report on fonts failed to load
int		gsfont_duplicates;		// status report on id duplicates
int		gsfont_group;			// current font group

bool Fonts::LoadFile( const char* filepath, int group )
{

	// check file type (not counted if unaccepted);
	const char* name = file_path2file(filepath); if(!name) return false;
	const char* ext = file_path2ext(filepath); if(!ext) return false;
	if(0!=stricmp(ext,"fnt")) return false;
	
	gsfont_total++;

	// check name format
	char szt[128];
	int id = -1;
	int ret = sscanf(name,"%i %s",&id,szt);
	if(ret==0 || id==-1) 
	{ 
		gsfont_fail++; 
		dlog(LOGSYS, L"! %S (bad name)\n", filepath); 
		return false; 
	}
	
	// check unique id
	int idx = Find(id);
	if( idx!=-1 )
	{
		gsfont_fail++;
		gsfont_duplicates++;
		dlog(LOGSYS, L"! %S (duplicate id)\n", filepath, id);
		return false;
	}

	// add to list
	cFont font(id, group, new r9Font());
	font.font->Create(8,8,8,32,128);
	if(!font.font->Create(filepath))
	{
		gsfont_fail++;
		dlog(LOGSYS, L"! %S (failed to load)\n", filepath);
		return false;
	}
	push_back(std::move(font));

	if(g_dizdebug.active()) // log for developers
		dlog(LOGAPP, L"  %S\n", filepath );
	
	return true;
}

void FFCallback_Font( const char* filepath, BOOL dir )
{
	if(dir) return;
	bool ret = g_paint.fonts.LoadFile(filepath, gsfont_group);
}

bool Fonts::Load( char* path, int group )
{
	if(!path || !path[0]) return false; // invalid path
	int szlen = (int)strlen(path);
	if(path[szlen-1]!='\\') strcat(path,"\\");
	_strlwr(path);
	dlog(LOGAPP, L"Loading fonts from \"%S\" (group=%i)\n", path, group);

	// init
	gsfont_total		= 0;
	gsfont_fail			= 0;
	gsfont_duplicates	= 0;
	gsfont_group		= group;

	// find files on disk
	int archivefiles = F9_ArchiveGetFileCount(0);
	if(archivefiles==0) // if no archive found then open from disk
	{
		file_findfiles( path, FFCallback_Font, FILE_FINDREC );
	}
	else // if archive opened, load from it
	{
		for(int i=0;i<archivefiles;i++)
		{
			std::string filename = F9_ArchiveGetFileName(0,i);
			if(strstr(filename.c_str(),path)==filename)
			{
				FFCallback_Font(filename.c_str(),false);
			}
		}
	}

	// report
	dlog(LOGAPP, L"Fonts report: total=%i, failed=%i (duplicates=%i)\n\n", gsfont_total, gsfont_fail, gsfont_duplicates );

	return true;
}

void Fonts::Unload( int group )
{
	for(size_t i=0;i<size();)
		if((*this)[i].group==group)
			Del(i);
		else
			++i;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
