//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiPaint.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include <algorithm>
#include "EdiPaint.h"

cEdiPaint g_paint;

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////

cEdiPaint::cEdiPaint()
{
	m_tilepath[0] = 0;
	m_brushrect = 0;
}

cEdiPaint::~cEdiPaint()
{
	// nothing - use Done
}

BOOL cEdiPaint::Init()
{
	return TRUE;
}

void cEdiPaint::Done()
{
	TileUnload();
	index.clear();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// TILES
//////////////////////////////////////////////////////////////////////////////////////////////////

bool cEdiPaint::TileLoadFile( const std::wstring & filepath, size_t & total, size_t & fail, size_t & duplicates)
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
		elog::err() << "! "<< filepath.c_str() << " (bad name)" << std::endl; 
		return false; 
	}
	std::wstring szt;
	int frames	= 1;
	name >> szt >> frames;

	int fpl;
	if(! (name >> fpl))
		fpl = frames;
	// check unique id
	if( TileFind(id)!=-1 )
	{
		fail++;
		duplicates++;
		elog::err() << "! " << filepath.c_str() << " (duplicate id)" << std::endl;
		return false;
	}

	// load image rgba
	r9Img img;
	if(!R9_ImgLoadFile(filepath,&img))
	{
		fail++;
		elog::err() << "! " << filepath.c_str() << " (load failed)" << std::endl;
		return false;
	}

	// create new tile
	R9TEXTURE tex = R9_TextureCreate(&img);
	if(!tex)
	{
		fail++;
		elog::err() << "! " << filepath.c_str() << " (texture failed)" << std::endl;
		return false;
	}
	Tile* tile = TileGet(TileAdd(id));
	tile->tex = tex;
	tile->frames	= frames;
	tile->fx = fpl;
	tile->fy = frames / fpl;
	if(frames % fpl)
		tile->fy += 1;
	tile->name = filepath;

	R9_ImgDestroy(&img);
	
	elog::app() << "  " << filepath.c_str() << " [" << frames << "]" << std::endl;
	
	return true;
}

bool cEdiPaint::TileLoad( const std::wstring & path )
{
	m_tilepath = path;
	if(!m_tilepath.empty() && *m_tilepath.rbegin() != '\\')
		m_tilepath += L"\\";
	elog::app() << "Loading tiles from \"" << m_tilepath.c_str() << "\"" << std::endl;

	size_t total = 0;
	size_t fail = 0;
	size_t duplicates = 0;

	file_findfiles(m_tilepath, L"*.*", [this, &total, &fail, &duplicates](const std::wstring & filepath, bool) { TileLoadFile(filepath, total, fail, duplicates); }, FILE_FINDREC );

	elog::app() << "Tiles report: total=" << total << ", failed=" << fail << " (duplicates=" << duplicates << ")" << std::endl;

	// sort by id
	std::sort(m_tile.begin(), m_tile.end(), [](Tile * t1, Tile * t2) { return t1->id < t2->id; });

	// rehash after reordering
	index.clear();
	for(int i=0;i<TileCount();i++)
	{
		Tile* tile = g_paint.TileGet(i); assert(tile!=NULL);
		index.insert(Hash::value_type(tile->id, i));
	}

	return true;
}

void cEdiPaint::TileUnload()
{
	// done
	index.clear();
	std::for_each(m_tile.begin(), m_tile.end(), [](Tile *t) {t->Destroy(); delete t; });
	m_tile.clear();
}

int cEdiPaint::TileAdd( int id )
{
	if(id<0) return -1; // negative ids not accepted

	// check duplicate id
	if(TileFind(id)!=-1) return -1; // duplicate id
	
	// add new tile to list
	Tile* tile = new Tile();
	tile->id = id;
	int idx = m_tile.size();
	m_tile.push_back(tile);

	// add tracker to hash
	index.insert(Hash::value_type(tile->id, idx));
	return idx;
}

void cEdiPaint::TileDel( int idx )
{
	if(Tile* tile = TileGet(idx)) {
		index.erase(tile->id);
		delete tile;
		m_tile.erase(m_tile.begin() + idx);
	}
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// Draw functions
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiPaint::DrawTile( int idx, int x, int y, const iRect & map, dword color, int flip, int frame, Blend blend, float scale )
{
	Tile* tile = TileGet(idx); 
	if(tile==NULL) return;
	
	int w = tile->GetWidth();
	int h = tile->GetHeight();
	if(frame<0) frame=0;
	frame = frame % tile->frames;
	int fx = tile->GetFx(frame);
	int fy = tile->GetFy(frame);
	fRect src = map;
	src.Offset(fV2(fx * w, fy *h));
	R9_SetBlend(blend);
	R9_DrawSprite( fV2(x, y), src, tile->tex, color, flip, (float)scale );

}
	
void cEdiPaint::DrawTile( int idx, int x, int y, dword color, int flip, int frame, Blend blend, float scale )
{
	Tile* tile = TileGet(idx); 
	if(tile==NULL) return;

	int w = tile->GetWidth();
	int h = tile->GetHeight();
	if(frame<0) frame=0;
	frame = frame % tile->frames;
	int fx = tile->GetFx(frame);
	int fy = tile->GetFy(frame);
	fRect src(fx * w, fy * h, (fx+1) * w, (fy + 1) * h);
	R9_SetBlend(blend);
	R9_DrawSprite( fV2(x, y), src, tile->tex, color, flip, scale );

}

void cEdiPaint::DrawBrushAt( Brush* brush, int x, int y, float zoom, BOOL anim )
{
	if(brush==NULL) return;
	int idx = TileFind(brush->tile);
	// if(idx==-1) return;

	iRect map = brush->map;
	int mw = static_cast<int>(brush->mapWith());
	int mh = static_cast<int>(brush->mapHeight());
	float ms = brush->mapScale();
	if( mw==0 || mh==0 ) return;

	fRect oldclip = R9_GetClipping();
	fRect newclip( (float)x, (float)y, (float)x+zoom*brush->size.x, (float)y+zoom*brush->size.y );
	R9_AddClipping(newclip);
	if(!R9_IsClipping()) { R9_SetClipping(oldclip); return; } // fully clipped

	if(idx==-1) // no tile
	{
		dword color = brush->color & 0xffff40ff;
		R9_DrawLine( fV2(newclip.p1.x,newclip.p1.y),		fV2(newclip.p2.x-1,newclip.p1.y),	color );
		R9_DrawLine( fV2(newclip.p2.x-1,newclip.p1.y),		fV2(newclip.p2.x-1,newclip.p2.y-1),	color );
		R9_DrawLine( fV2(newclip.p2.x-1,newclip.p2.y-1),	fV2(newclip.p1.x,newclip.p2.y-1),	color );
		R9_DrawLine( fV2(newclip.p1.x,newclip.p2.y-1),		fV2(newclip.p1.x,newclip.p1.y),		color );
		R9_DrawLine( fV2(newclip.p1.x,newclip.p1.y),		fV2(newclip.p2.x-1,newclip.p2.y-1),	color );
		R9_DrawLine( fV2(newclip.p2.x-1,newclip.p1.y),		fV2(newclip.p1.x,newclip.p2.y-1),	color );
		R9_SetClipping(oldclip);
		return;
	}

	int cx = (brush->size.x+mw-1) / mw;
	int cy = (brush->size.y+mh-1) / mh;


	int xt=x;
	for(int i=0;i<cy;i++)
	{
		x = xt;
		for(int j=0;j<cx;j++)
		{
			DrawTile( idx, x, y, map, brush->color, brush->flip, brush->frame, brush->shader, zoom*ms );
			x+=(int)(zoom*mw);
		}
		y+=(int)(zoom*mh);
	}

	if(m_brushrect)
	{
		dword color = 0xa04040ff;
		R9_DrawLine( fV2(newclip.p1.x,newclip.p1.y),		fV2(newclip.p2.x-1,newclip.p1.y),	color );
		R9_DrawLine( fV2(newclip.p2.x-1,newclip.p1.y),		fV2(newclip.p2.x-1,newclip.p2.y-1),	color );
		R9_DrawLine( fV2(newclip.p2.x-1,newclip.p2.y-1),	fV2(newclip.p1.x,newclip.p2.y-1),	color );
		R9_DrawLine( fV2(newclip.p1.x,newclip.p2.y-1),		fV2(newclip.p1.x,newclip.p1.y),		color );
	}

	R9_SetClipping(oldclip);
}

void cEdiPaint::DrawBrushFlashAt( Brush* brush, int x, int y, float zoom, BOOL anim )
{
	if(brush==NULL) return;
	Blend shd = brush->shader;
	int col = brush->color;
	brush->shader = Blend::AlphaRep;
	brush->color = GetFlashingColorBW();
	g_paint.DrawBrushAt( brush, x, y, zoom, anim );
	brush->shader = shd ;
	brush->color = col;
}

dword cEdiPaint::GetFlashingColorBW()
{
	int period = 500;
	float pow = (float)((sys_gettickcount())%period) / period;
	if(pow>0.5f) pow = 1.0f-pow;
	dword color = (dword)(pow * 255);
	color = 0xa0000000 | (color<<16) | (color<<8) | (color);
	return color;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
