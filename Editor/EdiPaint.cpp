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
	m_shadersel = Blend::AlphaRep;
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
int		gstile_total;			// status report on total tiles declared (load+failed)
int		gstile_fail;			// status report on tiles failed to load
int		gstile_duplicates;		// status report on id duplicates

BOOL cEdiPaint::TileLoadFile( const char* filepath )
{
	
	// check file type (not counted if unaccepted); only TGA and PNG files accepted
	std::string ext = file_path2ext(filepath);
	if(ext != "tga" && ext != "png") return FALSE;
	const char* name = file_path2file(filepath); if(!name) return FALSE;
	
	gstile_total++;

	// check name format
	char szt[256];
	int id		= -1;
	int frames	= 1;
	int fpl = -1;
	int ret = sscanf(name,"%i %s %i %i",&id,szt,&frames,&fpl);
	if(ret==0 || id==-1) 
	{ 
		gstile_fail++; 
		dlog(LOGSYS, L"! %S (bad name)\n", filepath); 
		return FALSE; 
	}
	if(frames<1) frames=1;
	if(fpl < 0) fpl = frames;	
	// check unique id
	int idx = TileFind(id);
	if( idx!=-1 )
	{
		gstile_fail++;
		gstile_duplicates++;
		dlog(LOGSYS, L"! %S (duplicate id)\n", filepath, id);
		return FALSE;
	}

	// load image rgba
	strcpy(szt,m_tilepath);strcat(szt,filepath);
	r9Img img;
	if(!R9_ImgLoadFile(szt,&img))
	{
		gstile_fail++;
		dlog(LOGSYS, L"! %S (load failed)\n", filepath);
		return FALSE;
	}

	// create new tile
	idx = TileAdd(id);
	cTile* tile = TileGet(idx); assert(tile!=NULL);
	tile->m_tex = R9_TextureCreate(&img);
	if(tile->m_tex==NULL)
	{
		TileDel(idx);
		gstile_fail++;
		dlog(LOGSYS, L"! %S (texture failed)\n", filepath);
		return FALSE;
	}

	// set current options
	tile->m_frames	= frames;
	tile->fx = fpl;
	tile->fy = frames / fpl;
	if(frames % fpl)
		tile->fy += 1;
	tile->m_name	= sstrdup(filepath);

	R9_ImgDestroy(&img);
	
	dlog(LOGAPP, L"  %S [%i]\n", filepath, frames );
	
	return TRUE;
}

void FFCallback_Tile( const char* filepath, BOOL dir )
{
	if(dir) return;
	int n = (int)strlen(g_paint.m_tilepath);
	BOOL ret = g_paint.TileLoadFile(filepath+n);
}

BOOL cEdiPaint::TileLoad( const char* path )
{
	if(!path || !path[0]) return FALSE; // invalid path
	strcpy(m_tilepath,path);
	int szlen = (int)strlen(m_tilepath);
	if( szlen>0 && m_tilepath[szlen-1]!='\\' ) strcat(m_tilepath,"\\");
	_strlwr(m_tilepath);
	dlog(LOGAPP, L"Loading tiles from \"%S\"\n", m_tilepath);

	// init
	gstile_total		= 0;
	gstile_fail			= 0;
	gstile_duplicates	= 0;

	// find files on disk
	file_findfiles( m_tilepath, FFCallback_Tile, FILE_FINDREC );

	// report
	dlog(LOGAPP, L"Tiles report: total=%i, failed=%i (duplicates=%i)\n", gstile_total, gstile_fail, gstile_duplicates );

	// sort by id
	int i;
	BOOL ok;
	do
	{
		ok=TRUE;
		for(i=1;i<TileCount();i++)
		{
			if(m_tile[i-1]->m_id > m_tile[i]->m_id)
			{
				ok = FALSE;
				std::swap(m_tile[i-1], m_tile[i]);
			}
		}
	}
	while(!ok);

	// rehash after reordering
	index.clear();
	for(i=0;i<TileCount();i++)
	{
		cTile* tile = g_paint.TileGet(i); assert(tile!=NULL);
		index.insert(Hash::value_type(tile->m_id, i));
	}

	return TRUE;
}

void cEdiPaint::TileUnload()
{
	// done
	index.clear();
	std::for_each(m_tile.begin(), m_tile.end(), [](cTile *t) {t->Destroy(); delete t; });
	m_tile.clear();
}

int cEdiPaint::TileAdd( int id )
{
	if(id<0) return -1; // negative ids not accepted

	// check duplicate id
	if(TileFind(id)!=-1) return -1; // duplicate id
	
	// add new tile to list
	cTile* tile = new cTile();
	tile->m_id = id;
	int idx = m_tile.size();
	m_tile.push_back(tile);

	// add tracker to hash
	index.insert(Hash::value_type(tile->m_id, idx));
	return idx;
}

void cEdiPaint::TileDel( int idx )
{
	if(cTile* tile = TileGet(idx)) {
		index.erase(tile->m_id);
		delete tile;
		m_tile.erase(m_tile.begin() + idx);
	}
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// Draw functions
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiPaint::DrawTile( int idx, int x, int y, const iRect & map, dword color, int flip, int frame, Blend blend, float scale )
{
	cTile* tile = TileGet(idx); 
	if(tile==NULL) return;
	
	int w = tile->GetWidth();
	int h = tile->GetHeight();
	if(frame<0) frame=0;
	frame = frame % tile->m_frames;
	int fx = tile->GetFx(frame);
	int fy = tile->GetFy(frame);
	fRect src = map;
	src.Offset(fV2(fx * w, fy *h));
	R9_SetBlend(blend);
	R9_DrawSprite( fV2(x, y), src, tile->m_tex, color, flip, (float)scale );

}
	
void cEdiPaint::DrawTile( int idx, int x, int y, dword color, int flip, int frame, Blend blend, float scale )
{
	cTile* tile = TileGet(idx); 
	if(tile==NULL) return;

	int w = tile->GetWidth();
	int h = tile->GetHeight();
	if(frame<0) frame=0;
	frame = frame % tile->m_frames;
	int fx = tile->GetFx(frame);
	int fy = tile->GetFy(frame);
	fRect src(fx * w, fy * h, (fx+1) * w, (fy + 1) * h);
	R9_SetBlend(blend);
	R9_DrawSprite( fV2(x, y), src, tile->m_tex, color, flip, scale );

}

void cEdiPaint::DrawBrushAt( tBrush* brush, int x, int y, float zoom, BOOL anim )
{
	if(brush==NULL) return;
	int idx = TileFind(brush->m_data[BRUSH_TILE]);
	// if(idx==-1) return;

	iRect map = brush->map();
	int mw = static_cast<int>(brush->mapWith());
	int mh = static_cast<int>(brush->mapHeight());
	float ms = brush->mapScale();
	if( mw==0 || mh==0 ) return;

	fRect oldclip = R9_GetClipping();
	fRect newclip( (float)x, (float)y, (float)x+zoom*brush->m_data[BRUSH_W], (float)y+zoom*brush->m_data[BRUSH_H] );
	R9_AddClipping(newclip);
	if(!R9_IsClipping()) { R9_SetClipping(oldclip); return; } // fully clipped

	if(idx==-1) // no tile
	{
		dword color = brush->m_data[BRUSH_COLOR] & 0xffff40ff;
		R9_DrawLine( fV2(newclip.p1.x,newclip.p1.y),		fV2(newclip.p2.x-1,newclip.p1.y),	color );
		R9_DrawLine( fV2(newclip.p2.x-1,newclip.p1.y),		fV2(newclip.p2.x-1,newclip.p2.y-1),	color );
		R9_DrawLine( fV2(newclip.p2.x-1,newclip.p2.y-1),	fV2(newclip.p1.x,newclip.p2.y-1),	color );
		R9_DrawLine( fV2(newclip.p1.x,newclip.p2.y-1),		fV2(newclip.p1.x,newclip.p1.y),		color );
		R9_DrawLine( fV2(newclip.p1.x,newclip.p1.y),		fV2(newclip.p2.x-1,newclip.p2.y-1),	color );
		R9_DrawLine( fV2(newclip.p2.x-1,newclip.p1.y),		fV2(newclip.p1.x,newclip.p2.y-1),	color );
		R9_SetClipping(oldclip);
		return;
	}

	int cx = (brush->m_data[BRUSH_W]+mw-1) / mw;
	int cy = (brush->m_data[BRUSH_H]+mh-1) / mh;

	int sh = brush->m_data[BRUSH_SHADER];
	Blend shader;
	if(sh < static_cast<int>(Blend::Min) || sh >= static_cast<int>(Blend::Max)) shader = m_shadersel;
	else shader = static_cast<Blend>(sh);

	int frame = brush->m_data[BRUSH_FRAME];
	//@ if(!anim) frame=0;
	
	int xt=x;
	for(int i=0;i<cy;i++)
	{
		x = xt;
		for(int j=0;j<cx;j++)
		{
			DrawTile( idx, x, y, map, brush->m_data[BRUSH_COLOR], brush->m_data[BRUSH_FLIP], frame, shader, zoom*ms );
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

void cEdiPaint::DrawBrushFlashAt( tBrush* brush, int x, int y, float zoom, BOOL anim )
{
	if(brush==NULL) return;
	int shd = brush->m_data[BRUSH_SHADER];
	int col = brush->m_data[BRUSH_COLOR];
	brush->m_data[BRUSH_SHADER] = -1; // will use the shadersel (with the ALPHAREP blend)
	brush->m_data[BRUSH_COLOR] = GetFlashingColorBW();
	g_paint.DrawBrushAt( brush, x, y, zoom, anim );
	brush->m_data[BRUSH_SHADER] = shd ;
	brush->m_data[BRUSH_COLOR] = col;
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
