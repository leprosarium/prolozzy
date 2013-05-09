//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiMap.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include <algorithm>

#include "I9Input.h"

#include "EdiMap.h"
#include "EdiApp.h"
#include "GUI.h"

#include "R9ImgLoader.h"

cEdiMap g_map;

PREDICATE_M(map, getMapW, 1)
{
	return A1 = g_map.m_mapw;
}

PREDICATE_M(map, getMapH, 1)
{
	return A1 = g_map.m_maph;
}

PREDICATE_M(map, getRoomW, 1)
{
	return A1 = g_map.m_roomw;
}

PREDICATE_M(map, getRoomH, 1)
{
	return A1 = g_map.m_roomh;
}

PREDICATE_M(map, getRoomGrid, 1)
{
	return A1 = g_map.m_roomgrid;
}

PREDICATE_M(map, getCamX, 1)
{
	return A1 = g_map.m_camx;
}

PREDICATE_M(map, getCamY, 1)
{
	return A1 = g_map.m_camy;
}

PREDICATE_M(map, getZoom, 1)
{
	return  A1 = g_map.m_camz;
}

PREDICATE_M(map, getSelect, 1)
{
	return A1 = g_map.m_selectcount;
}

PREDICATE_M(map, setRoomW, 1)
{
	g_map.m_roomw = A1;
	return true;
}

PREDICATE_M(map, setRoomH, 1)
{
	g_map.m_roomh = A1;
	return true;
}

PREDICATE_M(map, setRoomGrid, 1)
{
	g_map.m_roomgrid = A1;
	return true;
}

PREDICATE_M(map, setCamX, 1)
{
	g_map.m_camx = A1;
	return true;
}

PREDICATE_M(map, setCamY, 1)
{
	g_map.m_camy = A1;
	return true;
}

PREDICATE_M(map, setZoom, 1)
{
	g_map.m_camz = A1;
	return true;
}

PREDICATE_M(map, setSelect, 1)
{
	g_map.m_selectcount = A1;
	return true;
}

PREDICATE_M(map, load, 1)
{
	return g_map.Load(WideStringToMultiByte(A1));
}

PREDICATE_M(map, brushCount, 1)
{
	return A1 = g_map.m_brushcount;
}

// map brush ................................................................................
#define MAP_BRUSH_PROP(Prop, PROP)\
GET_MAP_BRUSH_PROP(Prop, PROP)\
SET_MAP_BRUSH_PROP(Prop, PROP)

#define GET_MAP_BRUSH_PROP(Prop, PROP) PREDICATE_M(map, brushGet##Prop, 2)\
{\
	return A2 = g_map.GetBrush(A1).m_data[BRUSH_##PROP];\
}

#define SET_MAP_BRUSH_PROP(Prop, PROP) PREDICATE_M(map, brushSet##Prop, 2)\
{\
	g_map.GetBrush(A1).m_data[BRUSH_##PROP] = A2; \
	return true;\
}

MAP_BRUSH_PROP(Layer, LAYER)
MAP_BRUSH_PROP(X, X)
MAP_BRUSH_PROP(Y, Y)
MAP_BRUSH_PROP(W, W)
MAP_BRUSH_PROP(H, H)
MAP_BRUSH_PROP(Tile, TILE)
MAP_BRUSH_PROP(Frame, FRAME)
MAP_BRUSH_PROP(MapX1, MAP)
MAP_BRUSH_PROP(MapY1, MAP+1)
MAP_BRUSH_PROP(MapX2, MAP+2)
MAP_BRUSH_PROP(MapY2, MAP+3)
MAP_BRUSH_PROP(Flip, FLIP)
MAP_BRUSH_PROP(Shader, SHADER)
MAP_BRUSH_PROP(Scale, SCALE)
MAP_BRUSH_PROP(Select, SELECT)

MAP_BRUSH_PROP(Type, TYPE)
MAP_BRUSH_PROP(ID, ID)
MAP_BRUSH_PROP(Material, MATERIAL)
MAP_BRUSH_PROP(Draw, DRAW)
MAP_BRUSH_PROP(Disable, DISABLE)
MAP_BRUSH_PROP(Delay, DELAY)
MAP_BRUSH_PROP(Anim, ANIM)
MAP_BRUSH_PROP(Collider, COLLIDER)
MAP_BRUSH_PROP(Class, CLASS)
MAP_BRUSH_PROP(Status, STATUS)
MAP_BRUSH_PROP(Target, TARGET)
MAP_BRUSH_PROP(Death, DEATH)

PREDICATE_M(map, brushGetColor, 2) 
{
	int64 color = static_cast<unsigned>(g_map.GetBrush(A1).m_data[BRUSH_COLOR]);
	return A2 = color;
}


PREDICATE_M(map, brushGet, 3) 
{
	tBrush & brush = g_map.GetBrush(A1);
	int idx = A2;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A2);
	if(idx == BRUSH_COLOR) {
		int64 color = static_cast<unsigned>(brush.m_data[idx]);
		return A3 = color;
	}
	return A3 = brush.m_data[idx];
}

PREDICATE_M(map, brushSetColor , 2) 
{
	int64 color = A2;
	g_map.GetBrush(A1).m_data[BRUSH_COLOR] = static_cast<int>(color);
	return true;
}

PREDICATE_M(map, brushSet , 3) 
{
	tBrush & brush = g_map.GetBrush(A1);
	int idx = A2;
	if(idx < 0 || idx >= BRUSH_MAX) 
		throw PlDomainError("invalid brush variable", A2);
	if(idx == BRUSH_COLOR)
	{
		int64 color = A3;
		brush.m_data[idx] = static_cast<int>(color);
	} 
	else 
	{
		brush.m_data[idx] = A3;
	}
	return true;
}

PREDICATE_M(map, brushNew, 0)
{
	g_map.BrushNew();
	EdiApp()->UndoReset();
	return true;
}

PREDICATE_M(map, brushNew, 1)
{
	int idx = g_map.BrushNew();
	EdiApp()->UndoReset();
	return A1 = idx;
}

PREDICATE_M(map, brushDel, 1)
{
	g_map.BrushDel(A1);
	EdiApp()->UndoReset();
	return 0;
}

PREDICATE_M(map, repartition, 0)
{
	return g_map.PartitionRepartition();
}


PREDICATE_M(map, refresh, 0) 
{
	g_map.m_refresh = TRUE;
	return true;
}

PREDICATE_M(map, reset, 0)
{
	g_map.Reset();
	EdiApp()->UndoReset();
	return true;
}


PREDICATE_M(map, resize, 2)
{
	int ret = g_map.Resize(A1, A2); 
	EdiApp()->UndoReset();
	return ret; 
}


PREDICATE_M(selection, goto, 1)
{
	g_map.SelectionGoto(A1);
	return true;
}

PREDICATE_M(selection, refresh, 0)
{
	g_map.SelectionRefresh();
	return 0;
}

PREDICATE_M(map, saveImage, 1)
{
	bool ret = g_map.SaveMapImage(WideStringToMultiByte(A1));
	g_map.m_refresh = TRUE;
	return ret;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// PARTITION
//////////////////////////////////////////////////////////////////////////////////////////////////
cPartitionCel::cPartitionCel()
{
	m_count=0;
	m_size=0;
	m_data=NULL;
}

cPartitionCel::~cPartitionCel()
{
	if(m_data!=NULL) free(m_data);
}

void cPartitionCel::Add( int val )
{
	if(m_count==m_size)
	{
		m_size+=256;
		m_data = (int*)realloc(m_data,m_size*sizeof(int));
		assert(m_data!=NULL);
	}
	m_data[m_count]=val;
	m_count++;
}

void cPartitionCel::Sub( int val )
{
	for(int i=0;i<m_count;i++)
	{
		if(m_data[i]==val) 
		{
			if( i<m_count-1 ) 
				memcpy( &m_data[i], &m_data[i+1], (m_count-1-i)*sizeof(int) );
			m_count--;
			return;
		}
	}
}

int cPartitionCel::Find( int val )
{
	for(int i=0;i<m_count;i++)
		if(m_data[i]==val) return i;
	return -1;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////

cEdiMap::cEdiMap()
{
	
	// map
	m_mapw			= 0;
	m_maph			= 0;
	m_roomw			= 0;
	m_roomh			= 0;
	m_roomgrid		= 0;

	// view
	m_viewx			= 0;
	m_viewy			= 0;
	m_vieww			= 0;
	m_viewh			= 0;

	// camera
	m_camx			= 0;
	m_camy			= 0;
	m_camz			= 1;
	
	// refresh
	m_hideselected	= FALSE;
	m_refresh		= TRUE;
	m_target		= NULL;

	// brushes
	m_brushcount	= 0;
	m_brushsize		= 0;
	m_brush			= NULL;

	// selection
	m_selectcount	= 0;
	m_selectgoto	= 0;

	// others
	m_count_brushdraw = 0;
	m_count_brushcheck = 0;

}

cEdiMap::~cEdiMap()
{
}

BOOL cEdiMap::Init()
{

	// create render target shader
	int width = R9_GetWidth();
	int height = R9_GetHeight();
	m_target = R9_TextureCreateTarget(width,height);
	if(!m_target) {	dlog(LOGAPP, L"can't create render target."); return FALSE; }

	m_mapw = MAP_SIZEDEFAULT;
	m_maph = MAP_SIZEDEFAULT;
	CheckMapView();

	m_camx = m_mapw/2;
	m_camy = m_maph/2;

	// partitioning
	PartitionInit();

	// clear the target
	if(R9_BeginScene(m_target))
	{
		R9_Clear(EdiApp()->GetColor(EDI_COLORMAP));
		R9_EndScene();
	}

	return TRUE;
}

void cEdiMap::Done()
{

	MarkerClear();
	PartitionDone();

	// refresh
	if(m_target) { R9_TextureDestroy(m_target); m_target=NULL; }
	
	// brushes
	if(m_brush) free(m_brush);
	m_brush=NULL;
	m_brushcount = 0;
	m_brushsize	= 0;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// UPDATE
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::Update( float dtime )
{

	int stepx = EdiApp()->m_gridsize;
	int stepy = EdiApp()->m_gridsize;
	int mx = EdiApp()->GetMouseX() - VIEWX;
	int my = EdiApp()->GetMouseY() - VIEWY;
	int mz = I9_GetAxeDelta(I9_MOUSE_Z);
	BOOL shift	= (I9_GetKeyValue(I9K_LSHIFT))	 || (I9_GetKeyValue(I9K_RSHIFT));
	BOOL alt	= (I9_GetKeyValue(I9K_LALT))	 || (I9_GetKeyValue(I9K_RALT)) || (I9_GetKeyValue(I9_MOUSE_B3));
	BOOL ctrl	= (I9_GetKeyValue(I9K_LCONTROL)) || (I9_GetKeyValue(I9K_RCONTROL));

	// navigation
	int dx = 0;
	int dy = 0;
	
	// key
	if(ctrl) 
	{
		stepx=m_roomw;
		stepy=m_roomh;
	}
	if( I9_GetKeyValue(I9K_RIGHT) )	dx = stepx;
	if( I9_GetKeyValue(I9K_LEFT) )	dx =-stepx;
	if( I9_GetKeyValue(I9K_DOWN) )	dy = stepy;
	if( I9_GetKeyValue(I9K_UP) )	dy =-stepy;

	// smart key hit delaying system
	static int keycnt=0; // key delay counter
	if( dx!=0 || dy!=0 )
	{
		if( keycnt>0 ) // allow first hit
		{
			if(keycnt<400 ) 
			{
				dx = dy = 0; // big wait when first pressed
			}
			else
			{
				keycnt=350; // allow this hit and request another small wait
			}
		}
		keycnt += 1+(int)(dtime*1000); // grow keycounter (make sure it grows)
	}
	else
	{
		keycnt=0; // reset keycounter
	}

	// vertical scroll
	if( mz!=0 && iRect(VIEWW, 0, VIEWW+VIEWB, VIEWH).IsInside(iV2(mx, my)) )
	{
		if(mz<0) dy = stepy;
		if(mz>0) dy =-stepy;
		EdiApp()->m_mscrolly = 0;
		EdiApp()->m_mscrollx = 0;
	}
	
	// horizontal scroll
	if( mz!=0 && iRect(0, VIEWH, VIEWW, VIEWH+VIEWB).IsInside(iV2(mx, my)))
	{
		if(mz<0) dx = stepx;
		if(mz>0) dx =-stepx;
		EdiApp()->m_mscrolly = 0;
		EdiApp()->m_mscrollx = 0;
	}

	// scroll with the Scroll function (from WM_MOUSEWHEEL)
	if(EdiApp()->m_mscrolly!=0) { dy = EdiApp()->m_mscrolly * stepy; EdiApp()->m_mscrolly = 0; }
	if(EdiApp()->m_mscrollx!=0) { dx = EdiApp()->m_mscrollx * stepx; EdiApp()->m_mscrollx = 0; }

	// scrolling
	iRect rc;
	mx = EdiApp()->GetMouseX();
	my = EdiApp()->GetMouseY();
	if(!m_scrolling && I9_GetKeyDown(I9_MOUSE_B1))
	{
		rc = GetHScrollRect();
		if(rc.IsInside(fV2(mx,my))) 
		{
			m_scrolling = 1;
			m_scrollofs = mx-rc.p1.x;
			App.SetCursor(Cursor::Hand);
		}
		else
		{
			rc = GetVScrollRect();
			if(rc.IsInside(fV2(mx,my)))
			{
				m_scrolling = 2;
				m_scrollofs = my-rc.p1.y;
				App.SetCursor(Cursor::Hand);
			}
		}
	}
	else
	if(m_scrolling && !I9_GetKeyValue(I9_MOUSE_B1))
	{
		m_scrolling = 0;
		App.SetCursor(Cursor::Arrow);
	}

	if(m_scrolling==1) // scroll horizontal
	{
		rc = GetHScrollRect();
		dx = (mx-m_scrollofs)-rc.p1.x;
		dx = (dx/EdiApp()->m_gridsize)*EdiApp()->m_gridsize;
	}
	else
	if(m_scrolling==2) // scroll vertical
	{
		rc = GetVScrollRect();
		dy = (my-m_scrollofs)-rc.p1.y;
		dy = (dy/EdiApp()->m_gridsize)*EdiApp()->m_gridsize;
	}


	// apply
	if( dx!=0 || dy!=0 )
	{
		m_camx += dx;
		m_camy += dy;
		m_refresh=TRUE;
	}

	// others
	if(!alt && !shift && !ctrl)
	{
		if(I9_GetKeyDown(I9K_A))			{ EdiApp()->m_axes = !EdiApp()->m_axes; }
		if(I9_GetKeyDown(I9K_S))			{ EdiApp()->m_snap = !EdiApp()->m_snap; }
		if(I9_GetKeyDown(I9K_G))			{ EdiApp()->m_grid = !EdiApp()->m_grid; m_refresh=TRUE; }
	}

	// bounds
	if(m_camz<1) m_camz=1;
	if(m_camz>4) m_camz=4;

	int camw = m_vieww/m_camz;
	int camh = m_viewh/m_camz;
	if(m_camx<camw/2) m_camx=camw/2;
	if(m_camy<camh/2) m_camy=camh/2;
	if(m_camx>m_mapw-camw/2) m_camx=m_mapw-camw/2;
	if(m_camy>m_maph-camh/2) m_camy=m_maph-camh/2;

	if(m_refresh) Refresh(); // @HM is it safe for draw (needs to be after bounds checks) !
	m_refresh = FALSE;

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// DRAW
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::Draw()
{

	// draw pre-rendered map
	if(!m_target) return; // render target is not supported
	R9_SetBlend(Blend::Opaque);
	R9_DrawSprite( fV2(m_viewx,m_viewy), fRect(0,0,m_vieww,m_viewh), m_target );
	R9_Flush();
	R9_SetBlend(Blend::Alpha);

	// draw scrollers
	DrawScrollers();

	// draw marker
	int dist = 0;
	int mark = MarkerClosest(m_camx,m_camy,dist);
	if(mark!=-1 && dist==0) // on marker
	{
		R9_DrawBar( fRect(m_viewx-VIEWB+3,m_viewy-VIEWB+3,m_viewx-3, m_viewy-3), 0xffffffff );
	}

}

void cEdiMap::Refresh()
{
	// draw in render target
	if(R9_BeginScene(m_target))
	{
		R9_Clear(EdiApp()->GetColor(EDI_COLORMAP));
		iV2 cam(m_camx, m_camy);
		iRect view(cam, cam);
		view.Deflate((iV2(m_vieww, m_viewh) / m_camz) /2);
		BrushDrawExtra( view );

		DrawGrid( view );

		R9_EndScene();
	}
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// IO
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::Reset()
{

	MarkerClear();
	PartitionDone();
	BrushClear();

	m_mapw = MAP_SIZEDEFAULT;
	m_maph = MAP_SIZEDEFAULT;
	CheckMapView();

	// partitioning
	PartitionInit();

	m_camx = m_mapw/2;
	m_camy = m_maph/2;

	m_refresh = TRUE;
	SelectionRefresh();

}

int cEdiMap::Resize( int width, int height )
{
	if(width<MAP_SIZEMIN)	width = MAP_SIZEMIN;	// too small
	if(height<MAP_SIZEMIN)	height = MAP_SIZEMIN;	// too small
	if(width>MAP_SIZEMAX)	width = MAP_SIZEMAX;	// too big
	if(height>MAP_SIZEMAX)	height = MAP_SIZEMAX;	// too big

	PartitionDone();
	
	m_mapw = width;
	m_maph = height;
	CheckMapView();
	
	PartitionInit();
	BOOL ok = PartitionRepartition();

	SelectionRefresh();
	MarkerResize();

	m_camx = m_mapw/2;
	m_camy = m_maph/2;
	m_refresh = TRUE;
	return ok;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// BRUSHES
//////////////////////////////////////////////////////////////////////////////////////////////////
int	cEdiMap::BrushNew()
{
	if(m_brushcount==m_brushsize)
	{
		m_brushsize += 1024; // grow
		m_brush = (tBrush*)realloc(m_brush, m_brushsize*sizeof(tBrush));
		assert(m_brush!=NULL);
	}
	
	// init
	tBrush& brush = m_brush[m_brushcount];
	memset(&brush,0,sizeof(brush));
	brush.m_data[BRUSH_TILE] = -1;
	brush.m_data[BRUSH_COLOR] = 0xffffffff;

	m_brushcount++;
	return m_brushcount-1;
}

void cEdiMap::BrushIns( int idx, tBrush& brush )
{
	assert(0<=idx && idx<=m_brushcount);
	BrushNew();
	for(int i=m_brushcount-1;i>idx;i--) m_brush[i] = m_brush[i-1];
	m_brush[idx]=brush;
	if(m_brush[idx].m_data[BRUSH_SELECT]) m_selectcount++;
}

void cEdiMap::BrushDel( int idx )
{
	assert(0<=idx && idx<m_brushcount);
	if(m_brush[idx].m_data[BRUSH_SELECT]) m_selectcount--;
	if( idx<m_brushcount-1 )
		memcpy( &m_brush[idx], &m_brush[idx+1], sizeof(tBrush)*(m_brushcount-1-idx) );
	m_brushcount--;
}

tBrush & cEdiMap::GetBrush(int idx)
{
	if(idx < 0 || idx >= g_map.m_brushcount) 
		throw PlDomainError("invalid map brush index");
	return g_map.m_brush[idx];
}

/*
void cEdiMap::BrushDrawOld( iRect& view )
{
	return; // update brushview to be usable !!!???

	m_count_brushdraw = 0;
	m_count_brushcheck = 0;

	tBrush brushtemp = EdiApp()->m_brush;
	int fid = gs_findfn(g_gui->m_vm,"MOD_BrushDraw");

	for( int idx=0; idx<m_brushcount; idx++ )
	{
		m_count_brushcheck++;
		tBrush& brush = m_brush[idx];
		int layer = brush.m_data[BRUSH_LAYER];
		if(layer<0 || layer>=LAYER_MAX) continue;
		if(EdiApp()->LayerGet(layer)==0) continue; // hidden

		iRect bb;
		bb.x1 = brush.m_data[BRUSH_X];
		bb.y1 = brush.m_data[BRUSH_Y];
		bb.x2 = brush.m_data[BRUSH_X] + brush.m_data[BRUSH_W];
		bb.y2 = brush.m_data[BRUSH_Y] + brush.m_data[BRUSH_H];
		if(!RECT2RECT(view,bb)) continue;

		// user callback
		EdiApp()->m_brush = brush;
		if(fid!=-1)
		{
			int ret=g_gui->ScriptCallback(fid);
			if(!ret) continue;
		}

		int x = m_camz * (EdiApp()->m_brush.m_data[BRUSH_X]-view.x1);
		int y = m_camz * (EdiApp()->m_brush.m_data[BRUSH_Y]-view.y1);
		g_paint.DrawBrushAt( &EdiApp()->m_brush, x, y, m_camz );

		m_count_brushdraw++;
	}

	EdiApp()->m_brush = brushtemp;

}
*/

void cEdiMap::BrushDrawExtra( iRect& view )
{
	m_count_brushdraw = 0;
	m_count_brushcheck = 0;

	tBrush brushtemp = EdiApp()->m_brush;

	int partition[32];
	int partitioncount = PartitionGet(view, partition,32);
	if( partitioncount==0 ) return;

	// brushvis is a draw buffer that holds indexes to brushes accepted for draw; will be order before draw
	brushvis.clear();

	// check partitions for draw
	for( int p=0; p<partitioncount; p++ )
	{
		int pidx = partition[p];
		int brushcount = m_partition[pidx]->m_count;
		for( int i=0; i<brushcount; i++ )
		{
			int idx = m_partition[pidx]->m_data[i];
			assert(0<=idx && idx<m_brushcount);
			m_count_brushcheck++;

			tBrush& brush = m_brush[idx];
			int layer = brush.m_data[BRUSH_LAYER];
			if(layer<0 || layer>=LAYER_MAX) continue;
			if(EdiApp()->LayerGet(layer)==0) continue; // hidden

			if(!view.Intersects(brush.rect())) continue;

			brushvis.push_back(idx); // store in drawbuffer
		}
	}

	// order drawbuffer by index // @TODO optimize

	std::sort(brushvis.begin(), brushvis.end(), [this](int idx1, int idx2) {
			int l1 = m_brush[idx1].m_data[BRUSH_LAYER];
			int l2 = m_brush[idx2].m_data[BRUSH_LAYER];
			return l1 == l2 ? idx1 < idx2 : l1 < l2;
	});

	// remove duplicates
	brushvis.erase( std::unique( brushvis.begin(), brushvis.end() ), brushvis.end() );

	// draw drawbuffer
	for(size_t idx: brushvis)
	{
		tBrush & brush = m_brush[idx];

		// user callback
		EdiApp()->m_brush = brush;
		if(!g_gui->ScriptPrologDo("mod:brushDraw")) continue;

		if( m_hideselected && brush.m_data[BRUSH_SELECT] ) continue;
		iV2 p = m_camz  * EdiApp()->m_brush.pos() - view.p1;
		g_paint.DrawBrushAt( &EdiApp()->m_brush, p.x, p.y, (float)m_camz );

		m_count_brushdraw++;
	}

	EdiApp()->m_brush = brushtemp;

}

int	cEdiMap::BrushPick( int x, int y )
{
	for( int idx=m_brushcount-1; idx>=0; idx-- ) // top to bottom
	{
		tBrush& brush = m_brush[idx];
		int layer = brush.m_data[BRUSH_LAYER];
		if(layer<0 || layer>=LAYER_MAX) continue;
		if(EdiApp()->LayerGet(layer)==0) continue; // hidden
		if(brush.rect().IsInside(iV2(x,y))) return idx;
	}
	return -1;
}

void cEdiMap::BrushToFront( int idx )
{
	assert(0<=idx && idx<m_brushcount);
	// search	
	int i;
	int idx0=idx;
	int layer = m_brush[idx].m_data[BRUSH_LAYER];
	for(i=idx;i<m_brushcount;i++)
	{
		if(m_brush[i].m_data[BRUSH_LAYER]==layer) idx0=i;
	}
	if(idx0==idx) return; // already in front

	PartitionDel(idx); // delete before messing it

	// shift and replace
	tBrush brush = m_brush[idx];
	for(i=idx;i<idx0;i++)
	{
		m_brush[i] = m_brush[i+1];
	}
	m_brush[idx0] = brush;

	PartitionFix(idx+1,idx0,-1); // fix indices after shifting
	PartitionAdd(idx0); // add new one

}

void cEdiMap::BrushToBack( int idx )
{
	assert(0<=idx && idx<m_brushcount);
	// search	
	int i;
	int idx0=idx;
	int layer = m_brush[idx].m_data[BRUSH_LAYER];
	for(i=idx;i>=0;i--)
	{
		if(m_brush[i].m_data[BRUSH_LAYER]==layer) idx0=i;
	}
	if(idx0==idx) return; // already in back

	PartitionDel(idx); // delete before messing it

	// shift and replace
	tBrush brush = m_brush[idx];
	for(i=idx;i>idx0;i--)
	{
		m_brush[i] = m_brush[i-1];
	}
	m_brush[idx0] = brush;

	PartitionFix(idx0, idx-1, 1); // fix indices after shifting
	PartitionAdd(idx0); // add new one
	

}

void cEdiMap::BrushClear()
{
	if(m_brush) free(m_brush);
	m_brush=NULL;
	m_brushcount = 0;
	m_brushsize	= 0;
	brushvis.clear();
	m_selectcount=0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// PARTITIONING
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::PartitionInit()
{
	PartitionDone();// safety
	int pcountw = PartitionCountW();
	int pcounth = PartitionCountH();
	for(int i=0;i<pcountw*pcounth;i++)
		m_partition.push_back(new cPartitionCel());
}

void cEdiMap::PartitionDone()
{
	std::for_each(m_partition.begin(), m_partition.end(), [](cPartitionCel * c) {delete c;});
	m_partition.clear(); 
}

BOOL cEdiMap::PartitionAdd( int brushidx )
{
	int pcountw = PartitionCountW();
	iRect br = m_brush[brushidx].rect();
	BOOL ok=FALSE;
	for(size_t i=0; i<m_partition.size(); i++)
		if( br.Intersects(PartitionRect(i, pcountw)) )
		{	
			m_partition[i]->Add(brushidx);
			ok = TRUE;
		}
	if(!ok)
		dlog(LOGAPP, L"Brush # %d (%d, %d)-(%d, %d) out of bounds\n", brushidx, br.p1.x, br.p1.y, br.p2.x, br.p2.y);
	return ok;
}

void cEdiMap::PartitionDel( int brushidx )
{
	int pcountw = PartitionCountW();
	iRect br = m_brush[brushidx].rect();
	for(size_t i=0; i<m_partition.size(); i++)
		if( br.Intersects(PartitionRect(i, pcountw)) )
			m_partition[i]->Sub(brushidx);
}

int	cEdiMap::PartitionGet( iRect& rect, int* buffer, int buffersize )
{
	assert(buffer!=NULL);
	assert(buffersize>0);
	int pcountw = PartitionCountW();
	int count = 0;
	for(size_t i=0; i<m_partition.size(); i++)
		if(rect.Intersects(PartitionRect(i, pcountw)) )
		{
			buffer[count] = i;
			count++;
			if(count==buffersize) break;
		}
	return count;
}

void cEdiMap::PartitionFix( int brushidx1, int brushidx2, int delta )
{
	for(cPartitionCel* pcel: m_partition)
		for(int j=0; j<pcel->m_count; j++)
			if( pcel->m_data[j]>=brushidx1 && pcel->m_data[j]<=brushidx2 )
				pcel->m_data[j] += delta;
}

BOOL cEdiMap::PartitionRepartition()
{
	// force all clean
	for(cPartitionCel *c: m_partition) c->m_count = 0;
	// repartition all brushes
	BOOL ok = TRUE;
	for(int i=0; i<m_brushcount; i++)
		ok &= PartitionAdd(i);
	return ok;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// MARKERS
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::MarkerToggle( int x, int y )
{
	int dist = -1;
	int mark = MarkerClosest( m_camx, m_camy, dist );
	if(mark!=-1 && dist==0) // remove existing mark
	{
		m_marker.erase(m_marker.begin() + mark);
	}
	else // add mark
	{
		m_marker.push_back(tMarker(x,y,m_camz));
	}
}
void cEdiMap::MarkerGoto( int dir )				
{
	int dist = -1;
	int mark = MarkerClosest( m_camx, m_camy, dist );
	if(mark==-1) return; // no markers
	if(dist==0) // select next
	{
		mark = mark+dir;
		if(mark<0) mark = m_marker.size()-1;
		if(mark>m_marker.size()-1) mark = 0;
	}
	m_camx = m_marker[mark].x;
	m_camy = m_marker[mark].y;
	m_camz = m_marker[mark].z;
	m_refresh = TRUE;
}

int	cEdiMap::MarkerClosest( int x, int y, int &dist )
{
	int mark=-1;
	int mind = -1;
	for(int i=0;i<m_marker.size();i++)
	{
		int mx = m_marker[i].x;
		int my = m_marker[i].y;
		double d = (double)(x-mx)*(double)(x-mx)+(double)(y-my)*(double)(y-my);
		d=sqrt(d);
		if(mind==-1 || (int)d<mind)
		{
			mark=i;
			mind=(int)d;
		}
	}
	dist = mind;
	return mark;
}

void cEdiMap::MarkerClear()
{
	m_marker.clear();
}

void cEdiMap::MarkerResize()
{
	// remove markers out of the new map size
	for(int i=0;i<m_marker.size();i++)
	{
		if(!MarkerTest(i))	
		{
			m_marker.erase(m_marker.begin() + i);
			i--;
		}
	}
}

BOOL cEdiMap::MarkerTest( int idx )
{
	if(idx<0 || idx>=m_marker.size()) return FALSE;
	int camx = m_marker[idx].x;
	int camy = m_marker[idx].y;
	int camz = m_marker[idx].z;
	int camw = m_vieww/camz;
	int camh = m_viewh/camz;
	if(camx<camw/2) return FALSE;
	if(camy<camh/2) return FALSE;
	if(camx>m_mapw-camw/2) return FALSE;
	if(camy>m_maph-camh/2) return FALSE;
	return TRUE;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// SELECTION GOTO
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::SelectionRefresh()
{
	m_selectcount=0;
	for(int i=0;i<m_brushcount;i++)
	{
		if(m_brush[i].m_data[BRUSH_SELECT]) 
			m_selectcount++;
	}
}

void cEdiMap::SelectionGoto( int dir )
{
	if(m_brushcount==0) return;
	assert(dir==-1 || dir==1);
	if(m_selectgoto<=-1) m_selectgoto = m_brushcount-1;
	if(m_selectgoto>=m_brushcount) m_selectgoto = 0;
	int i = m_selectgoto;
	while(TRUE)
	{
		i+=dir;
		if(i<=-1) i=m_brushcount-1;
		if(i>=m_brushcount) i=0;
		if(m_brush[i].m_data[BRUSH_SELECT]) 
		{
			m_camx = m_brush[i].m_data[BRUSH_X];
			m_camy = m_brush[i].m_data[BRUSH_Y];
			m_refresh = TRUE;
			m_selectgoto = i;
			return;
		}
		if(i==m_selectgoto) return;
	}
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// OTHERS
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::DrawGrid( iRect &view )
{
	int i;
	iRect view2;
	R9_SetBlend(Blend::Alpha);

	// TILE GRID
	int grid = EdiApp()->m_gridsize;
	if( EdiApp()->m_grid && grid!=0 ) 
	{
		// snap view
		view2 = view; 
		view2.p1 = view2.p1 / grid * grid;
		view2.p2 = view2.p2 / grid * grid;
		
		// vertical
		for( i=view2.p1.x; i<=view2.p2.x; i+=grid )
		{
			int x = m_camz * (i - (m_camx - (m_vieww/m_camz)/2));
			R9_DrawLine( fV2(x,0), fV2(x,m_viewh), EdiApp()->GetColor(EDI_COLORGRID1) );
		}

		// horizontal
		for( i=view2.p1.y; i<=view2.p2.y; i+=grid )
		{
			int y = m_camz * (i - (m_camy - (m_viewh/m_camz)/2));
			R9_DrawLine( fV2(0,y), fV2(m_vieww,y), EdiApp()->GetColor(EDI_COLORGRID1) );
		}
	}

	// ROOM GRID
	int gridx = m_roomw;
	int gridy = m_roomh;
	if( m_roomgrid && gridx!=0 && gridy!=0 ) 
	{
		iV2 gr(gridx, gridy);
		// snap view
		view2 = view; 
		view2.p1 = view2.p1 / gr * gr;
		view2.p2 = view2.p2 / gr * gr;
		
		// vertical
		for( i=view2.p1.x; i<=view2.p2.x; i+=gridx )
		{
			int x = m_camz * (i - (m_camx - (m_vieww/m_camz)/2));
			R9_DrawLine( fV2(x,0), fV2(x,m_viewh), EdiApp()->GetColor(EDI_COLORGRID2) );
		}

		// horizontal
		for( i=view2.p1.y; i<=view2.p2.y; i+=gridy )
		{
			int y = m_camz * (i - (m_camy - (m_viewh/m_camz)/2));
			R9_DrawLine( fV2(0,y), fV2(m_vieww,y), EdiApp()->GetColor(EDI_COLORGRID2));
		}
	}

}

void cEdiMap::DrawAxes( int x, int y )
{
	if(!EdiApp()->m_axes) return;

	CAM2VIEW(x,y);
	x += VIEWX;
	y += VIEWY;
	R9_DrawLine( fV2(x,VIEWY), fV2(x,VIEWY+VIEWH), EdiApp()->GetColor(EDI_COLORGRID3) );
	R9_DrawLine( fV2(VIEWX,y), fV2(VIEWX+VIEWW,y), EdiApp()->GetColor(EDI_COLORGRID3) );
}

void cEdiMap::DrawScrollers()
{
	fRect rc;
	rc = GetHScrollRect();
	R9_DrawBar(rc, (EdiApp()->GetColor(EDI_COLORBACK2) & 0x00ffffff) | 0x60000000 );
	rc = GetVScrollRect();
	R9_DrawBar(rc, (EdiApp()->GetColor(EDI_COLORBACK2) & 0x00ffffff) | 0x60000000);
}

iRect cEdiMap::GetHScrollRect()
{
	float x1 = (float)(m_camx-(m_vieww/m_camz)/2) / m_mapw * m_vieww;
	float x2 = (float)(m_camx+(m_vieww/m_camz)/2) / m_mapw * m_vieww;
	iRect rc( (float)m_viewx+x1, (float)m_viewy+m_viewh+2, (float)m_viewx+x2, (float)m_viewy+m_viewh+VIEWB );	
	return rc;
}

iRect cEdiMap::GetVScrollRect()
{
	float y1 = (float)(m_camy-(m_viewh/m_camz)/2) / m_maph * m_viewh;
	float y2 = (float)(m_camy+(m_viewh/m_camz)/2) / m_maph * m_viewh;
	iRect rc( (float)m_viewx+m_vieww+2, (float)m_viewy+y1, (float)m_viewx+m_vieww+VIEWB, (float)m_viewy+y2 );
	return rc;
}

void cEdiMap::CheckMapView()
{
	m_viewx = VIEWB;
	m_viewy = VIEWB+32;
	m_vieww = R9_GetWidth() - 2*VIEWB;
	m_viewh = R9_GetHeight() - (32+16+2*VIEWB);

	if(m_vieww>m_mapw) m_vieww=m_mapw;
	if(m_viewh>m_maph) m_viewh=m_maph;
	m_viewx = VIEWB + (R9_GetWidth() - m_vieww - (2*VIEWB) ) / 2;
	m_viewy = VIEWB + 32 + (R9_GetHeight() - m_viewh - (32+16+2*VIEWB) ) / 2;
}



//////////////////////////////////////////////////////////////////////////////////////////////////
// SAVE MAP IMAGE
//////////////////////////////////////////////////////////////////////////////////////////////////
bool cEdiMap::SaveMapImage(const std::string & filename )
{
	if(!m_target) return false;

	// CREATE IMGHUGE
	r9Img imghuge;
	imghuge.m_pf = R9_PF_RGB;
	imghuge.m_width = m_mapw;
	imghuge.m_height = m_maph;
	if(!R9_ImgCreate(&imghuge)) return false;

	// LOOP
	int w=256;
	int h=256;
	int y=0;
	while(y<m_maph)
	{
		int x=0;
		while(x<m_mapw)
		{
			// draw in render target
			if(R9_BeginScene(m_target))
			{
				R9_Clear(EdiApp()->GetColor(EDI_COLORMAP));

				// DRAW
				int camz = m_camz;
				int camx = m_camx;
				int camy = m_camy;
				int vieww = m_vieww;
				int viewh = m_viewh;
				m_camz = 1;
				m_camx = x + 128;
				m_camy = y + 128;
				m_vieww = w;
				m_viewh = h;
				iRect view(x, y, x + w, y + h); // camera view in map
				BrushDrawExtra( view );
				DrawGrid( view );
				m_camz = camz;
				m_camx = camx;
				m_camy = camy;
				m_vieww = vieww;
				m_viewh = viewh;

				// END DRAW
				R9_Flush();
				R9_EndScene();
			}

			fRect rect(x,y,x+w,y+h);
			R9_CopyTargetToImage(m_target,&imghuge,&rect);

			x+=w;
		}

		y+=h;
	}

	bool ok = R9_ImgSaveFile(filename.c_str(), &imghuge);
	R9_ImgDestroy(&imghuge);
	return ok;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

bool cEdiMap::Load( const std::string & filename )
{
	dlog(LOGAPP, L"Loading map \"%S\"\n",filename.c_str());

	if(!LoadMap(filename)) { dlog(LOGERR, L"Loading map FAILED!\n\n"); return false; }
	dlog(LOGAPP, L"Loading map SUCCESSFUL!\n\n");
	return true;
}


#define ERROR_CHUNK( info )	{ files->FileClose(file); dlog(LOGAPP, L"brocken chunk (%S)\n", info); return false; }
bool cEdiMap::LoadMap(const std::string &filename)
{

	// open file
	F9FILE file = files->OpenFile(filename);
	if(!file) { dlog(LOGAPP, L"  map file not found.\n"); return false; }
	file->Seek(0,2);
	int64 filesize = file->Tell();
	file->Seek(0,0);

	// read chunks
	int64 size;
	int chunkid=0;
	int chunksize=0;
	int chunkcount=0;
	int count_brush = 0;
	int count_obj = 0;
	char* buffer;

	while(true)
	{
		if( file->Read(&chunkid, 4)!=4 )	{ ERROR_CHUNK("header"); }
		if( file->Read(&chunksize,4)!=4 )	{ ERROR_CHUNK("header"); }
		
		switch(chunkid)
		{
			case MAP_CHUNKID:
			{
				if( chunksize!=strlen(MAP_ID) )	{ ERROR_CHUNK("size"); }
				buffer = (char*)malloc(chunksize);
				size = 0;
				size += file->Read(buffer, chunksize);
				if(size!=chunksize) { free(buffer);  ERROR_CHUNK("size"); }

				if(memcmp(buffer,MAP_ID,chunksize)!=0) { dlog(LOGAPP, L"invalid map id: '%S' (current version: '%S')\n", buffer, MAP_ID); free(buffer); files->FileClose(file); return false; }
				free(buffer);
				break;
			}

			case MAP_CHUNKINFO2:
			{
				if( chunksize!= 6*4 ) { ERROR_CHUNK("size"); }
				size = 0;
				size += file->Read(&m_mapw, 4);
				size += file->Read(&m_maph, 4);
				size += file->Read(&g_map.m_roomw, 4);
				size += file->Read(&g_map.m_roomh, 4);
				size += file->Read(&g_map.m_camx, 4);
				size += file->Read(&g_map.m_camy, 4);
				if( size!=chunksize ) { ERROR_CHUNK("size"); }
				
				break;
			}

			case MAP_CHUNKMARKERS2:
			{
				file->Seek(chunksize,1); // skip
				break;
			}

			case MAP_CHUNKBRUSHES2:
			{
				if(chunksize%(BRUSH_MAX*4)!=0) { ERROR_CHUNK("size"); }
				int brushcount = chunksize/(BRUSH_MAX*4);
				for(int i = 0; i < brushcount;i++)
				{
					if(i != g_map.BrushNew()) { ERROR_CHUNK("index"); }
					for(int j = 0; j  < BRUSH_MAX; j++)
					{	
						int val = 0;
						if(!file->Read(&val, sizeof(val))) { ERROR_CHUNK("size"); }

						g_map.m_brush[i].m_data[j] = val;
					}
				}
				break;
			}
			
			default:
			{
				dlog(LOGAPP, L"  unknown chunk: id=%x size=%i\n", chunkid, chunksize);
				if(chunksize>0) file->Seek(chunksize,1);
			}
		}
		if( file->Tell()>=filesize) break;

	}

	files->FileClose(file);
	dlog(LOGAPP, L"  map=%ix%i, room=%ix%i, brushes=%i, objects=%i\n", m_mapw, m_maph, g_map.m_roomw, g_map.m_roomh, count_brush, count_obj );

	int ret=g_map.Resize(m_mapw, m_maph); 
	EdiApp()->UndoReset();

	return true;
}
