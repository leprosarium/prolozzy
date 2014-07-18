//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiMap.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"

#include "eInput.h"

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
	return A1 = static_cast<int>(g_map.m_brush.size());
}

bool cEdiMap::UnifyBrush(PlTerm t, tBrush * b)
{
	if(!(t = brush))
		return false;
	return t[1] = b;
}

PREDICATE_M(map, brushIdx, 2)
{
	return g_map.UnifyBrush(A2, g_map.m_brush[static_cast<int>(A1)]);
}

PREDICATE_NONDET_M(map, brush, 1)
{ 
	auto call = PL_foreign_control(handle);
	if(call == PL_PRUNED)
		return true;
	PlTerm t = A1;
	if(!(t = g_map.brush))
		return false;
	PlTerm br = t[1];
	if (br.type() != PL_VARIABLE)
			return std::find(g_map.m_brush.begin(), g_map.m_brush.end(), g_map.brushPtrNoEx(br)) != g_map.m_brush.end();
	size_t idx = call == PL_FIRST_CALL ? 0 : PL_foreign_context(handle);
	if(idx < g_map.m_brush.size() && (br = g_map.m_brush[idx]))
		if(++idx == g_map.m_brush.size())
			return true;
		else
			PL_retry(idx);
	return false;
}
	
#define BRUSH_PROP(Prop, PROP)\
GET_BRUSH_PROP(Prop, PROP)\
SET_BRUSH_PROP(Prop, PROP)

#define GET_BRUSH_PROP(Prop, PROP) PREDICATE_M(brush, get##Prop, 2) { return A2 = g_map.brushPtr(A1)->PROP; }
#define SET_BRUSH_PROP(Prop, PROP) PREDICATE_M(brush, set##Prop, 2) { g_map.brushPtr(A1)->PROP = A2; return true; }

BRUSH_PROP(Layer, layer)
BRUSH_PROP(X, pos.x)
BRUSH_PROP(Y, pos.y)
BRUSH_PROP(W, size.x)
BRUSH_PROP(H, size.y)
BRUSH_PROP(Tile, tile)
BRUSH_PROP(Frame, frame)
BRUSH_PROP(MapX1, map.p1.x)
BRUSH_PROP(MapY1, map.p1.y)
BRUSH_PROP(MapX2, map.p2.x)
BRUSH_PROP(MapY2, map.p2.y)
BRUSH_PROP(Flip, flip)
BRUSH_PROP(Scale, scale)
BRUSH_PROP(Material, material)
BRUSH_PROP(Draw, draw)
BRUSH_PROP(Delay, delay)
BRUSH_PROP(Anim, anim)
BRUSH_PROP(Collider, collider)

PREDICATE_M(brush, getColor, 2) 
{
	int64 color = static_cast<dword>(g_map.brushPtr(A1)->color);
	return A2 = color;
}

PREDICATE_M(brush, setColor , 2) 
{
	int64 color = A2;
	g_map.brushPtr(A1)->color = static_cast<dword>(color);
	return true;
}

PREDICATE_M(brush, getShader, 2) 
{
	return A2 = static_cast<int>(g_map.brushPtr(A1)->shader);
}

PREDICATE_M(brush, setShader, 2) 
{
	g_map.brushPtr(A1)->shader =  static_cast<Blend>(static_cast<int>(A2));
	return true;
}

PREDICATE_M(brush, getID, 2) 
{
	const std::string & id = g_map.brushPtr(A1)->id;
	return id.empty() ? false : (A2 = id);
}

PREDICATE_M(brush, setID , 2) 
{
	g_map.brushPtr(A1)->id = static_cast<const char *>(A2);
	return true;
}

PREDICATE_M(brush, getDisable, 2) 
{
	return A2 = g_map.brushPtr(A1)->disable ? 1 : 0;
}

PREDICATE_M(brush, setDisable , 2) 
{
	g_map.brushPtr(A1)->disable = static_cast<int>(A2) != 0;
	return true;
}

PREDICATE_M(brush, getSelect, 2) 
{
	return A2 = g_map.brushPtr(A1)->select ? 1 : 0;
}

PREDICATE_M(brush, setSelect , 2) 
{
	g_map.brushPtr(A1)->select = static_cast<int>(A2) != 0;
	return true;
}

PREDICATE_M(brush, getCollision, 2) 
{
	return A2 = g_map.brushPtr(A1)->collision ? 1 : 0;
}

PREDICATE_M(brush, setCollision, 2) 
{
	g_map.brushPtr(A1)->collision = static_cast<int>(A2) != 0;
	return true;
}

PREDICATE_M(map, brushNew, 0)
{
	g_map.BrushNew();
	return true;
}

PREDICATE_M(map, brushNew, 1)
{
	int idx = g_map.BrushNew();
	return g_map.UnifyBrush(A1, g_map.m_brush[idx]);
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
	return true;
}


PREDICATE_M(map, resize, 2)
{
	int ret = g_map.Resize(A1, A2); 
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
	return true;
}

PREDICATE_M(map, saveImage, 1)
{
	bool ret = g_map.SaveMapImage(WideStringToMultiByte(A1));
	g_map.m_refresh = TRUE;
	return ret;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////

cEdiMap::cEdiMap() : brush("brush", 1)
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
	if(!m_target) {	elog::app() << "can't create render target." << std::endl; return FALSE; }

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
		R9_Clear(Editor::app->GetColor(EDI_COLORMAP));
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
	
	for(auto b: m_brush) delete b;
	m_brush.clear();
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// UPDATE
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::Update( float dtime )
{

	int stepx = Editor::app->m_gridsize;
	int stepy = Editor::app->m_gridsize;
	iV2 m = Editor::app->GetMousePos() - iV2(VIEWX, VIEWY);
	int mz = einput->mouse.axe[2].delta;
	bool shift	= einput->shift();
	bool alt	= einput->alt() || einput->mouseValue(2);
	bool ctrl	= einput->ctrl();

	// navigation
	int dx = 0;
	int dy = 0;
	
	// key
	if(ctrl) 
	{
		stepx=m_roomw;
		stepy=m_roomh;
	}
	if( einput->keyValue(DIK_RIGHT) )	dx = stepx;
	if( einput->keyValue(DIK_LEFT) )	dx =-stepx;
	if( einput->keyValue(DIK_DOWN) )	dy = stepy;
	if( einput->keyValue(DIK_UP) )	dy =-stepy;

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
	if(mz!=0 && iRect(VIEWW, 0, VIEWW+VIEWB, VIEWH).IsInside(m))
	{
		if(mz<0) dy = stepy;
		if(mz>0) dy =-stepy;
		Editor::app->m_mscrolly = 0;
		Editor::app->m_mscrollx = 0;
	}
	
	// horizontal scroll
	if( mz!=0 && iRect(0, VIEWH, VIEWW, VIEWH+VIEWB).IsInside(m))
	{
		if(mz<0) dx = stepx;
		if(mz>0) dx =-stepx;
		Editor::app->m_mscrolly = 0;
		Editor::app->m_mscrollx = 0;
	}

	// scroll with the Scroll function (from WM_MOUSEWHEEL)
	if(Editor::app->m_mscrolly!=0) { dy = Editor::app->m_mscrolly * stepy; Editor::app->m_mscrolly = 0; }
	if(Editor::app->m_mscrollx!=0) { dx = Editor::app->m_mscrollx * stepx; Editor::app->m_mscrollx = 0; }

	// scrolling
	iRect rc;
	m = Editor::app->GetMousePos();
	if(!m_scrolling && einput->isMouseDown(0))
	{
		rc = GetHScrollRect();
		if(rc.IsInside(fV2(m))) 
		{
			m_scrolling = 1;
			m_scrollofs = m.x-rc.p1.x;
			Editor::app->SetCursor(App::Cursor::Hand);
		}
		else
		{
			rc = GetVScrollRect();
			if(rc.IsInside(fV2(m)))
			{
				m_scrolling = 2;
				m_scrollofs = m.y-rc.p1.y;
				Editor::app->SetCursor(App::Cursor::Hand);
			}
		}
	}
	else
	if(m_scrolling && !einput->mouseValue(0))
	{
		m_scrolling = 0;
		Editor::app->SetCursor(App::Cursor::Arrow);
	}

	if(m_scrolling==1) // scroll horizontal
	{
		rc = GetHScrollRect();
		dx = (m.x-m_scrollofs)-rc.p1.x;
		dx = (dx/Editor::app->m_gridsize)*Editor::app->m_gridsize;
	}
	else
	if(m_scrolling==2) // scroll vertical
	{
		rc = GetVScrollRect();
		dy = (m.y-m_scrollofs)-rc.p1.y;
		dy = (dy/Editor::app->m_gridsize)*Editor::app->m_gridsize;
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
		if(einput->isKeyDown(DIK_A))			{ Editor::app->m_axes = !Editor::app->m_axes; }
		if(einput->isKeyDown(DIK_S))			{ Editor::app->m_snap = !Editor::app->m_snap; }
		if(einput->isKeyDown(DIK_G))			{ Editor::app->m_grid = !Editor::app->m_grid; m_refresh=TRUE; }
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
		R9_Clear(Editor::app->GetColor(EDI_COLORMAP));
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
	bool ok = PartitionRepartition();

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
	m_brush.push_back(new tBrush());
	return m_brush.size()-1;
}

void cEdiMap::BrushIns( int idx, tBrush& brush )
{
	assert(validBrushIdx(idx));
	BrushNew();
	for(int i=m_brush.size()-1;i>idx;i--) m_brush[i] = m_brush[i-1];
	*m_brush[idx]=brush;
	if(m_brush[idx]->select) m_selectcount++;
}

void cEdiMap::TakeBrush(tBrush * b)
{
	auto it = std::find(m_brush.begin(), m_brush.end(), b);
	if(it == m_brush.end())
		return;
	if(b->select) m_selectcount--;
	m_brush.erase(it);
	brushvis.clear();
}

void cEdiMap::BrushDel(tBrush * brush)
{
	if(brush->select) m_selectcount--;
	PlTermv a(1);
	UnifyBrush(a[0], brush);
	g_gui->ScriptPrologDo("brush", "delete", a);
	delete brush;
	auto it = std::find(m_brush.begin(), m_brush.end(), brush);
	if(it == m_brush.end())
		return;
	m_brush.erase(it);
	brushvis.clear();
}

void cEdiMap::BrushDrawExtra( iRect& view )
{
	m_count_brushdraw = 0;
	m_count_brushcheck = 0;



	int partition[32];
	int partitioncount = PartitionGet(view, partition,32);
	if( partitioncount==0 ) return;

	// brushvis is a draw buffer that holds indexes to brushes accepted for draw; will be order before draw
	brushvis.clear();

	// check partitions for draw
	for( int p=0; p<partitioncount; p++ )
	{
		int pidx = partition[p];
		for(auto brush: *m_partition[pidx])
		{

			m_count_brushcheck++;
			if (Editor::app->layers.IsHidden(brush->layer)) continue;
			if(!view.Intersects(brush->rect())) continue;

			brushvis.push_back(brush); // store in drawbuffer
		}
	}

	std::sort(brushvis.begin(), brushvis.end());
	brushvis.erase(std::unique(brushvis.begin(), brushvis.end()), brushvis.end());

	std::sort(brushvis.begin(), brushvis.end(), [](tBrush * b1, tBrush * b2) { return  b1->layer < b2->layer; });

	// draw drawbuffer
	tBrush brushtemp;
	for(auto brush: brushvis)
	{
		if( m_hideselected && brush->select ) continue;
		brushtemp = *brush;
		PlTermv br(1);
		g_map.UnifyBrush(br[0], & brushtemp);
		if(!g_gui->ScriptPrologDo("mod", "brushDraw", br)) continue;
		iV2 p = m_camz  * (brushtemp.pos - view.p1);
		g_paint.DrawBrushAt( &brushtemp, p.x, p.y, (float)m_camz );
		m_count_brushdraw++;
	}
}

tBrush * cEdiMap::BrushPick(const iV2 & p) const
{
	auto it = find_if(m_brush.rbegin(), m_brush.rend(), 
		[p](tBrush *brush) -> bool
	{ 
		return !Editor::app->layers.IsHidden(brush->layer) && brush->rect().IsInside(p);
	});
	return it == m_brush.rend() ? nullptr : *it;
}

void cEdiMap::BrushClear()
{
	for(auto b: m_brush) delete b;
	m_brush.clear();
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
	for(auto c: m_partition) delete c;
	m_partition.clear(); 
}

bool cEdiMap::PartitionAdd( tBrush * b)
{
	int pcountw = PartitionCountW();
	iRect br = b->rect();
	bool ok = false;
	for(size_t i=0; i<m_partition.size(); i++)
		if( br.Intersects(PartitionRect(i, pcountw)) )
		{	
			m_partition[i]->Add(b);
			ok = true;
		}
	if(!ok)
		elog::app() << "Brush # " << b << " (" << br.p1.x << ", " << br.p1.y << ")-(" << br.p2.x << ", " << br.p2.y << ") out of bounds" << std::endl;
	return ok;
}

void cEdiMap::PartitionDel( tBrush * b )
{
	int pcountw = PartitionCountW();
	iRect br =b->rect();
	for(size_t i=0; i<m_partition.size(); i++)
		if( br.Intersects(PartitionRect(i, pcountw)) )
			m_partition[i]->Del(b);
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

bool cEdiMap::PartitionRepartition()
{
	for(auto c: m_partition) c->clear();
	bool ok = true;
	for(auto b: m_brush) ok &= PartitionAdd(b);
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
	for(tBrush * b: m_brush)
		if(b->select) 
			m_selectcount++;
}

void cEdiMap::SelectionGoto( int dir )
{
	if(m_brush.empty()) return;
	assert(dir==-1 || dir==1);
	if(m_selectgoto<=-1) m_selectgoto = m_brush.size()-1;
	if(m_selectgoto>=m_brush.size()) m_selectgoto = 0;
	int i = m_selectgoto;
	while(TRUE)
	{
		i+=dir;
		if(i<=-1) i=m_brush.size()-1;
		if(i>=m_brush.size()) i=0;
		if(m_brush[i]->select) 
		{
			m_camx = m_brush[i]->pos.x;
			m_camy = m_brush[i]->pos.y;
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
	int grid = Editor::app->m_gridsize;
	if( Editor::app->m_grid && grid!=0 ) 
	{
		// snap view
		view2 = view; 
		view2.p1 = view2.p1 / grid * grid;
		view2.p2 = view2.p2 / grid * grid;
		
		// vertical
		for( i=view2.p1.x; i<=view2.p2.x; i+=grid )
		{
			int x = m_camz * (i - (m_camx - (m_vieww/m_camz)/2));
			R9_DrawLine( fV2(x,0), fV2(x,m_viewh), Editor::app->GetColor(EDI_COLORGRID1) );
		}

		// horizontal
		for( i=view2.p1.y; i<=view2.p2.y; i+=grid )
		{
			int y = m_camz * (i - (m_camy - (m_viewh/m_camz)/2));
			R9_DrawLine( fV2(0,y), fV2(m_vieww,y), Editor::app->GetColor(EDI_COLORGRID1) );
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
			R9_DrawLine( fV2(x,0), fV2(x,m_viewh), Editor::app->GetColor(EDI_COLORGRID2) );
		}

		// horizontal
		for( i=view2.p1.y; i<=view2.p2.y; i+=gridy )
		{
			int y = m_camz * (i - (m_camy - (m_viewh/m_camz)/2));
			R9_DrawLine( fV2(0,y), fV2(m_vieww,y), Editor::app->GetColor(EDI_COLORGRID2));
		}
	}

}

void cEdiMap::DrawAxes( int x, int y )
{
	if(!Editor::app->m_axes) return;

	CAM2VIEW(x,y);
	x += VIEWX;
	y += VIEWY;
	R9_DrawLine( fV2(x,VIEWY), fV2(x,VIEWY+VIEWH), Editor::app->GetColor(EDI_COLORGRID3) );
	R9_DrawLine( fV2(VIEWX,y), fV2(VIEWX+VIEWW,y), Editor::app->GetColor(EDI_COLORGRID3) );
}

void cEdiMap::DrawScrollers()
{
	fRect rc;
	rc = GetHScrollRect();
	R9_DrawBar(rc, (Editor::app->GetColor(EDI_COLORBACK2) & 0x00ffffff) | 0x60000000 );
	rc = GetVScrollRect();
	R9_DrawBar(rc, (Editor::app->GetColor(EDI_COLORBACK2) & 0x00ffffff) | 0x60000000);
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
				R9_Clear(Editor::app->GetColor(EDI_COLORMAP));

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

	bool ok = R9_ImgSaveFile(filename, &imghuge);
	R9_ImgDestroy(&imghuge);
	return ok;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

bool cEdiMap::Load( const std::string & filename )
{
	elog::app() << "Loading map \"" << filename.c_str() << "\"" << std::endl;

	if(!LoadMap(filename)) { elog::err() << "Loading map FAILED!" << std::endl << std::endl; return false; }
	elog::app() << "Loading map SUCCESSFUL!" << std::endl << std::endl;
	return true;
}


#define ERROR_CHUNK( info )	{ files->FileClose(file); elog::app() << "brocken chunk (" << info << ")" << std::endl; return false; }
bool cEdiMap::LoadMap(const std::string &filename)
{

	// open file
	F9FILE file = files->OpenFile(filename);
	if(!file) { elog::app() << "  map file not found." << std::endl; return false; }
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
				buffer = new char[chunksize];
				size = 0;
				size += file->Read(buffer, chunksize);
				if(size!=chunksize) { delete [] buffer;  ERROR_CHUNK("size"); }

				if(memcmp(buffer,MAP_ID,chunksize)!=0) { elog::app() << "invalid map id: '" << buffer << "' (current version: '" << MAP_ID << "')" << std::endl; delete [] buffer; files->FileClose(file); return false; }
				delete [] buffer;
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
					tBrush * b = g_map.m_brush[i];
					for(int j = 0; j  < BRUSH_MAX; j++)
					{	
						int val = 0;
						if(!file->Read(&val, sizeof(val))) { ERROR_CHUNK("size"); }

						if(j == BRUSH_LAYER) b->layer = val;
						else if(j == BRUSH_X) b->pos.x = val;
						else if(j == BRUSH_Y) b->pos.y = val;
						else if(j == BRUSH_W) b->size.x = val;
						else if(j == BRUSH_H) b->size.y = val;
						else if(j == BRUSH_TILE) b->tile = val;
						else if(j == BRUSH_FRAME) b->frame = val;
						else if(j == BRUSH_MAP) b->map.p1.x = val;
						else if(j == BRUSH_MAP+1) b->map.p1.y = val;
						else if(j == BRUSH_MAP+2) b->map.p2.x = val;
						else if(j == BRUSH_MAP+3) b->map.p2.y = val;
						else if(j == BRUSH_FLIP) b->flip = val;
						else if(j == BRUSH_COLOR) b->color = val;
						else if(j == BRUSH_SHADER) b->shader = static_cast<Blend>(val);
						else if(j == BRUSH_SCALE) b->scale = val;
						else if(j == BRUSH_SELECT) b->select = val != 0;
						else if(j == BRUSH_ID) {std::ostringstream o; o << val; b->id = o.str(); }
						else if(j == BRUSH_MATERIAL) b->material = val;
						else if(j == BRUSH_DRAW) b->draw = val;
						else if(j == BRUSH_DISABLE) b->disable = val != 0;
						else if(j == BRUSH_DELAY) b->delay = val;
						else if(j == BRUSH_ANIM) b->anim = val;
						else if(j == BRUSH_COLLIDER) b->collider = val;
					}
				}
				break;
			}
			
			default:
			{
				elog::app() << "  unknown chunk: id=" << std::hex << chunkid << " size=" << chunksize << std::endl;
				if(chunksize>0) file->Seek(chunksize,1);
			}
		}
		if( file->Tell()>=filesize) break;

	}

	files->FileClose(file);
	elog::app() << "  map=" << m_mapw << "x" << m_maph << ", room=" << g_map.m_roomw << "x" << g_map.m_roomh << ", brushes=" << count_brush << ", objects=" << count_obj << std::endl;

	int ret=g_map.Resize(m_mapw, m_maph); 

	return true;
}
