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
	return A1 = g_map.mapSize.x;
}

PREDICATE_M(map, getMapH, 1)
{
	return A1 = g_map.mapSize.y;
}

PREDICATE_M(map, getRoomW, 1)
{
	return A1 = g_map.roomSize.x;
}

PREDICATE_M(map, getRoomH, 1)
{
	return A1 = g_map.roomSize.y;
}

PREDICATE_M(map, getRoomGrid, 1)
{
	return A1 = g_map.m_roomgrid;
}

PREDICATE_M(map, getCamX, 1)
{
	return A1 = g_map.cam.x;
}

PREDICATE_M(map, getCamY, 1)
{
	return A1 = g_map.cam.y;
}

PREDICATE_M(map, getZoom, 1)
{
	return  A1 = g_map.camScale;
}

PREDICATE_M(map, getSelect, 1)
{
	return A1 = g_map.m_selectcount;
}

PREDICATE_M(map, setRoomW, 1)
{
	g_map.roomSize.x = A1;
	return true;
}

PREDICATE_M(map, setRoomH, 1)
{
	g_map.roomSize.y = A1;
	return true;
}

PREDICATE_M(map, setRoomGrid, 1)
{
	g_map.m_roomgrid = A1;
	return true;
}

PREDICATE_M(map, setCamX, 1)
{
	g_map.cam.x = A1;
	return true;
}

PREDICATE_M(map, setCamY, 1)
{
	g_map.cam.y = A1;
	return true;
}

PREDICATE_M(map, setZoom, 1)
{
	g_map.camScale = A1;
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
	return g_map.UnifyBrush(A1, g_map.BrushNew());
}

PREDICATE_M(map, repartition, 0)
{
	return g_map.partitions.Repartition(g_map.m_brush);
}

PREDICATE_M(map, refresh, 0) 
{
	g_map.Refresh();
	return true;
}

PREDICATE_M(map, reset, 0)
{
	g_map.Reset();
	return true;
}


PREDICATE_M(map, resize, 2)
{
	int ret = g_map.Resize(iV2(A1, A2)); 
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
	g_map.Refresh();
	return ret;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////

cEdiMap::cEdiMap() : 
	refresh(true),
	brush("brush", 1), 
	camScale(1), 
	m_roomgrid(), 
	m_hideselected()
{
	m_target		= NULL;

	// selection
	m_selectcount	= 0;
	m_selectgoto	= 0;
}

bool cEdiMap::Init()
{
	// create render target shader
	int width = R9_GetWidth();
	int height = R9_GetHeight();
	m_target = R9_TextureCreateTarget(width,height);
	if(!m_target) {	elog::app() << "can't create render target." << std::endl; return FALSE; }

	mapSize = MAP_SIZEDEFAULT;
	CheckMapView();

	cam = mapSize / 2;

	partitions.Init(mapSize);

	// clear the target
	if(R9_BeginScene(m_target))
	{
		R9_Clear(Editor::app->GetColor(EDI_COLORMAP));
		R9_EndScene();
	}

	return true;
}

void cEdiMap::Done()
{

	MarkerClear();
	partitions.Done();

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

	iV2 step(Editor::app->m_gridsize);
	iV2 m = Editor::app->GetMousePos() - g_map.view.p1;
	int mz = einput->mouse.axe[2].delta;
	bool shift	= einput->shift();
	bool alt	= einput->alt() || einput->mouseValue(2);
	bool ctrl	= einput->ctrl();

	// navigation
	iV2 d;

	// key
	if(ctrl) 
		step = roomSize;

	if (einput->keyValue(DIK_RIGHT))	d.x = step.x;
	if (einput->keyValue(DIK_LEFT))	d.x = -step.x;
	if (einput->keyValue(DIK_DOWN))	d.y = step.y;
	if (einput->keyValue(DIK_UP))	d.y = -step.y;

	// smart key hit delaying system
	static int keycnt=0; // key delay counter
	if (d != 0)
	{
		if( keycnt>0 ) // allow first hit
		{
			if(keycnt<400 ) 
			{
				d = 0; // big wait when first pressed
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
	if (mz != 0 && iRect(view.Width(), 0, view.Width() + viewBorder, view.Height()).IsInside(m))
	{
		if(mz<0) d.y = step.y;
		if(mz>0) d.y =-step.y;
		Editor::app->m_mscrolly = 0;
		Editor::app->m_mscrollx = 0;
	}
	
	// horizontal scroll
	if (mz != 0 && iRect(0, view.Height(), view.Width(), view.Height() + viewBorder).IsInside(m))
	{
		if(mz<0) d.x = step.x;
		if(mz>0) d.x =-step.x;
		Editor::app->m_mscrolly = 0;
		Editor::app->m_mscrollx = 0;
	}

	// scroll with the Scroll function (from WM_MOUSEWHEEL)
	if(Editor::app->m_mscrolly!=0) { d.y = Editor::app->m_mscrolly * step.y; Editor::app->m_mscrolly = 0; }
	if(Editor::app->m_mscrollx!=0) { d.x = Editor::app->m_mscrollx * step.x; Editor::app->m_mscrollx = 0; }

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
		d.x = (m.x-m_scrollofs)-rc.p1.x;
		d.x = (d.x/Editor::app->m_gridsize)*Editor::app->m_gridsize;
	}
	else
	if(m_scrolling==2) // scroll vertical
	{
		rc = GetVScrollRect();
		d.y = (m.y-m_scrollofs)-rc.p1.y;
		d.y = (d.y/Editor::app->m_gridsize)*Editor::app->m_gridsize;
	}


	// apply
	if (d != 0)
	{
		cam += d;
		Refresh();
	}

	// others
	if(!alt && !shift && !ctrl)
	{
		if(einput->isKeyDown(DIK_A))			{ Editor::app->m_axes = !Editor::app->m_axes; }
		if(einput->isKeyDown(DIK_S))			{ Editor::app->m_snap = !Editor::app->m_snap; }
		if(einput->isKeyDown(DIK_G))			{ Editor::app->m_grid = !Editor::app->m_grid; Refresh(); }
	}

	// bounds
	camScale = std::min(4, std::max(1, camScale));

	cam.Clip(iRect(0, mapSize).Inflate(camSize() / 2));

	if(refresh) Render(); // @HM is it safe for draw (needs to be after bounds checks) !
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// DRAW
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::Draw()
{
	// draw pre-rendered map
	if(!m_target) return; // render target is not supported
	R9_SetBlend(Blend::Opaque);
	R9_DrawSprite( fV2(view.p1), fRect(0, view.Size()), m_target );
	R9_Flush();
	R9_SetBlend(Blend::Alpha);

	// draw scrollers
	DrawScrollers();

	// draw marker
	int dist = 0;
	int mark = MarkerClosest(cam,dist);
	if(mark!=-1 && dist==0) // on marker
		R9_DrawBar(fRect(view.p1.x - viewBorder + 3, view.p1.y - viewBorder + 3, view.p1.x - 3, view.p1.y - 3), 0xffffffff);
}

void cEdiMap::Render()
{
	// draw in render target
	if(R9_BeginScene(m_target))
	{
		R9_Clear(Editor::app->GetColor(EDI_COLORMAP));
		iRect v = camRect();
		BrushDrawExtra(v);
		DrawGrid(v);
		R9_EndScene();
	}
	refresh = false;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// IO
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::Reset()
{

	MarkerClear();
	partitions.Done();
	BrushClear();

	mapSize = MAP_SIZEDEFAULT;
	CheckMapView();
	cam = mapSize / 2;

	partitions.Init(mapSize);

	Refresh();
	SelectionRefresh();

}

bool cEdiMap::Resize(const iV2 & sz)
{
	partitions.Done();
	
	mapSize = iV2(sz).Clip(iRect(MAP_SIZEMIN, MAP_SIZEMAX));
	CheckMapView();
	cam = mapSize / 2;

	partitions.Init(mapSize);
	bool ok = partitions.Repartition(m_brush);

	SelectionRefresh();
	MarkerResize();

	Refresh();
	return ok;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// BRUSHES
//////////////////////////////////////////////////////////////////////////////////////////////////
tBrush * cEdiMap::BrushNew()
{
	tBrush * b = new tBrush();
	m_brush.push_back(b);
	return b;
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

void cEdiMap::BrushDrawExtra( const iRect& view )
{
	partitions.Filter(view, brushvis);
	tBrush brushtemp;
	for(auto brush: brushvis)
	{
		if( m_hideselected && brush->select ) continue;
		brushtemp = *brush;
		PlTermv br(1);
		g_map.UnifyBrush(br[0], & brushtemp);
		if(!g_gui->ScriptPrologDo("mod", "brushDraw", br)) continue;
		iV2 p = camScale  * (brushtemp.pos - view.p1);
		g_paint.DrawBrushAt(&brushtemp, p.x, p.y, static_cast<float>(camScale));
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

void Partitions::Init(const iV2 & mapSize)
{
	Done();
	size = (mapSize + CellSize - 1) / CellSize;
	iV2 p;
	for (int x = 0, y = 0; y < size.y; ++y, p.y += CellSize)
	for (x = 0, p.x = 0; x < size.x; ++x, p.x += CellSize)
		cells.push_back(new Cell(iRect(p, p + CellSize)));
}

void Partitions::Done()
{
	for (auto c : cells) delete c;
	cells.clear();
}

bool Partitions::Add(tBrush * b)
{
	iRect br = b->rect();
	bool ok = false;
	for (auto cell : cells)
	if (br.Intersects(cell->rect))
		{
			cell->Add(b);
			ok = true;
		}
	if (!ok)
		elog::app() << "Brush # " << b << " (" << br.p1.x << ", " << br.p1.y << ")-(" << br.p2.x << ", " << br.p2.y << ") out of bounds" << std::endl;
	return ok;
}

void Partitions::Del(tBrush * b)
{
	iRect br = b->rect();
	for (auto cell : cells)
	if (br.Intersects(cell->rect))
		cell->Del(b);
}

void Partitions::Get(const iRect& rect, Cont & tmp, int maxsize) const
{
	int count = 0;
	for (auto cell: cells)
	if (rect.Intersects(cell->rect))
	{
		tmp.push_back(cell);
		count++;
		if (count == maxsize) break;
	}
}

bool Partitions::Repartition(const Brushes & brushes)
{
	for (auto c : cells) c->clear();
	bool ok = true;
	for (auto b : brushes) ok &= Add(b);
	return ok;
}

void Partitions::Filter(const iRect & view, Brushes & vis) const
{
	vis.clear();
	Cont partition;
	Get(view, partition, 32);
	if (partition.empty()) return;

	for (auto cell : partition)
		for (auto brush : * cell)
			if (! Editor::app->layers.IsHidden(brush->layer) && view.Intersects(brush->rect()))
				vis.push_back(brush);

	std::sort(vis.begin(), vis.end());
	vis.erase(std::unique(vis.begin(), vis.end()), vis.end());

	std::sort(vis.begin(), vis.end(), [](tBrush * b1, tBrush * b2) { return  b1->layer < b2->layer; });
}
//////////////////////////////////////////////////////////////////////////////////////////////////
// MARKERS
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::MarkerToggle( const iV2 & p)
{
	int dist = -1;
	int mark = MarkerClosest(cam, dist );
	if(mark!=-1 && dist==0) // remove existing mark
	{
		m_marker.erase(m_marker.begin() + mark);
	}
	else // add mark
	{
		m_marker.push_back(tMarker(p.x, p.y, camScale));
	}
}
void cEdiMap::MarkerGoto( int dir )				
{
	int dist = -1;
	int mark = MarkerClosest(cam, dist );
	if(mark==-1) return; // no markers
	if(dist==0) // select next
	{
		mark = mark+dir;
		if(mark<0) mark = m_marker.size()-1;
		if(mark>m_marker.size()-1) mark = 0;
	}
	cam.x = m_marker[mark].x;
	cam.y = m_marker[mark].y;
	camScale = m_marker[mark].z;
	Refresh();
}

int	cEdiMap::MarkerClosest( const iV2 & p, int &dist )
{
	int mark=-1;
	int mind = -1;
	for(int i=0;i<m_marker.size();i++)
	{
		int mx = m_marker[i].x;
		int my = m_marker[i].y;
		double d = (double)(p.x-mx)*(double)(p.x-mx)+(double)(p.y-my)*(double)(p.y-my);
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
	iV2 cam(m_marker[idx].x, m_marker[idx].y);
	int camz = m_marker[idx].z;
	iV2 cam_sz = view.Size() / camz / 2;
	return iRect(cam_sz, mapSize - cam_sz).IsInside(cam);
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
			cam = m_brush[i]->pos;
			Refresh();
			m_selectgoto = i;
			return;
		}
		if(i==m_selectgoto) return;
	}
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// OTHERS
//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiMap::DrawGrid( const iRect & vw ) const
{
	int i;
	iRect view2;
	R9_SetBlend(Blend::Alpha);

	// TILE GRID
	int grid = Editor::app->m_gridsize;
	if( Editor::app->m_grid && grid!=0 ) 
	{
		// snap view
		view2 = vw; 
		view2.p1 = view2.p1 / grid * grid;
		view2.p2 = view2.p2 / grid * grid;
		
		// vertical
		iV2 cp1 = camP1();
		for( i=view2.p1.x; i<=view2.p2.x; i+=grid )
		{
			int x = camScale * (i - cp1.x);
			R9_DrawLine(fV2(x, 0), fV2(x, view.Height()), Editor::app->GetColor(EDI_COLORGRID1));
		}

		// horizontal
		for( i=view2.p1.y; i<=view2.p2.y; i+=grid )
		{
			int y = camScale * (i - cp1.y);
			R9_DrawLine(fV2(0, y), fV2(view.Width(), y), Editor::app->GetColor(EDI_COLORGRID1));
		}
	}

	// ROOM GRID
	if (m_roomgrid && roomSize.x && roomSize.y)
	{
		// snap view
		view2 = vw; 
		view2.p1 = view2.p1 / roomSize * roomSize;
		view2.p2 = view2.p2 / roomSize * roomSize;
		
		// vertical
		iV2 cp1 = camP1();
		for (i = view2.p1.x; i <= view2.p2.x; i += roomSize.x)
		{
			int x = camScale * (i - cp1.x);
			R9_DrawLine( fV2(x,0), fV2(x,view.Height()), Editor::app->GetColor(EDI_COLORGRID2) );
		}

		// horizontal
		for (i = view2.p1.y; i <= view2.p2.y; i += roomSize.y)
		{
			int y = camScale * (i - cp1.y);
			R9_DrawLine( fV2(0,y), fV2(view.Width(),y), Editor::app->GetColor(EDI_COLORGRID2));
		}
	}

}

void cEdiMap::DrawAxes( int x, int y )
{
	if(!Editor::app->m_axes) return;
	iV2 p = cam2view(iV2(x,y));
	R9_DrawLine( fV2(p.x,view.p1.y), fV2(p.x,view.p2.y), Editor::app->GetColor(EDI_COLORGRID3) );
	R9_DrawLine(fV2(view.p1.x, p.y), fV2(view.p2.x, p.y), Editor::app->GetColor(EDI_COLORGRID3));
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
	int w = view.Width();
	float x1 = (float)(cam.x - (w / camScale) / 2) / mapSize.x * w;
	float x2 = (float)(cam.x + (w / camScale) / 2) / mapSize.x * w;
	iRect rc(view.p1.x + x1, view.p2.y + 2.0f, view.p1.x + x2, static_cast<float>(view.p2.y + viewBorder));
	return rc;
}

iRect cEdiMap::GetVScrollRect()
{
	int h = view.Height();
	float y1 = (float)(cam.y - (h / camScale) / 2) / mapSize.y * h;
	float y2 = (float)(cam.y + (h / camScale) / 2) / mapSize.y * h;
	iRect rc(view.p2.x + 2.0f, view.p1.y + y1, static_cast<float>(view.p2.x + viewBorder), view.p1.y + y2);
	return rc;
}

void cEdiMap::CheckMapView()
{
	view = iRect(0, iV2(R9_GetWidth(), R9_GetHeight())).Inflate(viewBorder);
	view.p1.y += 32;
	view.p2.y -= 16;
	iV2 sz = view.Size();
	if (sz.x > mapSize.x) sz.x = mapSize.x;
	if (sz.y > mapSize.y) sz.y = mapSize.y;
	view.Inflate((view.Size() - sz) / 2);
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
	imghuge.m_width = mapSize.x;
	imghuge.m_height = mapSize.y;
	if(!R9_ImgCreate(&imghuge)) return false;

	// LOOP
	iV2 sz(256);
	iV2 p;
	while (p.y < mapSize.y)
	{
		p.x = 0;
		while (p.x < mapSize.x)
		{
			// draw in render target
			if(R9_BeginScene(m_target))
			{
				R9_Clear(Editor::app->GetColor(EDI_COLORMAP));

				// DRAW
				int _camScale = camScale;
				iV2 _cam = cam;
				iRect _view = view;
				camScale = 1;
				cam = p + 128;
				view.p2 = view.p1 + sz;
				iRect vw(p, p + sz); // camera view in map
				BrushDrawExtra( vw );
				DrawGrid( vw );
				camScale = _camScale;
				cam = _cam;
				view = _view;

				// END DRAW
				R9_Flush();
				R9_EndScene();
			}

			fRect rect(p, p + sz);
			R9_CopyTargetToImage(m_target,&imghuge,&rect);

			p.x+=sz.x;
		}

		p.y+=sz.y;
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
				size += file->Read(&mapSize.x, 4);
				size += file->Read(&mapSize.y, 4);
				size += file->Read(&g_map.roomSize.x, 4);
				size += file->Read(&g_map.roomSize.y, 4);
				size += file->Read(&g_map.cam.x, 4);
				size += file->Read(&g_map.cam.y, 4);
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
					tBrush * b = g_map.BrushNew();
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
						else if (j == BRUSH_ID) { if (val) { std::ostringstream o; o << val; b->id = o.str(); } }
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
	elog::app() << "  map=" << roomSize.x << "x" << roomSize.y << ", room=" << g_map.roomSize.x << "x" << g_map.roomSize.y << ", brushes=" << count_brush << ", objects=" << count_obj << std::endl;

	g_map.Resize(mapSize); 
	return true;
}
