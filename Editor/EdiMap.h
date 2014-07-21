//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiMap.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __EDIMAP_H__
#define __EDIMAP_H__

#include "EdiDef.h"
#include "EdiPaint.h"

#include "SWI-cpp-m.h"

#define	MAP_SIZEDEFAULT	2000
#define MAP_SIZEMIN		128
#define MAP_SIZEMAX		100000

#define MAP_ID							"dizzymap"
#define MAP_CHUNKID						0x11111111
#define MAP_CHUNKINFO					0x22222222	// obsolete
#define MAP_CHUNKINFO2					0x22222223
#define MAP_CHUNKMARKERS				0x33333333	// obsolete
#define MAP_CHUNKMARKERS2				0x33333334
#define MAP_CHUNKBRUSHES				0x88888888	// obsolete
#define MAP_CHUNKBRUSHES2				0x88888889

typedef std::vector<tBrush *> Brushes;

class Partitions
{
	static const int CellSize = 1024;
	class Cell : std::vector<tBrush *>
	{
		typedef std::vector<tBrush *> Cont;
	public:
		const iRect rect;
		using Cont::begin;
		using Cont::end;
		using Cont::clear;

		Cell(const iRect & rect) : rect(rect) {}
		void Add(tBrush *b) { push_back(b); }
		void Del(tBrush * b) { auto it = std::find(begin(), end(), b); if (it != end()) erase(it); }
		bool Find(tBrush * b) { return std::find(begin(), end(), b) != end(); }
	};
	typedef std::vector<Cell *> Cont;
	Cont cells;

	iV2 size;
	void Get(const iRect & rect, Cont & tmp, int maxsize) const;
public:
	void Init(const iV2 & mapSize);
	void Done();
	bool Add(tBrush * b);
	void Del(tBrush * b);
	bool Repartition(const Brushes & brushes);
	void Filter(const iRect & view, Brushes & vis) const;
};

struct tMarker
{
	int x;	// marker pos x
	int y;	// marker pos y
	int z;	// marker zoom
	tMarker( int _x, int _y, int _z ) { x=_x; y=_y; z=_z; }
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiMap
//////////////////////////////////////////////////////////////////////////////////////////////////

class cEdiMap
{
public:
	cEdiMap();

	bool Init();
	void Done();

	void Update(float dtime);			// update
	void Draw();							// draw current
	void Refresh();							// refresh draw

	void Reset();
	bool Resize(const iV2 & sz);	// resize map; return true if no crop occured

	PlFunctor brush;
	bool UnifyBrush(PlTerm t, tBrush * b);
	tBrush * brushPtrNoEx(PlTerm t) { return reinterpret_cast<tBrush *>(static_cast<void *>(t)); }
	tBrush * brushPtr(PlTerm t) { if (!(t = brush)) throw PlTypeError("brush", t); return brushPtrNoEx(t[1]); }

	// map
	iV2 mapSize;
	iV2 roomSize;
	int m_roomgrid;										// room grid visible
	iRect view;													// viewport
	static const int viewBorder = 16;
	iV2 cam;													// camera position
	int camScale;												// Camera scale
	iV2 camSize() const { return view.Size() / camScale; }
	iRect camRect() const  { return iRect(cam, cam).Deflate(camSize() / 2); }
	iV2 camP1() const { return cam - camSize() / 2; }
	iV2 view2cam(const iV2 & p) const { return (p - view.p1) / camScale + camP1(); }   // from screen space to world space
	iV2 cam2view(const iV2 & p) const { return camScale * (p - camP1()) + view.p1; } // from world space to screen space

	// refresh			
	BOOL m_hideselected;									// don't draw selected tiles
	BOOL m_refresh;										// if refresh is necessary
	R9TEXTURE m_target;										// render target texture

	// brushes
	int BrushNew();							// add a new brush to the brushlist
	void BrushIns(int idx, tBrush& brush);	// insert a new brush and shift others; selectcount friendly
	void BrushDel(tBrush * b);				// delete one brush from brushlist and shift others; selectcount friendly
	void BrushDrawExtra(const iRect & view);			// draw brushes in view using partitioning
	tBrush * BrushPick(const iV2 & p) const;
	void BrushToFront(tBrush * b) { BrushTo(m_brush.rbegin(), m_brush.rend(), b); }				// bring brush to front (first visible in layer)
	void BrushToBack(tBrush * b) { BrushTo(m_brush.begin(), m_brush.end(), b); } 				// bring brush to back (last visible in layer)
	void BrushClear();							// free brush buffers and counts; selectcount friendly

	template<class It>
	void BrushTo(It begin, It end, tBrush * b);

	void TakeBrush(tBrush * b);
	bool validBrushIdx(int idx) const { return static_cast<size_t>(idx) < m_brush.size(); }
	Brushes m_brush;							// brush buffer list (brushes in map)
	Brushes brushvis;							// visible brushes list (brushes to draw; updated on refresh)

	Partitions partitions;

	// markers
	void MarkerToggle(const iV2 &p);				// add/remove marker at a given pos
	void MarkerGoto(int dir = 1);				// go to nearest/next marker
	int MarkerClosest(const iV2 &p, int &dist); // get index of closest marker to a given pos 
	void MarkerClear();							// clear all markers
	void MarkerResize();							// remove out markers after map is resized (private)
	BOOL MarkerTest(int idx);				// test if a marker is inside map (private)
	std::vector<tMarker> m_marker;								// markers list

	// selection
	void SelectionRefresh();							// recount selection
	void SelectionGoto(int dir = 1);				// go to next selected brush
	int m_selectcount;									// count of selected brushes
	int	m_selectgoto;									// index of brush used in goto selection

	// others
	void DrawGrid(const iRect & vw) const;				// draw grid using EdiApp settings
	void DrawAxes(int x, int y);						// draw axes
	void DrawScrollers();								// draw side scrollers
	iRect GetHScrollRect();
	iRect GetVScrollRect();
	void CheckMapView();									// checks viewport if exceedes map sizes and adjust
	int	m_scrolling;									// 0=no, 1=horizontal, 1=vertical
	int	m_scrollofs;									// scroll pick offset (h or v)

	// save map image
	bool SaveMapImage(const std::string & filename);					// save huge map image, rendering in the target texture each 256x256 cel
	bool Load(const std::string & filename);
	bool LoadMap(const std::string & filename);
};

template<class It>
void cEdiMap::BrushTo(It begin, It end, tBrush * b)
{
	It cur = std::find(begin,end, b);
	if(cur == end)
		return;
	int layer = b->layer;
	It front = std::find_if(begin, cur, [layer](tBrush * b) { return b->layer==layer; });
	if(front == cur)
		return;
	It to = cur++;
	std::copy_backward(front, to, cur);
	*front = b;
	partitions.Repartition(g_map.m_brush);
}

extern	cEdiMap		g_map;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
