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

class Brushes;
typedef std::vector<Brush *> BrushList;

class Partitions
{
	static const int CellSize = 1024;
	class Cell : BrushList
	{
	public:
		const iRect rect;
		using BrushList::begin;
		using BrushList::end;
		using BrushList::clear;

		Cell(const iRect & rect) : rect(rect) {}
		void Add(Brush *b) { push_back(b); }
		void Del(Brush * b) { auto it = std::find(begin(), end(), b); if (it != end()) erase(it); }
		bool Find(Brush * b) { return std::find(begin(), end(), b) != end(); }
	};
	typedef std::vector<Cell *> Cont;
	Cont cells;

	iV2 size;
	void Get(const iRect & rect, Cont & tmp, int maxsize) const;
public:
	void Init(const iV2 & mapSize);
	void Done();
	bool Add(Brush * b);
	void Del(Brush * b);
	bool Repartition(const Brushes & brushes);
	void Filter(const iRect & view, BrushList & vis) const;
};

class PlBrush
{
	Brush * b;
public:
	PlBrush(Brush * b) : b(b) {}
	PlBrush(PlTerm t) { if (!(t = Functor())) throw PlTypeError("brush", t); b = Cast(t[1]); }
	static PlFunctor Functor() { static PlFunctor brush("brush", 1); return brush; }
	static Brush * Cast(PlTerm t) { return reinterpret_cast<Brush *>(static_cast<void *>(t)); }

	bool operator = (PlTerm t) { if (!(t = Functor())) return false; return t[1] = b; }
	operator Brush *() { return b; }
	Brush * operator ->() { return b; }
};

class Brushes : BrushList
{
	int	Selectgoto;
	template<class It>
	void To(It begin, It end, Brush * b);
public:
	int SelectCount;

	using BrushList::begin;
	using BrushList::end;
	using BrushList::back;
	using BrushList::size;
	using BrushList::operator[];

	Brushes();
	~Brushes() { Clear(); }
	void Clear();

	Brush * New();
	void Del(Brush * b);
	Brush * Pick(const iV2 & p) const;
	void ToFront(Brush * b) { To(rbegin(), rend(), b); }
	void ToBack(Brush * b) { To(begin(), end(), b); }
						
	void SelectionRefresh();
	void SelectionGoto(int dir);
};

template<class It>
void Brushes::To(It begin, It end, Brush * b)
{
	It cur = std::find(begin, end, b);
	if (cur == end)
		return;
	int layer = b->layer;
	It front = std::find_if(begin, cur, [layer](Brush * b) { return b->layer == layer; });
	if (front == cur)
		return;
	It to = cur++;
	std::copy_backward(front, to, cur);
	*front = b;
	g_map.partitions.Repartition(g_map.brushes);
	g_map.Refresh();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiMap
//////////////////////////////////////////////////////////////////////////////////////////////////

class cEdiMap
{
	bool refresh;
	void Render();
	iRect GetHScrollRect() const;
	iRect GetVScrollRect() const;
public:
	cEdiMap();

	bool Init();
	void Done();

	void Update(float dtime);
	void Draw();
							
	void Reset();
	bool Resize(const iV2 & sz);	// resize map; return true if no crop occured

	void Refresh() { refresh = true; }

	// map
	iV2 mapSize;
	iV2 roomSize;
	int m_roomgrid;												// room grid visible
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
	bool m_hideselected;									// don't draw selected tiles

	R9TEXTURE m_target;										// render target texture

	BrushList brushvis;
	Brushes brushes;
	Partitions partitions;

	void BrushDrawExtra(const iRect & view);			// draw brushes in view using partitioning
	// others
	void DrawGrid(const iRect & vw) const;				// draw grid using EdiApp settings
	void DrawAxes(int x, int y);						// draw axes
	void DrawScrollers();								// draw side scrollers

	void CheckMapView();									// checks viewport if exceedes map sizes and adjust
	int	m_scrolling;									// 0=no, 1=horizontal, 2=vertical
	int	m_scrollofs;									// scroll pick offset (h or v)

	// save map image
	bool SaveMapImage(const std::string & filename);					// save huge map image, rendering in the target texture each 256x256 cel
	bool Load(const std::string & filename);
	bool LoadMap(const std::string & filename);
};

extern	cEdiMap		g_map;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
