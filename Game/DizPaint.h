//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPaint.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZPAINT_H__
#define __DIZPAINT_H__

#include "R9Font.h"
#include "DizDef.h"

#include <deque> 
#include <algorithm>

//////////////////////////////////////////////////////////////////////////////////////////////////
// TILE
//////////////////////////////////////////////////////////////////////////////////////////////////


class cTile
{
	cTile(const cTile &);
	cTile & operator = (const cTile &);
public:
		int			id;						// unique id >=0
		int			group;					// resource group
		int			frames;					// number of frames
		int         fx;
		int			fy;
		R9TEXTURE	tex;						// texture
		r9Img		img;						// img alpha mask
		std::string	name;						// tile name
		
		cTile(int id) : id(id), group(), frames(1), fx(1), fy(1), tex(nullptr) { }
		cTile(cTile && t) : 
			id(t.id),
			group(t.group),
			frames(t.frames),
			fx(t.fx),
			fy(t.fy),
			tex(t.tex),
			img(std::move(t.img)),
			name(std::move(t.name))
		{
			t.tex = 0; 
		}

		cTile & operator = (cTile && t) 
		{
			id = t.id;
			group = t.group;
			frames = t.frames;
			fx = t.fx;
			fy = t.fy;
			tex = t.tex;
			name = std::move(t.name);
			img = std::move(t.img);
			t.tex = 0;
			return *this;
		}
		~cTile() { Destroy();  R9_ImgDestroy(&img); }
		void Destroy() { if(tex) { R9_TextureDestroy(tex); tex = 0; } }

		int         GetFx(int frame) const		{ return frame % fx; }
		int         GetFy(int frame) const		{ return frame / fx; }
		iV2			GetF(int frame) const		{ return iV2(GetFx(frame), GetFy(frame)); }
		int			GetWidth() const			{ return fx > 0 ? tex->width / fx : tex->width; }
		int			GetHeight()	const			{ return fy > 0 ? tex->height / fy : tex->height; }
		iV2			GetSize() const { return iV2(GetWidth(), GetHeight()); }
		iV2			TexSize() const { return tex->size(); }
		fRect		FrameRect(int frame, const iRect & map) const { fRect src = map; return src.Offset(GetF(frame) * GetSize()); }
		fRect		FrameRect(int frame) const { iV2 sz = GetSize(); iV2 p = GetF(frame) * sz; return fRect(p, p + sz); }

		int ComputeFrameOnce(int frame) const { return std::max(std::min(frame, frames - 1), 0); }
		int ComputeFrameLoop(int frame) const { return frames > 0 ? frame % frames : 0; }
		int ComputeFrame( int frame, int anim ) const
		{
			if( anim==1 ) return ComputeFrameOnce(frame);
			if( anim==2 ) return ComputeFrameLoop(frame);
			return frame;
		}


};

//////////////////////////////////////////////////////////////////////////////////////////////////
// FONT
//////////////////////////////////////////////////////////////////////////////////////////////////
class cFont
{
	cFont(const cFont &);
	cFont & operator = (const cFont &);
public:
	int id;						// unique id >=0
	int group;					// resource group
	r9Font * font;						// font
		
	cFont() : id(), group(), font() {}
	cFont(int id, int group, r9Font * font) : id(id), group(group), font(font) {}
	~cFont() { delete font; }
	cFont(cFont && f) : id(f.id), group(f.group), font(f.font) { f.font = 0; }
	cFont & operator = (cFont && f) { id = f.id; group = f.group; font = f.font; f.font = 0; return *this; }

	int GetSize() const { return (int)font->GetSize(); }
	int GetCharWidth( char c ) const { return (int)font->GetCharWidth(c); }
	int GetTextWidth( const char* text ) const { return (int)font->GetTextWidth(text); }
	int	GetOfsX() const { return (int)font->GetOfsX(); }
	int GetOfsY() const { return (int)font->GetOfsY(); }
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// BRUSH OBJECTS
//////////////////////////////////////////////////////////////////////////////////////////////////
#define BRUSH_TILE		5			// tile id
#define BRUSH_FRAME		6			// current tile animation frame (starts with 0)
#define BRUSH_MAP		7			// 4 values: x1,y1,x2,y2
#define BRUSH_FLIP		11			// flip: 0=none, 1=x, 2=y, 3=xy
#define BRUSH_COLOR		12			// color
#define BRUSH_SHADER	13			// shader 0..SHADER_MAX
#define BRUSH_SCALE		14			// scale value (0=1, 1, 2, ...)

#define BRUSH_TYPE		16			// type: 0=static(for map drawing), 1=dynamic(for objects)
#define BRUSH_ID		17			// id used to access from script (brush + obj)
#define BRUSH_MATERIAL	18			// material that brushes will write in material map if draw set correctly (brush only)
#define BRUSH_DRAW		19			// draw mode: 0=don't draw, 1=draw in view, 2=write material in material map (brush only), 3=both (brush only)
#define BRUSH_DISABLE	20			// 0=enable, 1=disabled (don't draw, don't update)      
#define BRUSH_DELAY		21			// frame delay ( should be updated only once in BRUSH_DELAY frames ) (brush + obj)
#define BRUSH_ANIM		22			// animation mode: 0=none, 1=play once, 2=play loop 
#define BRUSH_COLLIDER	23			// colider mode: 0=none, 1=call handler, 2=hard collision, 3=both
#define BRUSH_CLASS		24			// generic object class (pickable, action, hurt and others)
#define BRUSH_STATUS	25			// generic object status (used to select different behaviours)
#define BRUSH_TARGET	26			// generic object target id (used to link objects)
#define BRUSH_DEATH		27			// death cause, set in player P_DEATH for hurt and kill objects
#define BRUSH_COLLISION	31			// if collision with player 1, else 0 (updated per frame)

#define BRUSH_USER		32			// more user defined free values
#define BRUSH_MAX		48			// dummy

struct tBrush
{
	int		m_data[BRUSH_MAX];
	PlAtom _id;
	Blend _shader;
	int _layer;		// layer idx
	iV2 _pos;		//  position in world
	iV2 _size;
public:
	tBrush() : _id("0"), _shader(Blend::Opaque), _layer() { 	memset(m_data, 0, sizeof(m_data)); Set(BRUSH_TILE, -1); Set(BRUSH_COLOR, 0xffffffff); }
	PlAtom id() const { return _id;}
	void id(const PlAtom &id) { _id = id; }
	int Get( int idx ) const 
	{ 
		if(idx == 0) return _layer;
		if(idx == 1) return _pos.x;
		if(idx == 2) return _pos.y;
		if(idx == 3) return _size.x;
		if(idx == 4) return _size.y;

		if (idx == BRUSH_SHADER) return static_cast<int>(shader()); return m_data[idx]; }
	void Set( int idx, int val ) 
	{ 
		if(idx == 0)
			_layer = val;
		else if(idx == 1)
			_pos.x = val;
		else if(idx == 2)
			_pos.y = val;
		else if(idx == 3)
			_size.x = val;
		else if(idx == 4)
			_size.y = val;
		m_data[idx] = val; if(idx == BRUSH_SHADER) shader(static_cast<Blend>(val)); 
	}
	float mapScale() const { return Get(BRUSH_SCALE) > 0 ? Get(BRUSH_SCALE) / 100.0f : 1.0f; }		
	float mapWith() const   { return ( Is<Flip::R>(Get(BRUSH_FLIP)) ? (Get(BRUSH_MAP+3) - Get(BRUSH_MAP+1)) : (Get(BRUSH_MAP+2) - Get(BRUSH_MAP+0)) ) * mapScale(); }
	float mapHeight() const { return ( Is<Flip::R>(Get(BRUSH_FLIP)) ? (Get(BRUSH_MAP+2) - Get(BRUSH_MAP+0)) : (Get(BRUSH_MAP+3) - Get(BRUSH_MAP+1)) ) * mapScale(); }
	static bool InvalidProp(int idx) { return idx < 0 || idx >= BRUSH_MAX; }	

	iV2 pos() const { return _pos; }
	iV2 size() const { return _size; }
	iRect rect() const { iV2 p = pos(); return iRect(p, p + size()); }
	iRect map() const { return iRect(Get(BRUSH_MAP+0), Get(BRUSH_MAP+1), Get(BRUSH_MAP+2), Get(BRUSH_MAP+3)); }
	fV2 mapSize() const { fV2 sz = map().Size(); return (Is<Flip::R>(Get(BRUSH_FLIP)) ? fV2(sz.y, sz.x) : sz) * mapScale(); }

	Blend shader() const { return _shader; }
	void shader(Blend s) { _shader = s; }

	int layer() const { return _layer; }
	int x() const { return _pos.x; }
	int y() const { return _pos.y; }

};

//////////////////////////////////////////////////////////////////////////////////////////////////
// DIZPAINT
//////////////////////////////////////////////////////////////////////////////////////////////////

template<class T>
class Indexed : std::vector<T>
{
public:
	typedef std::vector<T> Cont;
	IntIndex Index;
	using Cont::size;
	using Cont::begin;
	using Cont::end;

	bool InvalidIdx(int idx) const {return idx < 0 && static_cast<size_type>(idx) >= size(); }
	T & get(int idx) { return (*this)[idx]; }
	const T & get(int idx) const{ return (*this)[idx]; }
	T * Get(int idx) { return InvalidIdx(idx) ? nullptr : & get(idx); }
	const T * Get(int idx) const { return InvalidIdx(idx) ? nullptr : & get(idx); }
	int	New() { push_back(T()); return size() - 1; }

	void clear() { Index.clear(); Cont::clear(); }
	void erase(iterator i) { Index.erase(i->id); Cont::erase(i); }
	
	int Find(int id) const { auto i = Index.find(id); return i != Index.end() ? i->second : -1; }
	int Add(int id, T && t)
	{
		if(Find(id)!=-1) return -1; // duplicate id
		push_back(std::move(t));
		int idx = size() - 1;

		Index.insert(IntIndex::value_type(id, idx));
		return idx;
	}
	void Erase( int idx )
	{
		if(!InvalidIdx(idx))
			erase(begin() + idx);
	}

};

class Tiles : public Indexed<cTile>
{
	bool LoadFile(const std::string & file, size_t & total, size_t & fail, size_t & duplicates, int group);	// load a tile file
public:
	bool Load(const std::string & path, int group = 0); 	// load tiles from a path, and set the specified group
	void Unload(int group=0 );						// unload load tiles (destroy) from a specified group
	bool Reacquire(); // called before render reset to reload render resources
	void Unacquire(); // called before render reset to free render resources
};

class Fonts : public Indexed<cFont>
{
	bool LoadFile(const std::string & filepath, size_t & total, size_t & fail, size_t & duplicates, int group = 0);
public:
	bool Load(const std::string & path, int group = 0);	// load fonts from a path and set the specified group
	void Unload(int group = 0);				// unload fonts (destroy) from the specified group
	void Unacquire() { std::for_each(begin(), end(), [](cFont &f) { if(f.font) f.font->SetTexture(nullptr); }); }

};

class HUD
{
	enum Cmd { None, Align, Color, Focus, Tile };
	int _font;		// current font id
	Blend _shader;	// current hud shader
	dword _color;	// current hud color
	bool _isDrawing;		// draw allowed
	Cmd ScanText(std::string::const_iterator start, std::string::const_iterator end, std::string::const_iterator & res, int* data);								// helper for hud text; scans for command and return command and data info

public:
	HUD() : _font(), _shader(Blend::Alpha), _color(0xffffffff), _isDrawing() {}
	void SetClipping(const iRect & dst);														// set a clipping rect
	void DrawTile(int tileid, const iRect & dst, const iRect & src, dword flags, int frame);	// draw tile
	void DrawText(int tileid, const iRect & dst, const std::string & text, int align);						// draw text with escape commands
	void GetTextSize(const std::string & text, int& w, int& h, int&c, int&r);								// in text's width and height in pixels and the number of columns and rows

	void font(int f) { _font = f; }
	void shader(Blend b) { _shader = b; }
	void color(dword c) { _color = c; }
	void draw(bool d) { _isDrawing = d; }

	int font() const { return _font; }
	Blend shader() const { return _shader; }
	dword color() const { return _color; }
	bool isDrawing() const { return _isDrawing; }
};

class cDizPaint
{
	r9Img _imgtarget;	// target image in PF_A8 format (pointing to material map data)
	bool _drawtilesoft;	// true for DrawBrush to call DrawTileSoft
	byte _drawtilemat;	// material to draw the tile
	int _scale;		// scale factor
	iV2 _scrOffs;			// screen offset

	struct
	{
		int scale;
		iV2 offs;
		fRect clip;
	} rollback;

	void DrawTileSoft(int idx, const iV2 & p, const iRect & map, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f ) const;	// paints tile in the image target map (faster, no rotation, no scale)
	void DrawTileSoft2(int idx, const iV2 & p, const iRect & map, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f ) const;	// paints tile in the image target map (accept rotation and scale)

	std::function<void(const iV2&)> selectDrawMethod(const tBrush & brush, int idx, int frame) const;

public:
	cDizPaint() : _scale(1), _drawtilesoft(), _drawtilemat() {}
	
	bool Init()	{ Layout(); return true; }
	void Done() { tiles.clear(); fonts.clear(); }
	void Layout();								// compute layout position (scale,scrx,scry)
		
	bool Reacquire() { return tiles.Reacquire(); }	// called before render reset to reload render resources
	void Unacquire();								// called before render reset to free render resources

	// Draw scaled
	void DrawTile(int idx, const iV2 & p, const iRect & map, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f ) const;	// tile scale (in editor paint it was full scale)
	void DrawTile(int idx, const iV2 & p, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f) const;				// tile scale (in editor paint it was full scale)
	void DrawChar(int fontidx, const iV2 & p, char c, dword color=0xffffffff ) const;

	// Draw brush 
	void DrawBrush( const tBrush & brush, const iV2 & p, int frame=-1 ) const; // if frame is -1, tile is automatic animated

	void BeginSoftwareRendering(const iV2 & size, dword cap, byte * data);
	void EndSoftwareRendering();


	// screen props
	iV2 scrPos(const iV2 & p) const { return scrOffs() + p * scale(); }
	int scale() const { return _scale; }
	void scale(int s) { _scale = s; }

	const iV2 & scrOffs() const { return _scrOffs; }
	void scrOffs(const iV2 & v) { _scrOffs = v; }

	void drawtilemat(int mat) { _drawtilemat = mat; }
	int drawtilemat() const { return _drawtilemat; }

	bool drawtilesoft() const { return _drawtilesoft; }

	Tiles tiles;
	Fonts fonts;
	HUD hud;
};

extern	cDizPaint	g_paint;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
